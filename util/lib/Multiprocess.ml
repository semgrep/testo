(*
   Manage the lifecycle of worker processes used for parallel test runs.

   master process: client
   worker processes: servers
*)

open Printf

module Client = struct
  type worker = {
    id : int; (* unique ID = position in the worker array *)
    name : string; (* unique name for display purposes *)
    (* The process field is the argument to be passed to
       'Unix.close_process'. *)
    process : in_channel * out_channel;
    (* Read the worker's stdout *)
    std_in_ch : in_channel;
    std_in_fd : Unix.file_descr;
    (* Write to the worker's stdin *)
    std_out_ch : out_channel;
    std_input_buffer : Buffer.t;
    mutable running : bool; (* = an END message has not been received *)
  }

  type t = {
    array : worker array;
    read : t -> (worker * Msg_from_worker.t) option;
    (* Create or replace a worker to be placed at position i
       in the array of workers. *)
    create_worker : int -> worker;
  }

  let iter_workers workers func = Array.iter func workers.array

  let fds_of_running_workers (workers : t) =
    Array.fold_right
      (fun x acc -> if x.running then x.std_in_fd :: acc else acc)
      workers.array []

  (* For debugging *)
  let show_process_status (x : Unix.process_status) =
    match x with
    | WEXITED n -> sprintf "WEXITED %i" n
    | WSIGNALED n -> sprintf "WSIGNALED %i" n
    | WSTOPPED n -> sprintf " %i" n

  let close_worker (x : worker) =
    if x.running then (
      (* Assume the worker process still exists, hasn't been waitpidded. *)
      x.running <- false;
      Debug.log (fun () -> sprintf "close worker %s" x.name);
      let t1 = Unix.gettimeofday () in
      let pid = Unix.process_pid x.process in
      (* Fun fact/bug (Linux, OCaml 5.3.0): if we don't send a signal
         to terminate the worker process, Unix.close_process will
         return immediately even though the child process keeps
         running until natural termination. Writes to stdout or
         stderr remain possible but only for a short while. Then the
         write calls will return without physically writing anything
         to console or to files. Then the process will block until the
         child process terminates naturally. Only then the expected
         physical writes will take place and our process will resume
         normally.

         If you want to debug this, do the following:
         1. Remove the Unix.kill call.
         2. Run 'make test' to recompile.
         3. Run './timeout-test --debug'. It will hang until the test
            running in the worker is finished (at which point the worker
            would try to write something on the pipe connected to our
            master process) but it will output something like this:

[DEBUG] [1757469030.653667] close worker 2/16
<5-second pause>
[DEBUG] [1757469030.655928] closed worker 2/16: WEXITED 0 (took 0.002222s)

            -> The second DEBUG line shows up on the console after a
               5-second delay as expected but reports 0.002 s instead
               of 5 seconds!

         I haven't managed to reproduce the bug with the following repro.ml:

         (*
            Program that attempts to reproduce the bug but works correctly

            Build command: ocamlopt -o repro -I +unix unix.cmxa repro.ml
            Run: ./repro
            Expectation and actual behavior: reports about 1 second elapsed
            Bug expectation: reports under a millisecond
         *)
         let () =
           let process = Unix.open_process "sleep 1" in
           let t1 = Unix.gettimeofday () in
           let _status = Unix.close_process process in
           let t2 = Unix.gettimeofday () in
           Printf.printf "elapsed: %.6f\n%!" (t2 -. t1)
      *)
      Unix.kill pid Sys.sigkill;
      let status = Unix.close_process x.process in
      let t2 = Unix.gettimeofday () in
      Debug.log (fun () ->
          sprintf "closed worker %s: %s (took %.6fs)" x.name
            (show_process_status status)
            (t2 -. t1)))

  (* This should be used only during final cleanup operations because
     it doesn't guarantee that the worker actually gets closed, potentially
     hiding problems. We don't want to hide problems so it's better to
     use the regular 'close_worker' if we're not about to exit. *)
  let safe_close_worker (x : worker) =
    try close_worker x with
    | exn ->
        eprintf "Failed to close worker %s: %s\n%!" x.name
          (Printexc.to_string exn)

  let safe_close (workers : t) = Array.iter safe_close_worker workers.array

  (* Used when a worker times out *)
  let replace_worker workers worker =
    close_worker worker;
    let new_worker = workers.create_worker worker.id in
    workers.array.(worker.id) <- new_worker;
    new_worker

  let take_lines_from_buffer buf =
    let lines = Buffer.contents buf |> String.split_on_char '\n' in
    let complete_lines, leftover =
      match lines |> List.rev with
      | leftover :: complete -> (List.rev complete, leftover)
      | [] -> assert false
    in
    Buffer.clear buf;
    Buffer.add_string buf leftover;
    complete_lines

  let flush_input_buffer worker buf queue =
    let lines =
      (* Ignore empty lines that may have been added as a way to protect
         messages against junk. See 'write'. *)
      take_lines_from_buffer buf
      |> List.filter (function
           | "" -> false
           | _ -> true)
    in
    List.iter
      (fun line -> Queue.add (worker, Msg_from_worker.of_string line) queue)
      lines

  (*
     Read a message fragment from a worker after we were notified
     that some input is available. If it contains the end of
     a message, the message is added to the queue. In general,
     input fragments don't align with message boundaries. An input fragment
     can contain multiple whole or partial messages.
  *)
  let read_from_worker (x : worker)
      (incoming_message_queue : (worker * Msg_from_worker.t) Queue.t) =
    (* Read everything that's available to read now without blocking *)
    let buf_len = 1024 in
    let buf = Bytes.create buf_len in
    let bytes_read = input x.std_in_ch buf 0 buf_len in
    if bytes_read = 0 then (
      (* end of input *)
      Debug.log (fun () ->
          sprintf "received 0 bytes from worker %s, closing" x.name);
      close_worker x)
    else (
      (* Add bytes to the worker's input buffer and see if we have
         complete messages that we can parse and add to the message
         queue. *)
      Debug.log (fun () ->
          sprintf "received %i bytes from worker %s" bytes_read x.name);
      Buffer.add_subbytes x.std_input_buffer buf 0 bytes_read;
      flush_input_buffer x x.std_input_buffer incoming_message_queue)

  let kill_and_replace_timed_out_workers ~feed_or_terminate_worker
      ~get_timed_out_workers workers =
    let timed_out_workers : (worker * (unit -> unit)) list =
      get_timed_out_workers ()
    in
    List.iter
      (fun (worker, on_worker_termination) ->
        let new_worker = replace_worker workers worker in
        on_worker_termination ();
        (* As soon as a worker is created, it must be fed with a test
         otherwise the poller will wait forever for input from the
         worker. Passing around the feed_or_terminate_worker function isn't
         elegant. There may be a better way. *)
        feed_or_terminate_worker new_worker)
      timed_out_workers

  (* This table maps file descriptors to worker data.
     This is needed to recover the worker info from the file descriptor
     returned by Unix.select.
     Each time a worker is created, this table must be updated. *)
  let create_fd_worker_table () = Hashtbl.create 100

  let make_poller ?(max_poll_interval_secs = 0.1) ~feed_or_terminate_worker
      ~fd_worker_tbl ~get_timed_out_workers () =
    let incoming_message_queue = Queue.create () in
    let rec poll (workers : t) =
      (* If a complete message was already read and is available from the
         queue, return it. Otherwise, read the next available fragment
         of incoming message from any worker. *)
      match Queue.take_opt incoming_message_queue with
      | Some msg -> Some msg
      | None -> (
          kill_and_replace_timed_out_workers ~feed_or_terminate_worker
            ~get_timed_out_workers workers;
          match fds_of_running_workers workers with
          | [] -> None
          | in_fds -> (
              (* Wait for data being available to read from one of the
                 workers. *)
              Debug.log (fun () ->
                  sprintf "wait for a message from one of %i worker(s)"
                    (List.length in_fds));
              match Unix.select in_fds [] [] max_poll_interval_secs with
              | in_fd :: _, _, _ ->
                  let worker =
                    match Hashtbl.find_opt fd_worker_tbl in_fd with
                    | None -> assert false
                    | Some x -> x
                  in
                  Debug.log (fun () ->
                      sprintf "receiving data from worker %s" worker.name);
                  (* Read the available data, resulting in 0, 1, or more
                     messages being added to the queue. *)
                  read_from_worker worker incoming_message_queue;
                  poll workers
              | [], _, _ ->
                  if max_poll_interval_secs >= 0. then
                    (* All the workers have been busy for more than
                       max_poll_interval_secs which is fine.
                       max_poll_interval_secs exists only to give us
                       a chance to check for timed out workers.
                       Keep polling. *)
                    poll workers
                  else (
                    (* Error.
                       max_poll_interval_secs is negative, preventing
                       Unix.select from timing out. *)
                    Debug.log (fun () ->
                        "'select' returned nothing. Did a worker exit \
                         prematurely?");
                    safe_close workers;
                    None)))
    in
    poll

  let create_worker ~fd_worker_tbl ~num_workers ~original_argv
      ~test_list_checksum worker_id =
    let worker_name = sprintf "%d/%d" (worker_id + 1) num_workers in
    let program_name =
      (* This requires the caller of the current program to not have
         set argv[0] to something else than a valid command name! *)
      Sys.argv.(0)
    in
    let argv =
      Array.concat
        [
          original_argv;
          [| "--worker"; "--test-list-checksum"; test_list_checksum |];
        ]
    in
    Debug.log (fun () ->
        sprintf "create worker %s with command: %s" worker_name
          (argv |> Array.to_list |> String.concat " "));

    (* This is supposed to work on all platforms. *)
    let ((std_in_ch, std_out_ch) as process) =
      Unix.open_process_args program_name argv
    in
    let std_in_fd = Unix.descr_of_in_channel std_in_ch in
    let worker =
      {
        id = worker_id;
        name = worker_name;
        process;
        std_in_ch;
        std_in_fd;
        std_out_ch;
        std_input_buffer = Buffer.create 200;
        running = true;
      }
    in
    Hashtbl.replace fd_worker_tbl std_in_fd worker;
    worker

  (*
     Create a set of workers. Each will execute a command derived from
     the same command as the current process but with modifications
     (e.g. '--worker' is added, a '--slice' option is added, ...)
  *)
  let create ~feed_or_terminate_worker ~get_timed_out_workers ~num_workers
      ~original_argv ~test_list_checksum () =
    Debug.log (fun () -> sprintf "create %i worker(s)" num_workers);
    let fd_worker_tbl = create_fd_worker_table () in
    let create_worker =
      create_worker ~fd_worker_tbl ~num_workers ~original_argv
        ~test_list_checksum
    in
    let worker_array = Array.init num_workers create_worker in
    let read =
      make_poller ~feed_or_terminate_worker ~fd_worker_tbl
        ~get_timed_out_workers ()
    in
    { array = worker_array; read; create_worker }

  (*
     Read the next available message from a worker, returning the worker's
     identifier and the message.

     A 'None' result indicates that all the workers are done.
  *)
  let read (x : t) = x.read x

  (*
     Send a request to an available worker.
     An available worker is one that's not currently running a test as
     determined by the messages we receive from it.
  *)
  let write worker msg =
    fprintf worker.std_out_ch "%s\n%!" (Msg_from_master.to_string msg)

  let start_test ~on_start_test worker test_id test =
    on_start_test (Some worker) test;
    write worker (Start_test test_id)

  (* Feed a worker with a test from the queue or terminate the worker. *)
  let feed_worker ~get_test_id ~on_start_test test_queue worker =
    match Queue.take_opt test_queue with
    | Some test -> start_test ~on_start_test worker (get_test_id test) test
    | None -> close_worker worker

  let run_tests_in_workers ~argv ~get_test_id ~get_timed_out_workers
      ~num_workers ~on_end_test ~on_start_test ~test_list_checksum
      (tests : 'test list) =
    let test_queue = tests |> List.to_seq |> Queue.of_seq in
    let feed_or_terminate_worker worker =
      (* feed the worker if possible or terminate it *)
      feed_worker ~get_test_id ~on_start_test test_queue worker
    in
    let workers =
      create ~feed_or_terminate_worker ~get_timed_out_workers ~num_workers
        ~original_argv:argv ~test_list_checksum ()
    in
    let get_test =
      let tbl = Hashtbl.create (2 * List.length tests) in
      List.iter (fun test -> Hashtbl.add tbl (get_test_id test) test) tests;
      fun test_id ->
        try Hashtbl.find tbl test_id with
        | Not_found ->
            failwith
              (sprintf
                 "Internal error: received invalid test ID from worker: %S"
                 test_id)
    in
    (* Send a first task (test) to each worker.
       Do not reuse a worker outside of the function below as it may
       become invalid. *)
    iter_workers workers feed_or_terminate_worker;
    (* Wait for responses from workers and feed them until the queue is empty *)
    let rec loop () =
      match read workers with
      | None ->
          (* There are no more workers = they were closed after we
             tried to feed each of them from the empty queue. *)
          Ok ()
      | Some (worker, msg) -> (
          match msg with
          | End_test test_id ->
              let test = get_test test_id in
              on_end_test test;
              feed_or_terminate_worker worker;
              safe_loop ()
          | Error msg ->
              let msg = sprintf "error in worker %s: %s" worker.name msg in
              safe_close workers;
              Error msg
          | Junk line ->
              printf "[worker %s] %s\n%!" worker.name line;
              safe_loop ())
    and safe_loop () =
      try loop () with
      | exn ->
          let trace = Printexc.get_backtrace () in
          safe_close workers;
          eprintf
            "Fatal internal error in Testo's master process:\n\
             %s\n\
             %s\n\
             Please report this bug at https://github.com/semgrep/testo/issues.\n\
             %!"
            (Printexc.to_string exn) trace;
          exit Error.Exit_code.internal_error
    in
    safe_loop ()
end

module Server = struct
  let read () =
    let line =
      try Some (input_line stdin) with
      | End_of_file -> None
    in
    Option.map Msg_from_master.of_string line

  let write msg =
    (* We print a newline before the message to terminate any junk
       that may have been written to stdout by user code. *)
    printf "\n%s\n%!" (Msg_from_worker.to_string msg)

  let fatal_error str =
    write (Error str);
    (* Internal error: see exit codes in Error.ml *)
    exit 3
end
