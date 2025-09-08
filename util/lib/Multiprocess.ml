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
       'Unix.close_process_full'. *)
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
    Array.fold_right (fun x acc ->
      if x.running then x.std_in_fd :: acc else acc)
      workers.array []

  let close_worker (x : worker) =
    Debug.log (fun () -> sprintf "close worker %s" x.name);
    x.running <- false;
    match Unix.close_process x.process with
    | _status -> ()

  let close (workers : t) = Array.iter close_worker workers.array

  (* Used when a worker times out *)
  let replace_worker workers worker =
    close_worker worker;
    let new_worker = workers.create_worker worker.id in
    workers.array.(worker.id) <- new_worker

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

  let kill_and_replace_timed_out_workers ~get_timed_out_workers workers =
    let timed_out_workers : worker list = get_timed_out_workers () in
    List.iter (replace_worker workers) timed_out_workers

  (* This table maps file descriptors to worker data.
     This is needed to recover the worker info from the file descriptor
     returned by Unix.select.
     Each time a worker is created, this table must be updated. *)
  let create_fd_worker_table () =
    Hashtbl.create 100

  let make_poller
      ?(max_poll_interval_secs = 1.)
      ~fd_worker_tbl ~get_timed_out_workers () =
    let incoming_message_queue = Queue.create () in
    let rec poll (workers : t) =
      (* If a complete message was already read and is available from the
         queue, return it. Otherwise, read the next available fragment
         of incoming message from any worker. *)
      match Queue.take_opt incoming_message_queue with
      | Some msg -> Some msg
      | None ->
          kill_and_replace_timed_out_workers ~get_timed_out_workers workers;
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
                    close workers;
                    None)
            )
    in
    poll

  let create_worker
      ~fd_worker_tbl
      ~num_workers
      ~original_argv
      ~test_list_checksum
      worker_id =
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
    let worker = {
      id = worker_id;
      name = worker_name;
      process;
      std_in_ch;
      std_in_fd;
      std_out_ch;
      std_input_buffer = Buffer.create 200;
      running = true;
    } in
    Hashtbl.replace fd_worker_tbl std_in_fd worker;
    worker

  (*
     Create a set of workers. Each will execute a command derived from
     the same command as the current process but with modifications
     (e.g. '--worker' is added, a '--slice' option is added, ...)
  *)
  let create
      ~get_timed_out_workers
      ~num_workers
      ~original_argv
      ~test_list_checksum
      () =
    Debug.log (fun () -> sprintf "create %i worker(s)" num_workers);
    let fd_worker_tbl = create_fd_worker_table () in
    (* TODO: make the set of workers somehow updatable so as to support
       killing and restarting workers that time out *)
    let create_worker =
      create_worker ~fd_worker_tbl ~num_workers ~original_argv ~test_list_checksum in
    let worker_array = Array.init num_workers create_worker in
    let read = make_poller ~fd_worker_tbl ~get_timed_out_workers () in
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

  let feed_worker ~get_test_id ~on_start_test test_queue worker =
    match Queue.take_opt test_queue with
    | Some test -> start_test ~on_start_test worker (get_test_id test) test
    | None -> close_worker worker

  let run_tests_in_workers
      ~argv
      ~get_test_id
      ~get_timed_out_workers
      ~num_workers
      ~on_end_test
      ~on_start_test
      ~test_list_checksum (tests : 'test list) =
    let workers =
      create
        ~get_timed_out_workers
        ~num_workers ~original_argv:argv ~test_list_checksum ()
    in
    let get_test =
      let tbl = Hashtbl.create (2 * List.length tests) in
      List.iter (fun test -> Hashtbl.add tbl (get_test_id test) test) tests;
      fun test_id ->
        try Hashtbl.find tbl test_id with
        | Not_found ->
            failwith
              (sprintf "Internal error: received invalid test ID from worker: %S"
                 test_id)
    in
    let test_queue = tests |> List.to_seq |> Queue.of_seq in
    (* Send a first task (test) to each worker.
       Do not reuse a worker outside of the function below as it may
       become invalid. *)
    iter_workers workers (fun worker ->
      feed_worker ~get_test_id ~on_start_test test_queue worker);
    (* Wait for responses from workers and feed them until the queue is empty *)
    let rec loop () =
      match read workers with
      | None ->
          (* There are no more workers = they were closed after we
             tried to feed each of them from the empty queue. *)
          Ok ()
      | Some (worker, msg) ->
          (match msg with
           | End_test test_id ->
               let test = get_test test_id in
               on_end_test test;
               feed_worker ~get_test_id ~on_start_test test_queue worker;
               loop ()
           | Error msg ->
               let msg =
                 sprintf "error in worker %s: %s" worker.name msg
               in
               close workers;
               Error msg
           | Junk line ->
               printf "[worker %s] %s\n%!" worker.name line;
               loop ())
    in
    loop ()
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
    exit 1
end
