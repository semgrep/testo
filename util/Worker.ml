(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

open Printf

module Client = struct
  type worker = {
    id : string; (* unique ID derived from the slice *)
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
    workers : worker list;
    read : unit -> (worker * Msg_from_worker.t) option;
  }

  let worker_id x = x.id
  let iter_workers x func = List.iter func x.workers

  let fds_of_running_workers workers =
    List.filter_map
      (fun x -> if x.running then Some x.std_in_fd else None)
      workers

  let close_worker (x : worker) =
    Debug.log (fun () -> sprintf "close worker %s" x.id);
    x.running <- false;
    match Unix.close_process x.process with
    | _status -> ()

  let close_workers workers = List.iter close_worker workers
  let close x = close_workers x.workers

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

  let read_from_worker (x : worker)
      (queue : (worker * Msg_from_worker.t) Queue.t) =
    (* Read everything that's available to read now without blocking *)
    let buf_len = 1024 in
    let buf = Bytes.create buf_len in
    let bytes_read = input x.std_in_ch buf 0 buf_len in
    if bytes_read = 0 then (
      (* end of input *)
      Debug.log (fun () ->
          sprintf "received 0 bytes from worker %s, closing" x.id);
      close_worker x)
    else (
      (* Add bytes to the worker's input buffer and see if we have
         complete messages that we can parse and add to the message
         queue. *)
      Debug.log (fun () ->
          sprintf "received %i bytes from worker %s" bytes_read x.id);
      Buffer.add_subbytes x.std_input_buffer buf 0 bytes_read;
      flush_input_buffer x x.std_input_buffer queue)

  let make_poller workers =
    (* The table maps file descriptor to worker data. This is needed to
       determine when all workers are done. *)
    let worker_tbl = Hashtbl.create 100 in
    List.iter
      (fun worker -> Hashtbl.add worker_tbl worker.std_in_fd worker)
      workers;
    let incoming_message_queue = Queue.create () in
    let rec poll () =
      match Queue.take_opt incoming_message_queue with
      | Some msg -> Some msg
      | None -> (
          match fds_of_running_workers workers with
          | [] -> None
          | in_fds -> (
              (* Wait for data being available to read from one of the
                 workers. *)
              Debug.log (fun () ->
                  sprintf "wait for a message from one of %i worker(s)"
                    (List.length in_fds));
              match Unix.select in_fds [] [] (-1.) with
              | in_fd :: _, _, _ ->
                  let worker =
                    match Hashtbl.find_opt worker_tbl in_fd with
                    | None -> assert false
                    | Some x -> x
                  in
                  Debug.log (fun () ->
                      sprintf "receiving data from worker %s" worker.id);
                  (* Read the available data, resulting in 0, 1, or more
                     messages being added to the queue. *)
                  read_from_worker worker incoming_message_queue;
                  poll ()
              | [], _, _ ->
                  Debug.log (fun () ->
                      "'select' returned nothing. Did a worker exit \
                       prematurely?");
                  close_workers workers;
                  None))
    in
    poll

  let create ~num_workers ~original_argv ~test_list_checksum =
    Debug.log (fun () -> sprintf "create %i worker(s)" num_workers);
    let worker_ids =
      List.init num_workers (fun i -> sprintf "%d/%d" (i + 1) num_workers)
    in
    let workers =
      worker_ids
      |> Helpers.list_map (fun id ->
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
                 sprintf "create worker %s with command: %s" id
                   (argv |> Array.to_list |> String.concat " "));

             (* This is supposed to work on all platforms. *)
             let ((std_in_ch, std_out_ch) as process) =
               Unix.open_process_args program_name argv
             in
             {
               id;
               process;
               std_in_ch;
               std_in_fd = Unix.descr_of_in_channel std_in_ch;
               std_out_ch;
               std_input_buffer = Buffer.create 200;
               running = true;
             })
    in
    let read = make_poller workers in
    { workers; read }

  let read (x : t) = x.read ()

  let write worker msg =
    fprintf worker.std_out_ch "%s\n%!" (Msg_from_master.to_string msg)
end

module Server = struct
  let read () =
    let line =
      try Some (input_line stdin) with
      | End_of_file -> None
      | Sys_error err when (* fragile *) err = "Broken pipe" -> None
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
