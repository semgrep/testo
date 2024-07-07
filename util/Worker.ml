(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

open Printf

module Client = struct
  type worker = {
    id : string; (* unique ID derived from the slice *)
    in_channel : in_channel; (* to be closed with 'Unix.close_process_in' *)
    file_descr : Unix.file_descr;
    input_buffer : Buffer.t;
    mutable running : bool; (* = an END message has not been received *)
  }

  type t = { workers : worker list; read : unit -> (worker * Message.t) option }

  let fds_of_running_workers workers =
    List.filter_map
      (fun x -> if x.running then Some x.file_descr else None)
      workers

  let close_worker (x : worker) =
    x.running <- false;
    match Unix.close_process_in x.in_channel with
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
      (fun line -> Queue.add (worker, Message.of_string line) queue)
      lines

  let read_from_worker (x : worker) (queue : (worker * Message.t) Queue.t) =
    (* Read everything that's available to read now without blocking *)
    let buf_len = 1024 in
    let buf = Bytes.create buf_len in
    let bytes_read = input x.in_channel buf 0 buf_len in
    if bytes_read = 0 then (* end of input *)
      close_worker x
    else (
      (* Add bytes to the worker's input buffer and see if we have
         complete messages that we can parse and add to the message queue. *)
      Buffer.add_subbytes x.input_buffer buf 0 bytes_read;
      flush_input_buffer x x.input_buffer queue)

  let make_poller workers =
    (* The table maps file descriptor to worker data. This is needed to
       determine when all workers are done. *)
    let worker_tbl = Hashtbl.create 100 in
    List.iter
      (fun worker -> Hashtbl.add worker_tbl worker.file_descr worker)
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
                 workers *)
              match Unix.select in_fds [] [] (-1.) with
              | in_fd :: _, _, _ ->
                  let worker =
                    match Hashtbl.find_opt worker_tbl in_fd with
                    | None -> assert false
                    | Some x -> x
                  in
                  (* Read the available data, resulting in 0, 1, or more
                     messages being added to the queue. *)
                  read_from_worker worker incoming_message_queue;
                  poll ()
              | [], _, _ ->
                  (* This shouldn't happen if all the workers terminated
                     cleanly after sending an END message. *)
                  close_workers workers;
                  None))
    in
    poll

  let create ~num_workers ~original_argv ~test_list_checksum =
    let slices = Slice.partition num_workers in
    let workers =
      slices
      |> Helpers.list_map (fun slice ->
             let slice_str = Slice.to_string slice in
             let program_name =
               (* This requires the caller of the current program to not have
                  set argv[0] to something else than a valid command name! *)
               Sys.argv.(0)
             in
             let argv =
               Array.concat
                 [
                   original_argv;
                   [|
                     "--worker";
                     "--test-list-checksum";
                     test_list_checksum;
                     "--slice";
                     slice_str;
                   |];
                 ]
             in
             (* This is supposed to work on all platforms. *)
             let in_channel = Unix.open_process_args_in program_name argv in
             let file_descr = Unix.descr_of_in_channel in_channel in
             {
               id = slice_str;
               in_channel;
               file_descr;
               input_buffer = Buffer.create 200;
               running = true;
             })
    in
    let read = make_poller workers in
    { workers; read }

  let read (x : t) =
    x.read () |> Option.map (fun (worker, msg) -> (worker.id, msg))
end

module Server = struct
  let write msg =
    (* We print a newline before the message to terminate any junk
       that may have been written to stdout by user code. *)
    printf "\n%s\n%!" (Message.to_string msg)

  let fatal_error str =
    write (Error str);
    exit 1
end
