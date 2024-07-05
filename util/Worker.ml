(*
   Manage the lifecycle of worker processes used for parallel test runs.
*)

type worker = {
  id : string; (* unique ID derived from the slice *)
  slice : Slice.t;
  in_channel : in_channel; (* to be closed with 'Unix.close_process_in' *)
  file_descr : Unix.file_descr;
  pid : int;
  input_buffer : Buffer.t;
  mutable running : bool; (* = an END message has not been received *)
}

type t = { workers : worker list; read : Message.t option }

let still_running workers = List.exists (fun x -> x.running) workers

(* Add bytes to a worker's input buffer. This results in 0, 1, or more
   messages becoming available. *)
let add_bytes (x : worker) : Message.t list = failwith "TODO: input buffer"

let close_worker (x : worker) =
  x.running <- false;
  match Unix.close_process_in x.in_channel with
  | _status -> ()

let close_workers workers = List.iter close_worker workers

let read_from_worker (x : worker) (queue : Message.t Queue.t) : Message.t list =
  (* Read everything that's available to read now without blocking *)
  let buf_len = 1024 in
  let buf = Bytes.create buf_len in
  let bytes_read = input x.in_channel buf 0 buf_len in
  if bytes_read = 0 then (
    (* end of input *)
    close_worker x;
    [])
  else (
    (* Add bytes to the worker's input buffer and see if we have
       complete messages that we can parse and add to the message queue. *)
    Buffer.add_subbytes x.input_buffer buf 0 bytes_read;
    flush_input_buffer x.input_buffer queue)

let make_poller workers =
  (* The table maps file descriptor to worker data. This is needed to
     determine when all workers are done. *)
  let worker_tbl = Hashtbl.create 100 in
  List.iter (fun worker -> Hashtbl.add worker_tbl worker.file_descr worker);
  let in_fds = Helpers.list_map (fun x -> x.file_descr) in
  let incoming_message_queue = Queue.create () in
  let rec poll () =
    match Queue.take_opt incoming_message_queue with
    | Some msg -> Some msg
    | None ->
        if still_running workers then (
          (* Wait for data being available to read from one of the workers *)
          match Unix.select in_fds [] [] (-1.) with
          | in_fd :: _, _, _ ->
              let worker =
                match Hashtbl.find_opt worker_tbl in_fd with
                | None -> assert false
                | Some x -> x
              in
              (* Read the available data, resulting in 0, 1, or more messages
                 being added to the queue. *)
              read_from_worker worker incoming_message_queue;
              poll ()
          | [], _, _ ->
              (* This shouldn't happen if all the workers terminated cleanly
                 after sending an END message. *)
              close_workers workers;
              None)
        else None
  in
  poll

let create ~num_workers ~original_argv =
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
               [ original_argv; [| "--worker"; "--slice"; slice_str |] ]
           in
           (* This is supposed to work on all platforms. *)
           let in_channel = Unix.open_process_args_in program_name argv in
           let pid = Unix.process_in_pid in_channel in
           { id = slice_str; slice; in_channel; pid })
  in
  let read () = make_poller workers in
  { workers; read }
