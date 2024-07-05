(*
   Return the number of available logical processors on the machine.
*)

(* Adapted from https://stackoverflow.com/a/16273514/597517 *)
let get_count () =
  try
    Some
      (match Sys.os_type with
      | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")
      | _ ->
          let ic = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
          let close () = ignore (Unix.close_process_in ic) in
          Fun.protect ~finally:close (fun () -> input_line ic |> int_of_string))
  with
  | Not_found
  | Sys_error _
  | Failure _
  | End_of_file
  | Unix.Unix_error (_, _, _) ->
      None
