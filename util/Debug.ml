(*
   Log messages for debugging purposes

   This is mostly for tracing the master/worker interactions.
*)

let debug = ref false
let log make_msg = Printf.eprintf "[DEBUG] %s\n%!" (make_msg ())
