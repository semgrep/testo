(*
   Log messages for debugging purposes

   This is mostly for tracing the master/worker interactions.
*)

let debug = ref false
let log make_msg = if !debug then Printf.eprintf "[DEBUG] %s\n%!" (make_msg ())
