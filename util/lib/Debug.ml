(*
   Log messages for debugging purposes

   This is mostly for tracing the master/worker interactions.
*)

open Printf

let debug = ref false

let log make_msg =
  if !debug then
    eprintf "[DEBUG] [%.6f] %s\n%!" (Unix.gettimeofday ()) (make_msg ())
