(*
   Implementation of synchronous computations.

   This implementation is replaced by something else when using Lwt or Async
   to represent deferred computations (promises).
*)

type 'a t = 'a

let return x = x
let bind x func = func x

let catch func handler =
  try func ()
  with exn ->
    let trace = Printexc.get_raw_backtrace () in
    handler exn trace
