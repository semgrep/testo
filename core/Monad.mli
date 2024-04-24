(*
   The interface for the module representing either an ordinary
   computation or a deferred computation or promise.

   It is designed for wrapping a sequence of tests that provide their result
   by either returning (success) or by raising an exception (failure).
   This wrapping allows asynchronous computations i.e. allows running
   a sequence of tests interleaved with other computations. While such
   concurrency is not a property we need for running a test suite,
   it is necessary due to environments where we can't turn asynchronous
   computations into synchronous ones such as JavaScript.
   See https://github.com/mirage/alcotest/issues/119
*)

(*
   The type of a promise. For a synchronous computations, 'a = 'a t.
   For an asynchronous computation, this a wrapper around mutable content
   that will eventually hold the result of the computation.
*)
type 'a t = 'a

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t

(* Exception catcher.

   [catch func handler] must catch any exception raised by [func ()]
   or raised during later computations leading to the resolution
   of the promise.
*)
val catch :
  (unit -> 'a t) ->
  (exn -> Printexc.raw_backtrace -> 'a t) ->
  'a t
