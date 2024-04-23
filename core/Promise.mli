(*
   Extension of the Monad module with generic utilities.
*)

type 'a t = 'a Monad.t

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val catch :
  (unit -> 'a t) ->
  (exn -> Printexc.raw_backtrace -> 'a t) ->
  'a t

module Operators : sig
  (* The bind operator *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(* Generalized version of 'Fun.protect' *)
val protect :
  (unit -> 'a t) ->
  finally:(unit -> unit t) ->
  'a t
