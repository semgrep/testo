(*
   Monad interface shared by all variants of Testo (synchronous, Lwt, ...)

   See 'core/Monad.mli'.
*)

type 'a t = 'a Lwt.t

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val catch : (unit -> 'a t) -> (exn -> Printexc.raw_backtrace -> 'a t) -> 'a t
