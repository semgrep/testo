(*
   Lazy computations that capture and restore stdout/stderr

   See interface documentation included in Testo.mli.
*)

type 'a t

type redirect = Stdout_to_stderr | Stderr_to_stdout

val create : ?redirect:redirect -> (unit -> 'a Promise.t) -> 'a t

val force : 'a t -> 'a Promise.t
