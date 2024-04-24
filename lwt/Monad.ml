(*
   Lwt promises
*)

type 'a t = 'a Lwt.t

let return = Lwt.return
let bind = Lwt.bind

let catch func handler =
  Lwt.catch func (fun exn ->
    (* TODO: need to capture the stack trace earlier? How? *)
    let trace = Printexc.get_raw_backtrace () in
    handler exn trace
  )
