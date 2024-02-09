(*
   Run tests that return Lwt promises
*)

type test = unit Lwt.t Testo.t

let mona : unit Lwt.t Testo.Mona.t =
  let catch func handler =
    Lwt.catch func (fun exn ->
        (* TODO: need to capture the stack trace earlier? How? *)
        let trace = Printexc.get_raw_backtrace () in
        handler exn trace)
  in
  { return = Lwt.return; bind = Lwt.bind; catch }

let create ?category ?checked_output ?expected_outcome ?normalize ?skipped
    ?tags ?tolerate_chdir name func =
  Testo.create_gen ?category ?checked_output ?expected_outcome
    ?normalize ?skipped ?tags ?tolerate_chdir mona name func

let interpret_argv = Testo.interpret_argv_gen ~mona
