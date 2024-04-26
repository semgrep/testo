(*
   Error management and exception printing
*)

open Printf

exception Testo_failure of string
exception Testo_internal_error of { loc: string; msg: string }

let fail msg =
  raise (Testo_failure msg)

let internal_error ~__LOC__:loc msg =
  raise (Testo_internal_error { loc; msg })

let assert_false ~__LOC__:loc () =
  internal_error ~__LOC__:loc "this shouldn't have happened"

let invalid_arg ~__LOC__:loc msg =
  raise (Testo_internal_error { loc; msg = "Invalid argument: " ^ msg })

let () =
  Printexc.register_printer (function
    | Testo_failure msg ->
        Some (sprintf "Error: %s" msg)
    | Testo_internal_error {loc; msg} ->
        Some (sprintf "Internal error in the Testo library at %s: %s"
                loc msg)
    | _ -> None
  )
