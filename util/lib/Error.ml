(*
   Error management and exception printing
*)

open Printf

type msg = Error of string | Warning of string

exception Test_failure of string
exception User_error of string
exception Internal_error of { loc : string; msg : string }

let fail_test msg = raise (Test_failure msg)
let user_error msg = raise (User_error msg)
let internal_error ~__LOC__:loc msg = raise (Internal_error { loc; msg })

let assert_false ~__LOC__:loc () =
  internal_error ~__LOC__:loc "this shouldn't have happened"

let invalid_arg ~__LOC__:loc msg =
  raise (Internal_error { loc; msg = "Invalid argument: " ^ msg })

let () =
  Printexc.register_printer (function
    | Test_failure msg -> Some (sprintf "Test failed: %s" msg)
    | User_error msg -> Some (sprintf "Error: %s" msg)
    | Internal_error { loc; msg } ->
        Some (sprintf "Internal error in the Testo library at %s: %s" loc msg)
    | _ -> None)

module Exit_code = struct
  let success = 0
  let test_failure = 1
  let configuration_error = 2
  let internal_error = 3
end
