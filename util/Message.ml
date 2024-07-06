(*
   Read and write messages between workers and master used to manage
   parallel test execution

   This is designed to be simple to debug and to minimize external
   dependencies.
*)

open Printf

type t =
  | Start_test of string (* test ID *)
  | End_test of string (* test ID *)
  | End
  | Error of string (* error message *)
  | Junk of string (* any non-conforming line of input *)

let check_test_id str =
  str <> ""
  && String.for_all
       (function
         | '0' .. '9'
         | 'a' .. 'f' ->
             true
         | _ -> false)
       str

let of_string str =
  match String.index_opt str ' ' with
  | None -> (
      match str with
      | "END" -> End
      | _ -> Junk str)
  | Some i ->
      let kind = String.sub str 0 i in
      let payload = String.sub str (i + 1) (String.length str - i - 1) in
      let test_id = payload in
      if check_test_id test_id then
        match kind with
        | "START_TEST" -> Start_test test_id
        | "END_TEST" -> End_test test_id
        | _ -> Junk str
      else Junk str

let replace_newlines str =
  String.map
    (function
      | '\n' -> ' '
      | c -> c)
    str

let to_string x =
  (match x with
  | Start_test test_id -> sprintf "START_TEST %s" test_id
  | End_test test_id -> sprintf "END_TEST %s" test_id
  | End -> "END"
  | Error msg -> sprintf "ERROR %s" msg
  | Junk data ->
      (* This should be unused. A writer should not send 'Junk' messages.
         If they do, it's by writing junk to stdout without going through
         this function. *)
      sprintf "JUNK %s" data)
  |> replace_newlines
