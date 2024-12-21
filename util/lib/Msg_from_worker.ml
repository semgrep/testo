(*
   Read and write messages between workers and master used to manage
   parallel test execution

   This is designed to be simple to debug and to minimize external
   dependencies.
*)

open Printf

type t =
  | End_test of string (* test ID *)
  | Error of string (* error message *)
  | Junk of string (* any non-conforming line of input *)

let check_test_id str =
  str <> ""
  && Helpers.string_for_all
       (function
         | '0' .. '9'
         | 'a' .. 'f' ->
             true
         | _ -> false)
       str

let of_string str =
  match String.index_opt str ' ' with
  | None -> Junk str
  | Some i -> (
      let kind = String.sub str 0 i in
      let payload = String.sub str (i + 1) (String.length str - i - 1) in
      match kind with
      | "END_TEST" ->
          let test_id = payload in
          if check_test_id test_id then End_test test_id else Junk str
      | "ERROR" -> Error payload
      | _ -> Junk str)

let replace_newlines str =
  String.map
    (function
      | '\n' -> ' '
      | c -> c)
    str

let to_string x =
  (match x with
  | End_test test_id -> sprintf "END_TEST %s" test_id
  | Error msg -> sprintf "ERROR %s" msg
  | Junk data ->
      (* This should be unused. A writer should not send 'Junk' messages.
         If they do, it's by writing junk to stdout without going through
         this function. *)
      sprintf "JUNK %s" data)
  |> replace_newlines
