(*
   Messages sent from the master process to a worker.
*)

open Printf

type t = Start_test of string

let of_string str =
  match String.split_on_char ' ' str with
  | [ "START_TEST"; test_id ] -> Start_test test_id
  | _ -> failwith (sprintf "Invalid message received from master: %S" str)

let to_string x =
  match x with
  | Start_test test_id -> sprintf "START_TEST %s" test_id
