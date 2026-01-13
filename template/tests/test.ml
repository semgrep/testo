(*
   The entry point for the 'test' program that runs the suite of OCaml
   tests for <this project>.
*)

let test_hello =
  Testo.create "hello"
    (fun () -> print_endline "hello!")

let tests _env = [
  test_hello;
]

let () =
  Testo.interpret_argv
    ~project_name:"my_project"
    tests
