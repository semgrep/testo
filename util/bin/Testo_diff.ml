(*
   Minimal 'diff -u' command
*)

let main () =
  let exit_code =
    match Sys.argv with
    | [| _; a; b |] ->
        let equal, diff =
          Testo_util.Diff.files ~color:true (Fpath.v a) (Fpath.v b)
        in
        if equal then 0
        else (
          print_string diff;
          flush stdout;
          1
        )
  | _ ->
      Printf.eprintf "Usage: testo-diff FILE1 FILE2\n%!";
      2
  in
  exit exit_code

let () = main ()
