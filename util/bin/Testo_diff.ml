(*
   Minimal 'diff -u' command
*)

let main () =
  let exit_code =
    match Sys.argv with
    | [| _; a; b |] -> (
        match Testo_util.Diff.files ~color:true (Fpath.v a) (Fpath.v b) with
        | None -> 0
        | Some diff ->
            print_string diff;
            flush stdout;
            1)
    | _ ->
        Printf.eprintf "Usage: testo-diff FILE1 FILE2\n%!";
        2
  in
  exit exit_code

let () = main ()
