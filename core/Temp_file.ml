(*
   Work with temporary files
*)

open Testo_util
open Fpath_.Operators
module P = Promise

let with_temp_file ?contents ?(persist = false) ?(prefix = "testo-")
    ?(suffix = "") ?temp_dir func =
  let path = Filename_.temp_file ?temp_dir prefix suffix in
  P.protect
    (fun () ->
      (match contents with
      | None -> ()
      | Some data -> Helpers.write_file path data);
      func path)
    ~finally:(fun () ->
      if not persist then Sys.remove !!path;
      P.return ())
