(*
   Work with temporary files
*)

open Testo_util
open Fpath_.Operators
module P = Promise

(* TODO: for Windows, open the temporary file with O_SHARE_DELETE
   (FILE_SHARE_DELETE) so as to allow the file's deletion once all the
   additional file handles are gone. This would require reimplementing
   'Filename.open_temp_file' which isn't great, or having it done in
   the standard library. Alternatively, we could use pipes instead
   of temporary files in our 'with_capture' implementation, which
   might have the benefit of not polluting temp folders.

   The reason why we're ending up with extra handles must have to do with the
   output redirections we do via Unix.dup and Unix.dup2.

   Uses of this function without output redirections seem to work cleanly
   on all platforms.
*)
let with_open_temp_text_file ?contents ?(persist = false) ?(prefix = "testo-")
    ?(suffix = "") ?temp_dir func =
  let path, oc = Filename_.open_temp_file ?temp_dir prefix suffix in
  P.protect
    (fun () ->
      (match contents with
      | None -> ()
      | Some data ->
          output_string oc data;
          flush oc);
      func path oc)
    ~finally:(fun () ->
      close_out_noerr oc;
      (if not persist then
         (* This fails on Windows with 'permission denied' if there are any
           handles left for this file in this process or elsewhere.
           It's nice to ignore the error for robustness but it makes leaks
           invisible. *)
         try Sys.remove !!path with
         | Sys_error _ -> ());
      P.return ())

let with_temp_text_file ?contents ?persist ?prefix ?suffix ?temp_dir func =
  with_open_temp_text_file ?contents ?persist ?prefix ?suffix ?temp_dir
    (fun path _oc -> func path)
