(*
   Work with temporary files
*)

open Testo_util
open Fpath_.Operators
module P = Promise

let with_open_temp_file ?contents ?get_random_key ?perms ?(persist = false)
    ?(prefix = "testo-") ?suffix ?temp_dir ?windows_binary
    ?windows_file_share_delete func =
  let path, oc =
    Create_temp_file.open_temp_file ?get_random_key ?perms ~prefix ?suffix
      ?temp_dir ?windows_binary ?windows_file_share_delete ()
  in
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
           It's nice to ignore the error for robustness but unfortunately
           it makes temp folder pollution harder to notice.
           We may want to print a warning but it's not clear how to make it
           nonintrusive. *)
         try if Sys.file_exists !!path then Sys.remove !!path with
         | Sys_error _ -> ());
      P.return ())

let with_temp_file ?contents ?get_random_key ?perms ?persist ?prefix ?suffix
    ?temp_dir ?windows_binary ?windows_file_share_delete func =
  with_open_temp_file ?contents ?get_random_key ?perms ?persist ?prefix ?suffix
    ?temp_dir ?windows_binary ?windows_file_share_delete (fun path _oc ->
      func path)
