(*
   Generic functions used by more than one module in this library
*)

open Printf
open Fpath_.Operators

(* safe version of List.map for ocaml < 5 *)
let list_map f l = List.rev_map f l |> List.rev

(* safe version of List.flatten *)
let list_flatten ll =
  List.fold_left (fun acc l -> List.rev_append l acc) [] ll |> List.rev

let split_result_list xs =
  let oks, errs =
    List.fold_left
      (fun (oks, errs) x ->
        match x with
        | Ok ok -> (ok :: oks, errs)
        | Error err -> (oks, err :: errs))
      ([], []) xs
  in
  (List.rev oks, List.rev errs)

let string_for_all func str =
  try
    for i = 0 to String.length str - 1 do
      if not (func str.[i]) then raise Exit
    done;
    true
  with
  | Exit -> false

let make_dir_if_not_exists ?(recursive = false) (dir : Fpath.t) =
  let rec mkdir dir =
    match (Unix.stat !!dir).st_kind with
    | S_DIR -> ()
    | S_REG
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK ->
        Error.user_error
          (sprintf
             "File %S already exists but is not a folder as required by the \
              testing setup."
             !!dir)
    | exception Unix.Unix_error (ENOENT, _, _) ->
        let parent = Fpath_.dirname dir in
        if parent = dir then
          (* dir is something like "." or "/" *)
          Error.user_error
            (sprintf
               "Folder %S doesn't exist and has no parent that we could create."
               !!dir)
        else if recursive then (
          mkdir parent;
          Unix.mkdir !!dir 0o777)
        else if Sys.file_exists !!parent then Unix.mkdir !!dir 0o777
        else
          Error.user_error
            (sprintf
               "The parent folder of %S doesn't exist (current folder: %S)"
               !!dir (Sys.getcwd ()))
  in
  dir |> Fpath.normalize |> Fpath.rem_empty_seg |> mkdir

let list_files dir =
  let names = ref [] in
  let dir = Unix.opendir !!dir in
  Fun.protect
    (fun () ->
      try
        while true do
          match Unix.readdir dir with
          | "."
          | ".." ->
              ()
          | name -> names := name :: !names
        done
      with
      | End_of_file -> ())
    ~finally:(fun () -> Unix.closedir dir);
  List.sort String.compare !names

let rec remove_file_or_dir path =
  if Sys.file_exists !!path then
    match (Unix.stat !!path).st_kind with
    | S_DIR ->
        path |> list_files
        |> list_map (fun name -> path / name)
        |> List.iter remove_file_or_dir;
        Unix.rmdir !!path
    | S_REG
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK ->
        Sys.remove !!path

let contains_pcre_pattern ~pat =
  let rex = Re.Pcre.regexp pat in
  fun str -> Re.execp rex str

let contains_substring ~sub = contains_pcre_pattern ~pat:(Re.Pcre.quote sub)

let write_text_file path data =
  let oc = open_out !!path in
  Fun.protect
    (fun () -> output_string oc data)
    ~finally:(fun () -> close_out_noerr oc)

(* 'In_channel.input_all' is available in the standard library starting with
   OCaml 4.14 *)
let input_all ic =
  let buf = Buffer.create 4096 in
  let tmp = Bytes.create 4096 in
  let rec loop () =
    match input ic tmp 0 (Bytes.length tmp) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf tmp 0 n;
        loop ()
  in
  loop ();
  Buffer.contents buf

let read_text_file path =
  let ic = open_in !!path in
  Fun.protect (fun () -> input_all ic) ~finally:(fun () -> close_in_noerr ic)

let map_text_file func src_path dst_path =
  let old_contents = read_text_file src_path in
  let new_contents = func old_contents in
  write_text_file dst_path new_contents

let copy_text_file src_path dst_path =
  map_text_file (fun data -> data) src_path dst_path

let chdir dir =
  try Sys.chdir !!dir with
  | e ->
      failwith
        (sprintf "Cannot chdir into '%s': %s" (Printexc.to_string e) !!dir)

let with_chdir path func =
  let orig_cwd = Sys.getcwd () in
  chdir path;
  Fun.protect ~finally:(fun () -> Sys.chdir orig_cwd) func

let with_opt_chdir opt_path func =
  match opt_path with
  | None -> func ()
  | Some path -> with_chdir path func
