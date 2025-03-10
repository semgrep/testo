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

let contains_substring ~sub =
  contains_pcre_pattern ~pat:(Re.Pcre.quote sub)

let write_file path data =
  let oc = open_out_bin !!path in
  Fun.protect
    (fun () -> output_string oc data)
    ~finally:(fun () -> close_out_noerr oc)

let read_file path =
  let ic = open_in_bin !!path in
  Fun.protect
    (fun () ->
      (* This fails for named pipes *)
      let len = in_channel_length ic in
      really_input_string ic len)
    ~finally:(fun () -> close_in_noerr ic)
