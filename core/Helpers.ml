(*
   Generic functions used by more than one module in this library
*)

open Printf
open Filename_.Operators
open Promise.Operators
module P = Promise

(* safe version of List.map for ocaml < 5 *)
let list_map f l = List.rev_map f l |> List.rev

(* safe version of List.flatten *)
let list_flatten ll =
  List.fold_left (fun acc l -> List.rev_append l acc) [] ll |> List.rev

let rec make_dir_if_not_exists ?(recursive = false) (dir : Filename_.t) =
  match (Unix.stat !!dir).st_kind with
  | S_DIR -> ()
  | S_REG
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK ->
      Error.fail
        (sprintf
           "File %S already exists but is not a folder as required by the \
            testing setup."
           !!dir)
  | exception Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename_.dirname dir in
      if parent = dir then
        (* dir is something like "." or "/" *)
        Error.fail
          (sprintf
             "Folder %S doesn't exist and has no parent that we could create."
             !!dir)
      else if recursive then (
        make_dir_if_not_exists ~recursive parent;
        Unix.mkdir !!dir 0o777)
      else if Sys.file_exists !!parent then
        Unix.mkdir !!dir 0o777
      else
        Error.fail
          (sprintf "The parent folder of %S doesn't exist (current folder: %S)"
             !!dir (Sys.getcwd ()))

let contains_pcre_pattern pat =
  let rex = Re.Pcre.regexp pat in
  fun str -> Re.execp rex str

let contains_substring substring =
  contains_pcre_pattern (Re.Pcre.quote substring)

let with_temp_file
    ?contents
    ?(persist = false)
    ?(prefix = "testo-")
    ?(suffix = "")
    ?temp_dir
    func =
  let path = Filename.temp_file ?temp_dir prefix suffix in
  P.protect
    (fun () ->
       (match contents with
        | None -> P.return ()
        | Some data ->
            let oc = open_out_bin path in
            P.protect
              (fun () -> output_string oc data; P.return ())
              ~finally:(fun () -> close_out_noerr oc; P.return ())
       ) >>= fun () ->
       func path
    )
    ~finally:(fun () ->
      if not persist then
        Sys.remove path;
      P.return ()
    )
