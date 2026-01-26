(*
   Reimplementation of file opening operations

   This is for better Windows support and to allow multicore
   support for users of OCaml 5+, as well as cloning the process with
   fork(). Note that Testo doesn't use these features directly but
   users of temporary files might.
*)

open Fpath_.Operators

(* Random-number generator + some safety against fork() resulting in
   two processes generating the same file names. *)
type prng = { pid : int; state : Random.State.t }

let create_prng () =
  { pid = Unix.getpid (); state = Random.State.make_self_init () }

(* This is used for compatibility with OCaml < 5.
   This is not suitable for multidomain uses, where each domain
   should have a different PRNG seed. *)
let default_prng = ref (create_prng ())

(* Ensure that we don't use a duplicated random number generator state
   after a fork() *)
let check_prng () =
  if Unix.getpid () <> !default_prng.pid then default_prng := create_prng ()

let default_get_random_key () =
  check_prng ();
  Random.State.bits !default_prng.state

(* Note that the temp dir depends on environment variables which
   can change during the lifetime of the process. *)
let get_temp_dir_path () = Fpath.v (Filename.get_temp_dir_name ())

let get_temp_file_path ?(get_random_key = default_get_random_key) ?(prefix = "")
    ?(suffix = "") ?(temp_dir = get_temp_dir_path ()) () =
  let rnd = get_random_key () land 0xFFFFFF in
  Fpath.add_seg temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let base_fd_flags : Unix.open_flag list =
  [
    (* Same flags as those used by 'open_out' and 'open_out_gen'
     for creating a file descriptor (Unix) or a file handle (Windows). *)
    O_WRONLY;
    O_CREAT;
    O_EXCL;
  ]

let open_out ?(perms = 0o666) ?(windows_binary = false)
    ?(windows_file_share_delete = true) path =
  let fd_flags =
    if windows_file_share_delete then Unix.O_SHARE_DELETE :: base_fd_flags
    else base_fd_flags
  in
  let fd = Unix.openfile !!path fd_flags perms in
  let oc = Unix.out_channel_of_descr fd in
  set_binary_mode_out oc windows_binary;
  oc

let open_temp_file ?get_random_key ?(perms = 0o600) ?temp_dir ?windows_binary
    ?windows_file_share_delete ?prefix ?suffix () =
  (* Same tactic as 'Filename.open_temp_file' from the standard library *)
  let rec try_name counter =
    let path =
      get_temp_file_path ?get_random_key ?prefix ?suffix ?temp_dir ()
    in
    try
      (path, open_out ~perms ?windows_binary ?windows_file_share_delete path)
    with
    | Sys_error _ as e ->
        if counter >= 20 then
          failwith
            ("Cannot create a temporary file after trying 20 different names: "
           ^ Printexc.to_string e)
        else try_name (counter + 1)
  in
  try_name 0
