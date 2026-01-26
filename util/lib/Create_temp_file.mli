(** Reimplementation of temporary file creation

    This solves the following problems:

    - open files on Windows such that they can be deleted even if some file
      handles still exist, which occurs after output redirection via dup/dup2.
    - force a reinitialization of the random number generator sequence after a
      [Unix.fork()]. *)

val get_temp_dir_path : unit -> Fpath.t

val get_temp_file_path :
  ?get_random_key:(unit -> int) ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  unit ->
  Fpath.t

val open_out :
  ?perms:int ->
  ?windows_binary:bool ->
  ?windows_file_share_delete:bool ->
  Fpath.t ->
  out_channel

val open_temp_file :
  ?get_random_key:(unit -> int) ->
  ?perms:int ->
  ?temp_dir:Fpath.t ->
  ?windows_binary:bool ->
  ?windows_file_share_delete:bool ->
  ?prefix:string ->
  ?suffix:string ->
  unit ->
  Fpath.t * out_channel
