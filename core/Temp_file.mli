(* Work with a temporary file, ensuring its eventual deletion. *)

val with_open_temp_file :
  ?contents:string ->
  ?get_random_key:(unit -> int) ->
  ?perms:int ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  ?windows_binary:bool ->
  ?windows_file_share_delete:bool ->
  (Fpath.t -> out_channel -> 'a Promise.t) ->
  'a Promise.t

val with_temp_file :
  ?contents:string ->
  ?get_random_key:(unit -> int) ->
  ?perms:int ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  ?windows_binary:bool ->
  ?windows_file_share_delete:bool ->
  (Fpath.t -> 'a Promise.t) ->
  'a Promise.t
