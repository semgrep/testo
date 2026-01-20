(* Work with a temporary file, ensuring its eventual deletion. *)
val with_open_temp_text_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  (Fpath.t -> out_channel -> 'a Promise.t) ->
  'a Promise.t

val with_temp_text_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  (Fpath.t -> 'a Promise.t) ->
  'a Promise.t
