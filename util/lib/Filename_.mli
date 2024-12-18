(*
   Subset of the standard 'Filename' module using 'Fpath.t' for the type
   of file paths.
*)

val temp_file : ?temp_dir:Fpath.t -> string -> string -> Fpath.t
val get_temp_dir_name : unit -> Fpath.t
