(*
   Subset of the standard 'Filename' module using 'Fpath.t' for the type
   of file paths.
*)

(* This is not strictly file path manipulation as it creates a file.
   This is the same as 'Filename.temp_file' except for the path type. *)
val temp_file : ?temp_dir:Fpath.t -> string -> string -> Fpath.t
