(*
   Subset of the standard 'Filename' module using 'Fpath.t' for the type
   of file paths.
*)

let temp_file ?temp_dir prefix suffix =
  Filename.temp_file
    ?temp_dir:(Option.map Fpath.to_string temp_dir)
    prefix suffix
  |> Fpath.v

let get_temp_dir_name () = Filename.get_temp_dir_name () |> Fpath.v
