(*
   Subset of the standard 'Filename' module using 'Fpath.t' for the type
   of file paths.
*)

let open_temp_file ?temp_dir prefix suffix =
  let path, oc =
    Filename.open_temp_file
      ?temp_dir:(Option.map Fpath.to_string temp_dir)
      prefix suffix
  in
  (Fpath.v path, oc)

let get_temp_dir_name () = Filename.get_temp_dir_name () |> Fpath.v
