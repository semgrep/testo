(*
   Extensions of the Fpath module which deals with file paths.
*)

module Operators = struct
  let ( // ) = Fpath.( // )
  let ( / ) = Fpath.( / )
  let ( !! ) = Fpath.to_string
end

let to_string_list paths = List.rev_map Fpath.to_string paths |> List.rev
