(*
   A trivial wrapper around "string" to make signatures clearer.
*)

type t = string

let of_string x = x
let to_string x = x

include Filename

module Operators = struct
  let ( // ) a b =
    if is_relative b then concat a b else b

  let ( / ) a seg =
    concat a seg

  let ( !! ) path = path
end
