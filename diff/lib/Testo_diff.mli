(** A simple diffing algorithm

    The current implementation is a copy of Gabriel Jaldon's original OCaml
    implementation at https://github.com/gjaldon/simple-diff

    The heuristic being used is subject to change and may result in slightly
    different results in future versions. *)

module type Comparable = sig
  type t [@@deriving show]
  (** The type of the items being compared *)

  val compare : t -> t -> int
  (** A way to distinguish if items are equal or unequal. It follows the OCaml
      convention of returning an integer between -1 to 1. *)
end

module type S = sig
  type item [@@deriving show]
  (** The type of the item that will be compared. *)

  (** Represents the change or lack of change in a line or character between the
      old and new version. *)
  type diff =
    | Deleted of item array
    | Added of item array
    | Equal of item array
  [@@deriving show]

  type t = diff list [@@deriving show]
  (** List of diffs which is the return value of the main function. *)

  val get_diff : item array -> item array -> t
  (** Returns a list of diffs between two arrays *)

  val recover_input : t -> item array * item array
  (** Recover the original input passed to {!get_diff}. *)
end

module Make (Item : Comparable) : S with type item = Item.t
