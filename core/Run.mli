(*
   Filter and run tests
*)

type status_output_style =
  | Long_all
  | Compact_all
  | Long_important
  | Compact_important

(* Type alias for Alcotest test cases *)
type alcotest_test_case = string * [ `Quick | `Slow ] * (unit -> unit Promise.t)

(* Type alias for an Alcotest 'test'. *)
type alcotest_test = string * alcotest_test_case list

(* See comments in the public interface Testo.mli *)
val to_alcotest :
  alcotest_skip:(unit -> _) -> Types.test list -> alcotest_test list

val cmd_run :
  always_show_unchecked_output:(bool * int option) ->
  argv:string array ->
  autoclean:bool ->
  filter_by_substring:string list option ->
  filter_by_tag:Testo_util.Tag_query.t option ->
  intro:string ->
  is_worker:bool ->
  jobs:int option ->
  lazy_:bool ->
  slice:Testo_util.Slice.t list ->
  strict:bool ->
  test_list_checksum:string option ->
  Types.test list ->
  (int -> Types.test_with_status list -> _) ->
  _ Promise.t

(* Print the status of each test.
   Return a non-zero exit status if any of the tests is not a success
   (PASS or XFAIL). *)
val cmd_status :
  always_show_unchecked_output:(bool * int option) ->
  autoclean:bool ->
  filter_by_substring:string list option ->
  filter_by_tag:Testo_util.Tag_query.t option ->
  intro:string ->
  output_style:status_output_style ->
  strict:bool ->
  Types.test list ->
  int * Types.test_with_status list

val cmd_approve :
  filter_by_substring:string list option ->
  filter_by_tag:Testo_util.Tag_query.t option ->
  Types.test list ->
  int

val introduction_text : string

(* This returns the test being executed, if any.
   It allows a test function to obtain the test object or to call functions
   that depend on the test object while keeping the type of the test
   function simple.
*)
val get_current_test : unit -> Types.test option
