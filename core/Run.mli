(*
   Filter and run tests
*)

type status_output_style =
  | Long_all
  | Compact_all
  | Long_important
  | Compact_important

(* Type alias for Alcotest test cases *)
type alcotest_test_case =
  string * [ `Quick | `Slow ] * (unit -> unit Promise.t)

(* Type alias for an Alcotest 'test'. *)
type alcotest_test =
  string * alcotest_test_case list

(* See comments in the public interface Testo.mli *)
val to_alcotest :
  Types.test list -> alcotest_test list

val run_tests :
  always_show_unchecked_output:bool ->
  filter_by_substring:string option ->
  filter_by_tag:Testo_util.Tag.t option ->
  lazy_:bool ->
  Types.test list ->
  (int -> Types.test_with_status list -> _) ->
  _

(* Print the status of each test.
   Return a non-zero exit status if any of the tests is not a success
   (PASS or XFAIL). *)
val list_status :
  always_show_unchecked_output:bool ->
  filter_by_substring:string option ->
  filter_by_tag:Testo_util.Tag.t option ->
  output_style:status_output_style ->
  Types.test list ->
  int * Types.test_with_status list

val approve_output :
  ?filter_by_substring:string ->
  ?filter_by_tag:Testo_util.Tag.t ->
  Types.test list -> int
