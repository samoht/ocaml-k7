(** Tests for {!Vhs}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks tape printing. *)

val cram_suite : string * unit Alcotest.test_case list
(** [cram_suite] checks cram parsing and tape translation. *)

val run_suite : string * unit Alcotest.test_case list
(** [run_suite] checks running [vhs]. *)
