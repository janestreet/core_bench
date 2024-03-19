(** A module internal to [Core_bench]. Please look at {!Bench}.

    Runs a user specified benchmark and measures runtime, allocations etc. *)

open! Core
open Core_bench_internals

val measure_all
  :  ?postprocess:(Measurement.t -> unit)
  -> Run_config.t
  -> Test.Basic_test.t list
  -> Measurement.t list
