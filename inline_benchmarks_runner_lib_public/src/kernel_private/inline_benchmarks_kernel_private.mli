open! Core
open Core_bench_internals

(** This library factors out the logic for running inline benchmarks from the JS and
    native runners. Having separate runners for JS and native is necessary because
    [core_bench] uses some unix-only APIs, e.g. forking and lib/pmc.

    The signatures here are fairly arbitrary, and should be considered private. *)

val command
  :  filename_argtype:string Command.Arg_type.t
  -> libname:string
  -> bench:
       (analysis_configs:Analysis_config.t list
        -> display_config:Display_config.t
        -> run_config:Run_config.t
        -> ?save_to_file:(Measurement.t -> string)
        -> Test.t list
        -> unit)
  -> Command.t
