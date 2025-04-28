open! Core
open Core_bench
module Common = Inline_benchmarks_kernel_private

let main ~libname =
  Command_unix.run
    (Common.command
       ~filename_argtype:Filename_unix.arg_type
       ~libname
       ~bench:(fun ~analysis_configs ~display_config ~run_config ?save_to_file tests ->
         Bench.bench
           ~analysis_configs
           ~display_config
           ~run_config
           ?save_to_file
           ~libname
           tests))
;;
