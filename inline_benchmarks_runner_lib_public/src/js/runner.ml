open! Core
open Core_bench_js
module Common = Inline_benchmarks_kernel_private

let main ~libname =
  Command_nodejs.run
    (Common.command
       ~filename_argtype:Command.Arg_type.Export.string
       ~libname
       ~bench:(fun ~analysis_configs ~display_config ~run_config ?save_to_file tests ->
         bench ~analysis_configs ~display_config ~run_config ?save_to_file ~libname tests))
;;
