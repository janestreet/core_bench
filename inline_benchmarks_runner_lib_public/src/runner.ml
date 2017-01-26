open Core
open Core_bench.Std

module Entry = Ppx_bench_lib.Benchmark_accumulator.Entry

let x_library_inlining_warning ~run_without_inlining ~suppress_warnings =
  if not Version_util.x_library_inlining then begin
    if not suppress_warnings
    then
      Core.printf
        "Warning: X_LIBRARY_INLINING is not set to true, benchmarks may be inaccurate.\n%!";
    if not run_without_inlining then
      failwith "If you would like to run benchmarks, and are ok with getting inaccurate \
                results due to lack of cross library inlining, use the \
                -run-without-cross-library-inlining flag."
  end

(* The main function for the inline benchmarks *)
let run_benchmarks
      ~libname
      ~test_locations
      ~no_sexp:_
      ~run_config
      ~run_without_inlining
      ~suppress_warnings
      ~display_config
      ~analysis_configs
      ?save_to_file
      ()
  =
  x_library_inlining_warning ~run_without_inlining ~suppress_warnings;
  let _tbl, tests = Common.get_matching_tests ~libname test_locations in
  if List.is_empty tests
  then printf "No benchmarks to run!\n%!"
  else
    Bench.bench
      ~run_config
      ~analysis_configs
      ~display_config
      ?save_to_file
      ~libname:libname
      tests

let spec () =
  Command.Spec.(
    empty
    +> flag "matching" (listed string)
         ~doc:"REGEX Run benchmarks matching the REGEX."
    +> flag "no-sexp" no_arg
         ~doc:" Do not generate a benchmarks.sexp file (quicker)."
    +> flag "run-without-cross-library-inlining" no_arg
         ~doc:" Run benchmarks even when compiled with X_LIBRARY_INLINING=false."
    +> flag "suppress-warnings" no_arg
         ~doc:" Suppress warnings when clean output needed"
  )


let command ~libname =
  Bench.make_command_ext
    ~summary:(sprintf "run inline benchmarks of %s now." libname)
    ~extra_spec:(spec ())
    ~f:(fun args test_locations no_sexp run_without_inlining suppress_warnings () ->
      let should_run =
        Option.value_map ~default:false ~f:((=) "TRUE") (Sys.getenv "BENCHMARKS_RUNNER")
      in
      if not should_run
      then failwith "Don't run directly, run using the benchmarks_runner script.";
      match args with
      | (analysis_configs, display_config, `Run (save_to_file, run_config)) ->
        run_benchmarks
          ~libname
          ~test_locations
          ~no_sexp
          ~run_config
          ~run_without_inlining
          ~suppress_warnings
          ~display_config
          ~analysis_configs
          ?save_to_file
          ()
      | (_analysis_configs, _display_config, `From_file _filenames) ->
        failwith "Loading saved files is not supported for inline executables."
    )

let main ~libname = Command.run (command ~libname)
