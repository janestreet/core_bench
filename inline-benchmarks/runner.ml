open Core.Std
open Core_bench.Std

module Entry = Ppx_bench_lib.Benchmark_accumulator.Entry

let make_benchmark_name entry =
  let module_name =
    match Entry.get_module_name_opt entry with
    | Some s -> ":" ^ s
    | None -> ""
  in
  let bench_module_name =
    match entry.Entry.bench_module_name with
    | Some s -> ":" ^ s
    | None -> ""
  in
  "[" ^ entry.Entry.filename
  ^ module_name
  ^ bench_module_name
  ^ "] " ^ entry.Entry.name

(* Code for filtering the out the benchmarks to run *)
let entry_to_bench_test entry ~key =
  let open Entry in
  let name = make_benchmark_name entry in
  match entry.Entry.test_spec with
  | Regular_thunk f ->
       let func = f () in
       Bench.Test.create ~name ~key func
  | Indexed_thunk { arg_values; thunk; _ } ->
    Bench.Test.create_indexed
      ~name ~args:arg_values ~key
      (fun len -> Staged.stage (thunk len))

let pattern_to_predicate s =
  let regexp = Str.regexp (".*" ^ s ^ ".*") in
  (fun entry ->
    let name = make_benchmark_name entry in
    Str.string_match regexp name 0)

let get_matching_tests ~libname patterns =
  let tbl = Int.Table.create () in
  let entries = Ppx_bench_lib.Benchmark_accumulator.lookup_lib ~libname in
  let entries =
    match patterns with
    (* if no regexes are specified not specified, run all entries *)
    | [] -> entries
    | _ :: _ ->
      List.dedup ~compare:Entry.compare
        (List.concat_map patterns ~f:(fun pattern ->
           let entries = List.filter entries ~f:(pattern_to_predicate pattern) in
           if List.is_empty entries
           then printf "Warning: %s didn't match any benchmark\n" pattern;
           entries))
  in
  let tests =
    List.map entries ~f:(fun entry ->
      let key = entry.Entry.unique_id in
      Hashtbl.add_exn tbl ~key ~data:entry;
      entry_to_bench_test entry ~key)
  in
  tbl, tests

let x_library_inlining_warning ~run_without_inlining =
  if not Version_util.x_library_inlining then begin
    Core.Std.printf
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
      ~display_config
      ~analysis_configs
      ?save_to_file
      ()
  =
  x_library_inlining_warning ~run_without_inlining;
  let _tbl, tests = get_matching_tests ~libname test_locations in
  if List.is_empty tests
  then printf "No benchmarks to run!\n%!"
  else
    Bench.bench
      ~run_config
      ~analysis_configs
      ~display_config
      ?save_to_file
      tests

let spec () =
  Command.Spec.(
    empty
    +> flag "benchmarks-runner" no_arg
         ~doc:" Always need to include this, no benchmarks are run otherwise."
    +> flag "matching" (listed string)
         ~doc:"REGEX Run benchmarks matching the REGEX."
    +> flag "no-sexp" no_arg
         ~doc:" Do not generate a benchmarks.sexp file (quicker)."
    +> flag "run-without-cross-library-inlining" no_arg
         ~doc:" Run benchmarks even when compiled with X_LIBRARY_INLINING=false."
  )


let main ~libname =
  let command =
    Bench.make_command_ext
      ~summary:(sprintf "Runs inline benchmarks in lib %s." libname)
      ~extra_spec:(spec ())
      ~f:(fun args benchmarks_runner test_locations no_sexp run_without_inlining () ->
        if not benchmarks_runner
        then failwith "Don't run directly, run using the benchmarks_runner script.";
        match args with
        | (analysis_configs, display_config, `Run (save_to_file, run_config)) ->
          run_benchmarks
            ~libname
            ~test_locations
            ~no_sexp
            ~run_config
            ~run_without_inlining
            ~display_config
            ~analysis_configs
            ?save_to_file
            ()
        | (_analysis_configs, _display_config, `From_file _filenames) ->
          failwith "Loading saved files is not supported for inline executables."
      )
  in
  Command.run command

