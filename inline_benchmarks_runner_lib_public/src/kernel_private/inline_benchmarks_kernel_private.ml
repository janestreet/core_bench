open Core
open Core_bench_internals
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
  String.concat
    [ "["; entry.Entry.filename; module_name; bench_module_name; "] "; entry.Entry.name ]
;;

(* Code for filtering the out the benchmarks to run *)
let entry_to_bench_test entry ~key =
  let open Entry in
  let name = make_benchmark_name entry in
  let test_name = entry.name in
  let file_name = entry.filename in
  let module_name = entry.bench_module_name in
  match entry.Entry.test_spec with
  | Regular_thunk f ->
    Test.create_with_initialization
      ~name
      ~test_name
      ~file_name
      ?module_name
      ~key
      (fun `init -> (f `init).uncurried)
  | Parameterised_thunk { params; thunk; _ } ->
    Test.create_parameterised
      ~name
      ~test_name
      ~file_name
      ?module_name
      ~args:params
      ~key
      (fun len -> Staged.stage (thunk len).uncurried)
;;

let pattern_to_predicate s =
  let regexp = Re.Perl.compile_pat s in
  fun name -> Re.execp regexp name
;;

let get_matching_tests ~libname patterns =
  let tbl = Int.Table.create () in
  let entries = Ppx_bench_lib.Benchmark_accumulator.lookup_lib ~libname in
  let entries =
    match patterns with
    (* if no regexes are specified, run all entries *)
    | [] -> entries
    | _ :: _ ->
      let filter =
        let preds = List.map patterns ~f:pattern_to_predicate in
        fun name -> List.exists preds ~f:(fun pred -> pred name)
      in
      (* for parameterized tests we must include the param in the filter (so we can filter
         to "size:1000" or what have you.) *)
      List.filter_map entries ~f:(fun entry ->
        let name = make_benchmark_name entry in
        match entry.Entry.test_spec with
        | Regular_thunk _ -> Option.some_if (filter name) entry
        | Parameterised_thunk { params; arg_name; thunk } ->
          let params =
            List.filter params ~f:(fun (p, _) ->
              let name = name ^ ":" ^ p in
              filter name)
          in
          (match params with
           | [] -> None
           | _ :: _ ->
             Some
               (Entry.with_test_spec
                  entry
                  (Parameterised_thunk { params; arg_name; thunk }))))
  in
  let tests =
    List.map entries ~f:(fun entry ->
      let key = entry.Entry.unique_id in
      Hashtbl.add_exn tbl ~key ~data:entry;
      entry_to_bench_test entry ~key)
  in
  tbl, tests
;;

let x_library_inlining_warning ~run_without_inlining ~display_config =
  if not Version_util.x_library_inlining
  then (
    "Warning: X_LIBRARY_INLINING is not set to true, benchmarks may be inaccurate."
    |> Display_config.print_warning display_config;
    if not run_without_inlining
    then
      failwith
        "If you would like to run benchmarks, and are ok with getting inaccurate results \
         due to lack of cross library inlining, use the \
         -run-without-cross-library-inlining flag.")
;;

let run
  ~libname
  ~matching
  ~run_without_inlining
  ~list_only
  ~display_config
  ~bench
  what_to_do
  =
  let should_run =
    Option.value_map
      ~default:false
      ~f:(String.equal "TRUE")
      (Sys.getenv "BENCHMARKS_RUNNER")
  in
  if not should_run
  then failwith "Don't run directly, run using the inline_benchmarks_runner script.";
  match list_only, what_to_do with
  | true, _ ->
    let _, tests = get_matching_tests ~libname matching in
    List.iter tests ~f:(fun test ->
      List.iter (Core_bench_internals.Test.tests test) ~f:(fun { name; _ } ->
        print_endline name))
  | false, `Run (save_to_file, run_config) ->
    x_library_inlining_warning ~run_without_inlining ~display_config;
    let _tbl, tests = get_matching_tests ~libname matching in
    if List.is_empty tests
    then Display_config.print_warning display_config "No benchmarks to run!"
    else bench ~run_config ?save_to_file tests
  | false, `From_file _filenames ->
    failwith "Loading saved files is not supported for inline executables."
;;

let matching_param =
  let open Command.Param in
  flag "matching" (listed string) ~doc:"REGEX Include only benchmarks matching the REGEX."
;;

let command ~filename_argtype ~libname ~bench =
  let open Command.Let_syntax in
  Core_bench_internals.Bench_command.make_ext
    ~filename_argtype
    ~summary:(sprintf "run inline benchmarks of %s now." libname)
    [%map_open
      let matching = matching_param
      and no_sexp =
        flag
          "no-sexp"
          no_arg
          ~doc:" [CURRENTLY IGNORED] Do not generate a benchmarks.sexp file (quicker)."
      and run_without_inlining =
        flag
          "run-without-cross-library-inlining"
          no_arg
          ~doc:" Run benchmarks even when compiled with X_LIBRARY_INLINING=false."
      and list_only =
        flag "list-only" no_arg ~doc:" List benchmarks and exit without running them."
      in
      fun (analysis_configs, display_config, what_to_do) ->
        ignore (no_sexp : bool);
        run
          ~libname
          ~matching
          ~run_without_inlining
          ~list_only
          ~display_config
          ~bench:(bench ~analysis_configs ~display_config)
          what_to_do]
;;
