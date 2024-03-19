(* This module makes the command line interface for bench. *)
open Core
open Core_bench_internals
module Time = Time_float_unix

type callback_bench =
  ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> ?save_to_file:(Measurement.t -> string)
  -> ?libname:string
  -> Test.t list
  -> unit

type callback_load_analyze_and_display =
  filenames:string list
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> unit
  -> unit

let wrapper_param =
  let%map_open.Command () = return ()
  and display_config = Display_config.param
  and quota =
    flag
      "-quota"
      (optional_with_default Defaults.quota Quota.arg_type)
      ~doc:
        (sprintf
           "<INT>x|<SPAN> Quota allowed per test. May be a number of runs (e.g. 1000x or \
            1e6x) or a time span (e.g. 10s or 500ms). Default %s. [pacs]"
           (Quota.to_string Defaults.quota))
  and fork_each_benchmark =
    flag
      "-fork"
      no_arg
      ~doc:" Fork and run each benchmark in separate child-process. [pacs]"
  and no_compactions =
    flag "-no-compactions" no_arg ~doc:" Disable GC compactions. [pacs]"
  and sampling_type =
    choose_one
      ~if_nothing_chosen:(Default_to (`Geometric Defaults.geometric_scale))
      [ flag
          "-linear"
          (optional int)
          ~doc:
            "INCREMENT Use linear sampling to explore number of runs, example 1. [pacs]"
        |> map ~f:(Option.map ~f:(fun k -> `Linear k))
      ; flag
          "-geometric"
          (optional float)
          ~doc:
            (sprintf
               "SCALE Use geometric sampling (default %.2f). [pacs]"
               Defaults.geometric_scale)
        |> map ~f:(Option.map ~f:(fun s -> `Geometric s))
      ]
  and save_sample_data =
    flag "-save" no_arg ~doc:" Save benchmark data to <test name>.txt files. [pacs]"
  and reduced_bootstrap =
    flag
      "-reduced-bootstrap"
      no_arg
      ~doc:" Reduce the number of bootstrapping iterations. [pacs]"
  and stabilize_gc_between_runs =
    flag "-stabilize-gc" no_arg ~doc:" Stabilize GC between each sample capture. [pacs]"
  and analyze_files =
    flag
      "-load"
      (listed Filename_unix.arg_type)
      ~doc:
        "FILE Analyze previously saved data files and don't run tests. [-load] can be \
         specified multiple times. [pacs]"
  and thin_overhead =
    flag
      "-thin-overhead"
      (optional float)
      ~doc:
        "INT If given, just run the test function(s) N times; skip measurements and \
         regressions. Float lexemes like \"1e6\" are allowed. [....]"
    |> map ~f:(Option.map ~f:Float.to_int)
  in
  fun ~main () ->
    let sanitize_name str =
      String.map str ~f:(fun c ->
        if Char.is_alphanum c || String.mem "-_." c then c else '_')
    in
    let analysis_configs = Display_config.analysis_configs display_config in
    let analysis_configs =
      if reduced_bootstrap
      then
        List.map
          analysis_configs
          ~f:
            (Analysis_config.reduce_bootstrap
               ~bootstrap_trials:Analysis_config.default_reduced_bootstrap_trials)
      else analysis_configs
    in
    let save =
      if save_sample_data
      then (
        printf "Measurements will be saved.\n%!";
        let time_str = Time.format (Time.now ()) "%F-%R" ~zone:(force Time.Zone.local) in
        Some
          (fun meas ->
            let name = Measurement.name meas in
            let fn =
              sprintf "%s-%s-%s.txt" (sanitize_name name) time_str (Quota.to_string quota)
            in
            printf "Saving to: %s.\n%!" fn;
            fn))
      else None
    in
    let verbosity = Display_config.verbosity display_config in
    let run_config =
      Run_config.create
        ~verbosity
        ~quota
        ~sampling_type
        ~stabilize_gc_between_runs
        ~no_compactions
        ~fork_each_benchmark
        ?thin_overhead
        ()
    in
    let configs =
      match analyze_files with
      | [] -> analysis_configs, display_config, `Run (save, run_config)
      | filenames -> analysis_configs, display_config, `From_file filenames
    in
    main configs
;;

let readme () =
  sprintf
    "Columns that can be specified are:\n\
     \t%s\n\n\
     Columns with no significant values will not be displayed. The\n\
     following columns will be displayed by default:\n\
     \t%s\n\n\
     Error Estimates\n\
     ===============\n\
     To display error estimates, prefix the column name (or\n\
     regression) with a '+'. Example +time.\n\n\
     (1) R^2 is the fraction of the variance of the responder (such as\n\
     runtime) that is accounted for by the predictors (such as number of\n\
     runs).  More informally, it describes how good a fit we're getting,\n\
     with R^2 = 1 indicating a perfect fit and R^2 = 0 indicating a\n\
     horrible fit. Also see:\n\
     http://en.wikipedia.org/wiki/Coefficient_of_determination\n\n\
     (2) Bootstrapping is used to compute 95%% confidence intervals\n\
     for each estimate.\n\n\
     Because we expect runtime to be very highly correlated with number of\n\
     runs, values very close to 1 are typical; an R^2 value for 'time' that\n\
     is less than 0.99 should cause some suspicion, and a value less than\n\
     0.9 probably indicates either a shortage of data or that the data is\n\
     erroneous or peculiar in some way.\n\n\
     Specifying additional regressions\n\
     =================================\n\
     The builtin in columns encode common analysis that apply to most\n\
     functions. Bench allows the user to specify custom analysis to help\n\
     understand relationships specific to a particular function using the\n\
     flag \"-regression\" . It is worth noting that this feature requires\n\
     some understanding of both linear regression and how various quatities\n\
     relate to each other in the OCaml runtime.  To specify a regression\n\
     one must specify the responder variable and a comma-separated list of\n\
     predictor variables.\n\n\
     For example: +Time:Run,mjGC,Comp\n\n\
     which asks bench to estimate execution time using three predictors\n\
     namely the number of runs, major GCs and compaction stats and display\n\
     error estimates. Drop the prefix '+' to suppress error estimation. The\n\
     variables available for regression include:\n\
     \t%s\n\n\
     Output formats\n\
     ==============\n\
     Benchmarks can be displayed in one of four formats:\n\n\
    \  (default) Pretty table\n\
    \  [-ascii]  Plain ASCII table\n\
    \  [-csv]    CSV\n\
    \  [-sexp]   Sexp\n\n\
     Some of the flags that follow are only compatible with a subset of\n\
     these display formats. Each flag in the help text below lists the\n\
     displays with which it can be used; we represent these formats with\n\
     the letters 'p', 'a', 'c', and 's', respectively. For example, a flag\n\
     ending with \"[pa..]\" can be used with pretty tables and ASCII\n\
     tables, but not with CSV or sexp output."
    Bench_command_column.column_description_table
    (String.concat ~sep:" " Defaults.columns_as_string)
    (Variable.summarize ())
;;

let make_ext ~summary main_param =
  let open Command.Let_syntax in
  Command.basic
    ~readme
    ~summary
    (let%map_open () = return ()
     and wrapper = wrapper_param
     and main = main_param in
     wrapper ~main)
;;

let make
  ~(bench : callback_bench)
  ~(analyze : callback_load_analyze_and_display)
  ~(tests : Test.t list)
  =
  make_ext
    ~summary:
      (sprintf
         "Benchmark for %s"
         (String.concat
            ~sep:", "
            (List.map tests ~f:(fun test ->
               let len = List.length (Test.tests test) in
               if len = 1
               then Test.name test
               else sprintf "%s (%d tests)" (Test.name test) len))))
    (Command.Param.return (fun args ->
       match args with
       | analysis_configs, display_config, `Run (save_to_file, run_config) ->
         bench ~analysis_configs ~display_config ~run_config ?save_to_file tests
       | analysis_configs, display_config, `From_file filenames ->
         analyze ~analysis_configs ~display_config ~filenames ()))
;;
