(* This module makes the command line interface for bench. *)
open Core.Std

module Ascii_table = Textutils.Ascii_table

type callback_bench
  = ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> ?save_to_file:(Measurement.t -> string)
  -> Test.t list
  -> unit

type callback_load_analyze_and_display
  = filenames:string list
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> unit
  -> unit

let spec () =
  Command.Spec.(
    (* flags *)
    empty
    +> flag "-width" (optional_with_default Defaults.limit_width_to int)
         ~doc:(sprintf "WIDTH width limit on column display (default %d)."
                 Defaults.limit_width_to)
    +> flag "-display" (optional_with_default Defaults.display_as_string string)
         ~doc:(sprintf "STYLE Table style (short, tall, line, blank or column). Default %s."
                 Defaults.display_as_string)
    +> flag "-v" no_arg ~doc:" High verbosity level."
    +> flag "-quota" (optional_with_default Defaults.time_quota_float float)
         ~doc:(sprintf "SECS Time quota allowed per test (default %s)."
                 (Time.Span.to_string Defaults.time_quota))
    +> flag "-fork" no_arg ~doc:" Fork and run each benchmark in separate child-process"
    +> flag "-all-values" no_arg ~doc:" Show all column values, including very small ones."
    +> flag "-no-compactions" no_arg ~doc:" Disable GC compactions."
    ++ step (fun m x -> m ~show_overheads:x)
    +> flag "-overheads" no_arg ~doc:" Show measurement overheads, when applicable."
    ++ step (fun m linear geometric ->
      let sampling_type =
        match linear, geometric with
        | None, None     -> `Geometric Defaults.geometric_scale
        | None, Some s   -> `Geometric s
        | Some k, None   -> `Linear k
        | Some _, Some _ ->
          failwith "Cannot specify both -linear and -geometric"
      in
      m ~sampling_type)
    +> flag "-linear" (optional int)
         ~doc:"INCREMENT Use linear sampling to explore number of runs, example 1."
    +> flag "-geometric" (optional float)
         ~doc:(sprintf "SCALE Use geometric sampling. (default %.2f)"
                 Defaults.geometric_scale)
    +> flag "-save" no_arg ~doc:" Save benchmark data to <test name>.txt files."
    +> flag "-ascii" no_arg ~doc:" Display data in simple ascii based tables."
    +> flag "-reduced-bootstrap" no_arg ~doc:" Reduce the number of bootstrapping iterations"
    +> flag "-ci-absolute" no_arg ~doc:" Display 95% confidence interval in absolute numbers"
    +> flag "-stabilize-gc" no_arg ~doc:" Stabilize GC between each sample capture."
    +> flag "-clear-columns" no_arg ~doc:" Don't display default columns. Only show \
                                          user specified ones."
    +> flag "-load" (listed file) ~doc:"FILE Analyze previously saved data files and
        don't run tests. [-load] can be specified multiple times."
    +> flag "-regression" (listed string)
         ~doc:"REGR Specify additional regressions (See -? help). "
    +> anon (sequence ("COLUMN" %: Bench_command_column.arg))
  )


let sanitize_name str =
  String.map str ~f:(fun c ->
    if Char.is_alphanum c || String.mem "-_." c
    then c
    else '_')

let parse_commandline_args
      ~f
      limit_width_to
      display_style
      verbosity
      time_quota
      fork_each_benchmark
      show_all_values
      no_compactions
      ~show_overheads
      ~sampling_type
      save_sample_data
      minimal_tables
      reduced_bootstrap
      show_absolute_ci
      stabilize_gc_between_runs
      clear_columns
      analyze_files
      regressions
      anon_columns
  =
  let display = Defaults.string_to_display display_style in
  let display, ascii_table =
    if minimal_tables
    then Ascii_table.Display.column_titles, true
    else display, false
  in
  let verbosity =
    if verbosity
    then `High
    else `Low
  in
  let time_quota = Time.Span.of_float time_quota in
  let columns =
    if clear_columns
    then []
    else Defaults.command_columns
  in
  let columns = columns @ anon_columns in
  let analysis_configs, columns =
    let f =
      let open Bench_command_column in
      function
      | Analysis analysis -> `Fst analysis
      | Display_column col -> `Snd col
    in
    List.partition_map columns ~f
  in
  let analysis_configs = List.concat analysis_configs in
  let analysis_configs =
    let to_name i = sprintf " [%d]" (i+1) in
    analysis_configs @
    (List.mapi regressions
       ~f:(fun i reg ->
         let regression_name = to_name i in
         printf "Regression%s = %s\n%!" regression_name reg;
         Analysis_config.parse reg ~regression_name))
  in
  let analysis_configs =
    if reduced_bootstrap
    then List.map analysis_configs
           ~f:(Analysis_config.reduce_bootstrap
                 ~bootstrap_trials:Analysis_config.default_reduced_bootstrap_trials)
    else analysis_configs
  in
  let save =
    if save_sample_data then begin
      printf "Measurements will be saved.\n%!";
      let time_str = Time.format (Time.now ()) "%F-%R" ~zone:Time.Zone.local in
      Some (fun meas ->
        let name = Measurement.name meas in
        let fn = sprintf "%s-%s-%s.txt"
                   (sanitize_name name)
                   time_str
                   (Time.Span.to_string time_quota)
        in
        printf "Saving to: %s.\n%!" fn;
        fn)
    end
    else None
  in
  let run_config =
    Run_config.create
      ~verbosity
      ~time_quota
      ~sampling_type
      ~stabilize_gc_between_runs
      ~no_compactions
      ~fork_each_benchmark
      ()
  in
  let display_config =
    Display_config.create
      ~limit_width_to
      ~show_samples:(List.mem columns `Samples)
      ~show_percentage:(List.mem columns `Percentage)
      ~show_speedup:(List.mem columns `Speedup)
      ~show_all_values
      ~show_absolute_ci
      ~show_overheads
      ~display
      ~ascii_table
      ()
  in
  let configs =
    match analyze_files with
    | [] ->
      (analysis_configs, display_config, `Run (save, run_config))
    | filenames ->
      (analysis_configs, display_config, `From_file filenames)
  in
  f configs

let readme () = sprintf "\
Columns that can be specified are:
\t%s

Columns with no significant values will not be displayed. The
following columns will be displayed by default:
\t%s

Error Estimates
===============
To display error estimates, prefix the column name (or
regression) with a '+'. Example +time.

(1) R^2 is the fraction of the variance of the responder (such as
runtime) that is accounted for by the predictors (such as number of
runs).  More informally, it describes how good a fit we're getting,
with R^2 = 1 indicating a perfect fit and R^2 = 0 indicating a
horrible fit. Also see:
http://en.wikipedia.org/wiki/Coefficient_of_determination

(2) Bootstrapping is used to compute 95%% confidence intervals
for each estimate.

Because we expect runtime to be very highly correlated with number of
runs, values very close to 1 are typical; an R^2 value for 'time' that
is less than 0.99 should cause some suspicion, and a value less than
0.9 probably indicates either a shortage of data or that the data is
erroneous or peculiar in some way.

Specifying additional regressions
=================================
The builtin in columns encode common analysis that apply to most
functions. Bench allows the user to specify custom analysis to help
understand relationships specific to a particular function using the
flag \"-regression\" . It is worth noting that this feature requires
some understanding of both linear regression and how various quatities
relate to each other in the OCaml runtime.  To specify a regression
one must specify the responder variable and a command separated list
of predictor variables.

For example: +Time:Run,mjGC,Comp

which asks bench to estimate execution time using three predictors
namely the number of runs, major GCs and compaction stats and display
error estimates. Drop the prefix '+' to suppress error estimation. The
variables available for regression include:
\t%s
"
                  Bench_command_column.column_description_table
                  (String.concat ~sep:" " Defaults.columns_as_string)
                  (Variable.summarize ())

let make_ext
      (type a)
      ~(summary : string)
      ~(extra_spec : (a, unit -> unit) Command.Spec.t)
      ~(f :
          (Analysis_config.t list * Display_config.t *
           [ `From_file of string list
           | `Run of (Measurement.t -> string) option * Run_config.t ])
        -> a)
  : Command.t
  =
    Command.basic
      ~readme
      ~summary
      Command.Spec.(spec () ++ extra_spec)
      (parse_commandline_args ~f)


let make
      ~(bench : callback_bench)
      ~(analyze : callback_load_analyze_and_display)
      ~(tests : Test.t list)
  =
  make_ext
    ~summary:(
      sprintf "Benchmark for %s"
        (String.concat ~sep:", "
           (List.map tests ~f:(fun test ->
              let len = List.length (Test.tests test) in
              if len = 1
              then Test.name test
              else sprintf "%s (%d tests)" (Test.name test) len))))
    ~extra_spec:Command.Spec.empty
    ~f:(fun args () ->
      match args with
      | (analysis_configs, display_config, `Run (save_to_file, run_config)) ->
        bench
          ~analysis_configs
          ~display_config
          ~run_config
          ?save_to_file
          tests
      | (analysis_configs, display_config, `From_file filenames) ->
        analyze
          ~analysis_configs
          ~display_config
          ~filenames
          ())
