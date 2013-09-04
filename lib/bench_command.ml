(* This module makes the command line interface for bench. *)
open Core.Std

module Ascii_table = Textutils.Ascii_table
module Variable    = Test_metrics.Variable



(* This type is written out here so that if [bench.ml] ever changes its interface, this
   will throw a type error. If you edit the type below, also edit the command generation
   code below to take any relevant additional arguments. *)
type callback_bench
  =  ?limit_width_to:int
  -> ?columns:[ Column.t | `If_not_empty of Column.t ] list
  -> ?display:Ascii_table.Display.t
  -> ?ascii_table:bool
  -> ?ci_absolute:bool
  -> ?predictors:Test_metrics.Variable.t list
  -> ?verbosity:[ `High | `Low ]
  -> ?no_compactions:bool
  -> ?save_sample_data:bool
  -> ?time_quota:Time.Span.t
  -> ?sampling_type:[`Geometric of float | `Linear of int]
  -> ?stabilize_gc_between_runs:bool
  -> ?fork_each_benchmark:bool
  -> Test.t list
  -> unit

type callback_analyze
  =  ?limit_width_to:int
  -> ?columns:[ Column.t | `If_not_empty of Column.t ] list
  -> ?display:Ascii_table.Display.t
  -> ?ascii_table:bool
  -> ?ci_absolute:bool
  -> ?predictors:Test_metrics.Variable.t list
  -> saved_files:string list
  -> unit


let readme () = sprintf "\
Columns that can be specified are:
\t%s

R^2 is the fraction of the variance of the responder (such as runtime)
that is accounted for by the predictors (such as number of runs).
More informally, it describes how good a fit we're getting, with
R^2 = 1 indicating a perfect fit and R^2 = 0 indicating a horrible
fit.  Because we expect runtime to be very highly correlated with our
predictors, values very close to 1 are typical; a value less than 0.99
should cause some suspicion, and a value less than 0.9 probably
indicates either a shortage of data or that the data is erroneous or
peculiar in some way.
Also see: http://en.wikipedia.org/wiki/Coefficient_of_determination

GC stats indicate how many collections or compactions happen per 1000
runs of the benchmarked function.

The following columns will be displayed by default:
\t%s

By default, columns that have no values are suppressed. To force
displaying empty columns, prefix the column name with a '+'.

Experimental feature: Internally, the library does a linear
regression between the time taken as the predicted value and the
number of runs as the predictor.  This can be changed to include
one or more of the additional predictors below, using the
flag called \"-predictors\":
  m : the number of minor collections
  c : the number of compactions
"

  Column.column_description_table
  (String.concat ~sep:" " Defaults.columns_as_string)

let make (bench : callback_bench) (analyze : callback_analyze) (tests : Test.t list) =
  Command.basic
    ~summary:(
      sprintf "Benchmark for %s"
        (String.concat ~sep:", "
           (List.map tests ~f:(fun test ->
             let len = List.length (Test.tests test) in
             if len = 1
             then Test.name test
             else sprintf "%s (%d tests)" (Test.name test) len))))
    ~readme
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
      +> flag "-predictors" (optional_with_default Defaults.predictors_string string)
        ~doc:"m,c Include additional predictors in regression (see help)."
      +> flag "-quota" (optional_with_default Defaults.time_quota_float float)
        ~doc:(sprintf "SECS Time quota allowed per test (default %s)."
                (Time.Span.to_string Defaults.time_quota))
      +> flag "-no-compactions" no_arg ~doc:" Disable GC compactions."
      +> flag "-fork" no_arg ~doc:" Fork and run each benchmark in separate child-process"
      ++ step (fun m linear geometric ->
        let sampling_type =
          match linear, geometric with
          | None, None -> `Geometric Defaults.geometric_scale
          | None, Some s -> `Geometric s
          | Some k, None -> `Linear k
          | Some _, Some _ ->
            failwith "Cannot specify both -linear and -geometric"
        in
        m ~sampling_type)
      +> flag "-linear" (optional int)
        ~doc:"INCREMENT Use linear sampling to explore number of runs, example 1."
      +> flag "-geometric" (optional float)
        ~doc:(sprintf "SCALE Use geometric sampling. (default %0.2f)"
                Defaults.geometric_scale)
      +> flag "-save" no_arg ~doc:" Save benchmark data to <test name>.txt files."
      +> flag "-ascii" no_arg ~doc:" Display data in simple ascii based tables."
      +> flag "-ci-absolute" no_arg ~doc:" Display 95% confidence interval in absolute numbers"
      +> flag "-stabilize-gc" no_arg ~doc:" Stabilize GC between each sample capture."
      +> flag "-clear-columns" no_arg ~doc:" Don't display default columns. Only show \
        user specified ones."
      +> flag "-load" (listed file) ~doc:"FILE Analyze previously saved data files and
        don't run tests. [-load] can be specified multiple times."
      +> anon (sequence ("COLUMN" %: Column.arg))
    )
    (fun limit_width_to display_style verbosity predictors time_quota no_compactions
      fork_each_benchmark ~sampling_type save_sample_data minimal_tables ci_absolute
      stabilize_gc_between_runs clear_columns analyze_files anon_columns () ->
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
        let predictors =
          let letters = String.split predictors ~on:',' in
          List.fold_left letters ~init:[] ~f:(fun list letter ->
            match letter with
            | "" -> list
            | letter ->
              let predictor = Variable.of_string letter in
              predictor::list
          )
        in
        (* If the user specifies "r" as one of the predictors, do not add it twice *)
        let predictors = List.dedup(Defaults.predictors @ predictors) in
        let predictors = List.rev predictors in
        let columns =
          if clear_columns
          then []
          else Defaults.columns
        in
        let columns = columns @ anon_columns in
        match analyze_files with
        | [] ->
          bench
            ~limit_width_to
            ~columns
            ~display
            ~ascii_table
            ~ci_absolute
            ~verbosity
            ~time_quota:(Time.Span.of_float time_quota)
            ~sampling_type
            ~save_sample_data
            ~stabilize_gc_between_runs
            ~no_compactions
            ~fork_each_benchmark
            ~predictors
            tests
        | saved_files ->
          analyze
            ~limit_width_to
            ~columns
            ~display
            ~ascii_table
            ~ci_absolute
            ~predictors
            ~saved_files
    )


