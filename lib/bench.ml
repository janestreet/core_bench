
open Core.Std

module Variable = Test_metrics.Variable
module Column = Column
module Test = Test

let bench
    (* printing parameters *)
    ?(limit_width_to=Defaults.limit_width_to)
    ?(columns=Defaults.columns)
    ?(display=Defaults.display)
    ?(ascii_table=false)
    ?(ci_absolute=false)
    (* benchmarking parameters *)
    ?verbosity
    ?no_compactions
    ?save_sample_data
    ?time_quota
    ?sampling_type
    ?stabilize_gc_between_runs
    ?(predictors=Defaults.predictors)
    ?fork_each_benchmark
    tests =
  let tests = List.concat (List.map ~f:Test.tests tests) in
  Bench_table.print ~limit_width_to ~columns ~display ~ascii_table ~ci_absolute ~predictors
    (Measurement.run_benchmarks
       ?verbosity
       ?no_compactions
       ?save_sample_data
       ?time_quota
       ?sampling_type
       ?stabilize_gc_between_runs
       ?fork_each_benchmark
       tests)
;;

let make_command tests = Bench_command.make bench tests




(* Older CRs are moved down here: *)

