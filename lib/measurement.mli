open Core.Std


val run_benchmarks
  :  ?verbosity:[ `High | `Low ]
  -> ?no_compactions:bool
  -> ?save_sample_data:bool
  -> ?time_quota:Time.Span.t
  -> ?sampling_type:[`Geometric of float | `Linear of int]
  -> ?stabilize_gc_between_runs:bool
  -> ?fork_each_benchmark:bool
  -> Test.Basic_test.t list
  -> (Test.Basic_test.t * Test_metrics.t array * int) list


