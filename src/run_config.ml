(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core.Std

type t = {
  verbosity:[ `High | `Low ];
  no_compactions:bool;
  time_quota:Time.Span.t;
  sampling_type:[`Geometric of float | `Linear of int];
  stabilize_gc_between_runs:bool;
  fork_each_benchmark:bool;
} [@@deriving fields, sexp]

let create
    ?(verbosity=`Low)
    ?(no_compactions=Defaults.no_compactions)
    ?(time_quota=Defaults.time_quota)
    ?(sampling_type=`Geometric Defaults.geometric_scale)
    ?(stabilize_gc_between_runs=Defaults.stabilize_gc_between_runs)
    ?(fork_each_benchmark=Defaults.fork_each_benchmark)
    ()
    =
  { verbosity;
    no_compactions;
    time_quota;
    sampling_type;
    stabilize_gc_between_runs;
    fork_each_benchmark;
  }
