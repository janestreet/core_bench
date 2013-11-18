open Core.Std

(** Bench is a micro-benchmarking library that attempts to measure the execution time,
    allocation, GC effects etc of a function. Here is a simple example of using bench:

    [{
      open Core.Std
      open Core_bench.Std

      let main () =
        let t1 =
          Bench.Test.create ~name:"ArrayCreateInt1" (fun () ->
            ignore (Array.create ~len:200 0))
        in
        let t2 =
          Bench.Test.create ~name:"ArrayCreateInt2" (fun () ->
            ignore (Array.create ~len:300 0))
        in
        Command.run (Bench.make_command [t1; t2;])

      let () = main ()
    }]

    For some notes on the design space of bench and TODO items, see:
    http://docs/programming/performance/benchmarking.html
*)

(* [Test.t] are benchmarked by calls to bench. *)
module Test : sig
  type t

  val create : name:string -> ?key:int -> (unit -> unit) -> t

  val create_indexed
    :  name:string
    -> args:int list
    -> ?key:int
    -> (int -> (unit -> unit) Staged.t)
    -> t
end

module Variable : sig
  type t =
  [ `Runs
  | `Cycles
  | `Nanos
  | `Compactions
  | `Minor_collections
  | `Major_collections
  | `Promoted
  | `Minor_allocated
  | `Major_allocated
  | `One (* the "variable" that is always 1 *)
  ] with sexp
end

(** For default values of all the optional arguments, see the module called [Defaults] in
    bench_main.ml. *)
module Run_config : sig
  type t

  val create
    :  ?verbosity:[`High | `Low ]
    -> ?no_compactions:bool
    -> ?time_quota:Time.Span.t
    -> ?sampling_type:[`Geometric of float | `Linear of int]
    -> ?stabilize_gc_between_runs:bool
    -> ?fork_each_benchmark:bool
    -> unit
    -> t
end

module Display_config : sig

  type t

  val create
    :  ?don't_display_table:bool
    -> ?limit_width_to:int
    -> ?display:Textutils.Ascii_table.Display.t
    -> ?ascii_table:bool
    -> ?show_absolute_ci:bool
    -> ?show_percentage:bool
    -> ?show_speedup:bool
    -> ?show_samples:bool
    -> ?show_all_values:bool
    -> unit
    -> t
end

module Analysis_config : sig
  type t

  val create
    : responder:Variable.t
    -> predictors:Variable.t list
    -> ?bootstrap_trials:int (* default 0 *)
    -> ?r_square:bool (* default false. *)
    -> ?regression_name:string
    -> unit
    -> t

  (* predicts nanos using runs, no confidence tests. *)
  val nanos_vs_runs : t

  (* predicts cycles using runs, no confidence tests. *)
  val cycles_vs_runs : t

  (* predicts nanos/cycles using specified variables, no confidence tests. *)
  val nanos  : predictors:Variable.t list -> t
  val cycles : predictors:Variable.t list -> t

  (*  no confidence tests. *)
  val allocations_vs_runs : t list
  val gc_vs_runs          : t list

  (* This includes a lot of things. *)
  val default : t list
end

module Measurement : sig
  type t with sexp

  val name : t -> string
end

(** The documentation for all of these arguments is in the implementation, as
    documentation for the command line flags created by [make_command]. *)
val bench
  :  ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> Test.t list
  -> unit

val measure
  :  ?run_config:Run_config.t
  -> Test.t list
  -> Measurement.t list

val analyze
  :  ?analysis_configs:Analysis_config.t list
  -> Measurement.t
  -> Analysis_result.t Or_error.t

val display
  :  ?display_config:Display_config.t
  -> Analysis_result.t list
  -> unit

val save_measurements
  :  Measurement.t list
  -> to_filename:(Measurement.t -> string)
  -> unit

val load_measurements
  :  filenames:string list
  -> Measurement.t list

val make_command : Test.t list -> Command.t

val make_command_ext
  :  summary:string
  -> extra_spec:('a, unit -> unit) Core.Std.Command.Spec.t
  -> f:(Analysis_config.t list * Display_config.t *
        [ `From_file of string list
        | `Run of (Measurement.t -> string) option * Run_config.t ]
        -> 'a)
  -> Command.t
