(** [Core_bench] is a micro-benchmarking library for OCaml that can measure execution
    costs of operations that take 1ns to about 100ms. [Core_bench] tries to measure
    execution costs of such short-lived computations precisely while trying to account for
    delayed GC costs and noise introduced by other activity on the system.

    The easiest way to get started is using an example:
    {[
      open! Core.Std
      open Core_bench.Std

      let () =
        Random.self_init ();
        let x = Random.float 10.0 in
        let y = Random.float 10.0 in
        Command.run (Bench.make_command [
          Bench.Test.create ~name:"Float add" (fun () ->
            ignore (x +. y));
          Bench.Test.create ~name:"Float mul" (fun () ->
            ignore (x *. y));
          Bench.Test.create ~name:"Float div" (fun () ->
            ignore (x /. y));
        ])
    ]}

    When compiled this gives you an executable:
    {[
      $ ./z.exe -ascii
      Estimated testing time 30s (3 benchmarks x 10s). Change using -quota SECS.

        Name        Time/Run   mWd/Run   Percentage
       ----------- ---------- --------- ------------
        Float add     2.50ns     2.00w       41.70%
        Float mul     2.55ns     2.00w       42.52%
        Float div     5.99ns     2.00w      100.00%
    ]}

    If any of the functions resulted in allocation on the major heap (mjWd) or promotions
    (Prom), columns corresponding to those would be automatically displayed. Columns that
    do not have significant values are not displayed by default. The most common options
    one would want to change are the `-q` flag which controls the time quota for testing
    and enabling/disabling specific columns. For example:

    {[
       $ ./z.exe -ascii -q 1 cycles
       Estimated testing time 3s (3 benchmarks x 1s). Change using -quota SECS.

         Name        Time/Run   Cycls/Run   mWd/Run   Percentage
        ----------- ---------- ----------- --------- ------------
         Float add     2.50ns       8.49c     2.00w       41.78%
         Float mul     2.77ns       9.40c     2.00w       46.29%
         Float div     5.99ns      20.31c     2.00w      100.00%
    ]}

    If you drop the `-ascii` flag, the output table uses extended Ascii characters. These
    display well on most modern terminals, but not on ocamldoc.

    The simplest benchmark specification is just a [unit -> unit] thunk and a name:
    {[
      Bench.Test.create ~name:"Float add" (fun () -> ignore (x +. y));
    ]}

    One can also create indexed benchmarks, which can be helpful in understanding
    non-linearities in the execution profiles of functions. For example:
    {[
      open! Core.Std open Core_bench.Std

      let () =
       Command.run (Bench.make_command [
         Bench.Test.create_indexed
           ~name:"Array.create"
           ~args:[1; 10; 100; 200; 300; 400]
           (fun len ->
              Staged.stage (fun () -> ignore(Array.create ~len 0)));
       ])
    ]}

    this produces:
    {[
      $ ./z.exe -ascii -q 3
      Estimated testing time 18s (6 benchmarks x 3s). Change using -quota SECS.

        Name                 Time/Run   mWd/Run   mjWd/Run   Percentage
       ------------------ ------------ --------- ---------- ------------
        Array.create:1        27.23ns     2.00w                   1.08%
        Array.create:10       38.79ns    11.00w                   1.53%
        Array.create:100     124.05ns   101.00w                   4.91%
        Array.create:200     188.13ns   201.00w                   7.44%
        Array.create:300   1_887.20ns              301.00w       74.64%
        Array.create:400   2_528.43ns              401.00w      100.00%
    ]}

    Executables produced using [Bench.make_command] are self documenting (use the `-?`
    flag). The documentation in the executable also closely corresponds to the
    functionality exposed through the .mli and is a great way to interactively explore
    what the various options do.

    @see <https://github.com/janestreet/core_bench/wiki> Core_bench wiki
*)
open! Core.Std


(** [Test.t] are benchmarked by calls to bench. *)
module Test : sig
  type t = Test.t

  (** Creates a simple benchmark. Here the benchmark may return some ['a] which is then
      ignored. One should be careful when putting calls to [ignore] in benchmarks because
      OCaml versions 4.02 onwards can optimize away some ignored computations. *)
  val create : name:string -> ?key:int -> (unit -> 'a) -> t

  (** Creates a group of benchmarks indexed by a size.  Also see comment on [create]
      about the ['a] below. *)
  val create_indexed
    :  name:string
    -> args:int list
    -> ?key:int
    -> (int -> (unit -> 'a) Staged.t)
    -> t

  val create_group
    :  name:string
    -> t list
    -> t

  val name : t -> string
end

(** [Variable.t]s represent variables than can be used as predictors or the responder
    when specifying a regression. *)
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
  ] [@@deriving sexp]
end

(** [Run_config.t] specifies how a benchmark should be run. *)
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

(** [Display_config.t] specifies how the output tables should be formatted. *)
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
    -> ?show_overheads:bool
    -> unit
    -> t
end

(** Each [Analysis_config.t] specifies a regression run by [Core_bench]. This module also
    provides several typical regressions that one might want to run. *)
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

  (** For any analysis [t], return a new [t] that includes error estimation *)
  val with_error_estimation : ?bootstrap_trials:int -> t -> t

  (** [nanos_vs_runs] predicts nanos using runs. In this regression and all of the ones
      below, no error estimate is computed. *)
  val nanos_vs_runs : t

  (** [cycles_vs_runs]  predicts cycles using runs. *)
  val cycles_vs_runs : t

  (** [nanos ~predictors] estimates nanos using specified [predictors]. *)
  val nanos  : predictors:Variable.t list -> t

  (** similar to [nanos] *)
  val cycles : predictors:Variable.t list -> t

  (** [allocations_vs_runs] estimates minor allocations, major allocations and
      promotoions in terms of runs and overhead. *)
  val allocations_vs_runs : t list

  (** [allocations_vs_runs] estimates minor collections, major collections and
      compations in terms of runs. *)
  val gc_vs_runs          : t list

  (** A laundry list of several typical regressions: [nanos_vs_runs],
      [allocations_vs_runs] and [gc_vs_runs]. *)
  val default : t list
end

(** A [Measurement.t] represents the result of measuring execution of a [Test.t]. It is
    used as input for subsequent analysis. *)
module Measurement : sig
  type t [@@deriving sexp]

  val name : t -> string
  val save : t -> filename:string -> unit
  val load : filename:string -> t
end


(** [make_command tests] is the easiest way to generate a command-line program that runs a
    list of benchmarks. Here [tests : Test.t list] are the benchmarks that should be run.
    This returns a [Command.t] which provides a command-line interface for running the
    benchmarks. See notes above for an example.
*)
val make_command : Test.t list -> Command.t

(** [bench tests] will run, analyze and display the specified [tests]. Use this when one
    needs more control over the execution parameters that what is exposed through
    [make_command]. [bench] can also save the measurements of each test to the filename
    returned by [save_to_file]. *)
val bench
  :  ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> ?save_to_file:(Measurement.t -> string)
  -> Test.t list
  -> unit

(** [measure] is a fragment of the functionality of [bench]. [measure tests] will run
    the specified [tests] and return the resulting measurement results. *)
val measure
  :  ?run_config:Run_config.t
  -> Test.t list
  -> Measurement.t list

(** [analyze] is a fragment of the functionality of [bench]. [analyze ~analysis_configs m]
    will analyze the measurement [m] using the regressions specified. *)
val analyze
  :  ?analysis_configs:Analysis_config.t list
  -> Measurement.t
  -> Analysis_result.t Or_error.t

(** [display] is a fragment of the functionality of [bench]. [display results] will
    display a tabular summary of [results] on the terminal. *)
val display
  :  ?display_config:Display_config.t
  -> Analysis_result.t list
  -> unit

(** [make_command_ext] is useful for creating [Command.t]s that have command line flags in
    addition to those provided by [make_command]. *)
val make_command_ext
  :  summary:string
  -> extra_spec:('a, unit -> unit) Core.Std.Command.Spec.t
  -> f:(Analysis_config.t list * Display_config.t *
        [ `From_file of string list
        | `Run of (Measurement.t -> string) option * Run_config.t ]
        -> 'a)
  -> Command.t
