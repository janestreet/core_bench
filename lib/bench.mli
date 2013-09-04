
open Core.Std

(** Simple example of using bench:
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

    For default values of all the optional arguments, see the module called [Defaults] in
    bench_main.ml.

    For some notes on the design space of bench and TODO items, see:
    http://docs/programming/performance/benchmarking.html
*)

module Test : sig
  type t
  val create : name:string -> (unit -> unit) -> t
  val create_indexed :
    name:string -> args:int list -> (int -> (unit -> unit) Staged.t) -> t
end

module Column : sig
  (** The documentation for this type is in the implementation, as documentation for the
      command line flags created by [make_command]. *)
  type t =
    [ `Name
    | `Cycles
    | `Nanos
    | `Confidence
    | `Allocated
    | `Percentage
    | `GC
    | `Speedup
    | `Samples
    ] with sexp
end


(* Note: Major_collections are not very good predictors for couple of reasons:

   (1) They are not discrete events, but happen per minor GC. Consequently the time at
   which the major GC counter is incremented does not correspond to a latency event in the
   runtime.

   (2) Also, they are strongly correlated with compactions and compactions are discrete
   events. *)
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

(** The documentation for all of these arguments is in the implementation, as
    documentation for the command line flags created by [make_command]. *)
val bench
  :  ?limit_width_to:int
  -> ?columns:[ Column.t | `If_not_empty of Column.t ] list
  -> ?display:Textutils.Ascii_table.Display.t
  -> ?ascii_table:bool
  -> ?ci_absolute:bool
  -> ?predictors:Variable.t list
  -> ?verbosity:[ `High | `Low ]
  -> ?no_compactions:bool
  -> ?save_sample_data:bool
  -> ?time_quota:Time.Span.t
  -> ?sampling_type:[`Geometric of float | `Linear of int]
  -> ?stabilize_gc_between_runs:bool
  -> ?fork_each_benchmark:bool
  -> Test.t list
  -> unit

val analyze
  :  ?limit_width_to:int
  -> ?columns:[ Column.t | `If_not_empty of Column.t ] list
  -> ?display:Textutils.Ascii_table.Display.t
  -> ?ascii_table:bool
  -> ?ci_absolute:bool
  -> ?predictors:Variable.t list
  -> saved_files:string list
  -> unit

val make_command : Test.t list -> Command.t
