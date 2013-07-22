
open Core.Std

(** Simple example of using bench:
    [{
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
    | `Nominal_cycles
    | `Nominal_nanos
    | `Bootstrap_cycles
    | `Bootstrap_nanos
    | `Allocated
    | `Percentage
    | `GC
    | `Speedup
    | `Samples
    ] with sexp
end

(** The documentation for all of these arguments is in the implementation, as
    documentation for the command line flags created by [make_command]. *)
val bench
  :  ?limit_width_to:int
  -> ?columns:[ Column.t | `If_not_empty of Column.t ] list
  -> ?display:Textutils.Ascii_table.Display.t
  -> ?verbosity:[ `High | `Low ]
  -> ?no_compactions:bool
  -> ?save_sample_data:bool
  -> ?time_quota:Time.Span.t
  -> ?sampling_type:[`Geometric of float | `Linear of int]
  -> ?stabilize_gc_between_runs:bool
  -> Test.t list
  -> unit

val make_command : Test.t list -> Command.t
