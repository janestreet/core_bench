(** A module internal to [Core_bench]. Please look at {!Bench}.

   Contains the measurements of several runs of one benchmark. *)
open! Core.Std

type t [@@deriving sexp]

val name         : t -> string
val largest_run  : t -> int
val sample_count : t -> int
val samples      : t -> Measurement_sample.t array

val create
  :  name:string
  -> largest_run:int
  -> sample_count:int
  -> samples:Measurement_sample.t array
  -> t

val save : t -> filename:string -> unit
val load : filename:string -> t

