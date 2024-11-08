(** A module internal to [Core_bench]. Please look at {!Bench}.

    Abstract set of variables used for specifying regressions. *)
open! Core

type t =
  [ `One (* the "variable" that is always 1 *)
  | `Runs
  | `Cycles
  | `Nanos
  | `Minor_collections
  | `Major_collections
  | `Compactions
  | `Minor_allocated
  | `Major_allocated
  | `Promoted
  | `Extra of string
  ]
[@@deriving sexp, hash]

val get_units : t -> Display_units.t
val make_col_name : t -> t -> string
val of_short_string : string -> t
val summarize : unit -> string
val to_short_string : t -> string
val to_string : t -> string
