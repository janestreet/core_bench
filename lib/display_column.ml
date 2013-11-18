(* open Core.Std *)

type t =
  [ `Name
  | `Speedup
  | `Percentage
  | `Samples
  ] with sexp
