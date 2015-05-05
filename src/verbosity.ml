(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core.Std

let verbosity : [`High | `Low] ref = ref `Low

let set_verbosity v =
  verbosity := v

let print_high s = match !verbosity with
  | `High -> printf s
  | `Low -> Printf.ifprintf stdout s
