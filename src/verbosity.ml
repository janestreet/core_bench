(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core.Std

let verbosity : [`High | `Low | `Suppress] ref = ref `Low

let set_verbosity v =
  verbosity := v

let print_high s = match !verbosity with
  | `High -> printf s
  | `Low | `Suppress -> Printf.ifprintf stdout s

let print_low s = match !verbosity with
  | `High | `Low -> printf s
  | `Suppress -> Printf.ifprintf stdout s
