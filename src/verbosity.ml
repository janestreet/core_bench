(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core

let verbosity : [`High | `Low | `Suppress_warnings_and_errors] ref = ref `Low

let set_verbosity v =
  verbosity := v

let print_high s = match !verbosity with
  | `High -> printf s
  | `Low | `Suppress_warnings_and_errors -> Printf.ifprintf stdout s

let print_low s = match !verbosity with
  | `High | `Low -> printf s
  | `Suppress_warnings_and_errors -> Printf.ifprintf stdout s

let should_suppress () =
  match !verbosity with
  | `Suppress_warnings_and_errors -> true
  | `High | `Low -> false
