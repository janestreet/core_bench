open Core.Std

(* numbers *)
let float_to_string n =
  if Float.abs n < 0.01
  then ""
  else if n < 1000.0
  then sprintf "%.2f" n
  else Int.to_string_hum (Float.iround_towards_zero_exn n)

let float_opt_to_string n_opt =
  Option.value_map n_opt ~default:"?" ~f:float_to_string

(* nanos *)
let float_to_ns_string n =
  Time.Span.to_string (Time.Span.of_ns n)

let float_opt_to_ns_string n_opt =
  Option.value_map n_opt ~default:"?" ~f:float_to_ns_string

(* tuples *)
let format_tuple (low, high) =
  sprintf "%s,%s" (float_to_string low) (float_to_string high)

let format_ns_tuple (low, high) =
  sprintf "%s,%s" (float_to_ns_string low) (float_to_ns_string high)

let format_plus_or_minus (plus, minus) =
  if Float.abs plus < 0.01
  && Float.abs minus < 0.01
  then ""
  else sprintf "+%s %s" (float_to_string plus) (float_to_string minus)

(* easier for function composition *)
let compose_float_to_string f x =
  float_to_string (f x)

let compose_float_opt_to_string f x =
  float_opt_to_string (f x)

let compose_float_to_ns_string f x =
  float_to_ns_string (f x)

let compose_float_opt_to_ns_string f x =
  float_opt_to_ns_string (f x)

