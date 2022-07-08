open Core

type t =
  | Span of Time_float.Span.t
  | Num_calls of int
[@@deriving sexp]

let int_of_string_with_scientific_notation string =
  try Int.of_string string with
  | _ ->
    let float = Float.of_string string in
    let int = Float.to_int float in
    if Float.equal float (Float.of_int int)
    then int
    else raise_s ([%sexp_of: string * float] ("fractional values not allowed", float))
;;

let of_string string =
  match String.chop_suffix string ~suffix:"x" with
  | Some prefix -> Num_calls (int_of_string_with_scientific_notation prefix)
  | None ->
    (match Time_float.Span.of_string string with
     | span -> Span span
     | exception _ -> Span (Time_float.Span.of_sec (Float.of_string string)))
;;

let to_string = function
  | Span s -> Time_float.Span.to_string s
  | Num_calls n -> sprintf "%ix" n
;;

let arg_type = Command.Param.Arg_type.create of_string

let fulfilled t ~start ~num_calls =
  match t with
  | Span span ->
    let now = Time_float.now () in
    Time_float.Span.( >= ) (Time_float.diff now start) span
  | Num_calls n -> num_calls >= n
;;

let max_count = function
  | Span _ -> Int.max_value
  | Num_calls n -> n
;;

let scale_int t factor =
  match t with
  | Span span -> Span (Time_float.Span.scale span (Float.of_int factor))
  | Num_calls num_calls -> Num_calls (num_calls * factor)
;;
