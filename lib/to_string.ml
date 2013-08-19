open Core.Std

(* numbers *)
let float_to_string n =
  if Float.abs n < 0.01
  then ""
  else if n < 100.0
  then sprintf "%.2f" n
  else Option.value_map (Float.iround_nearest n)
    ~default:"?"
    ~f:Int.to_string_hum

let float_to_perc_string n =
  if Float.abs n < 0.01
  then ""
  else (Option.value_map (Float.iround_nearest (n *. 100.))
          ~default:"?"
          ~f:Int.to_string_hum) ^ "%"

let float_to_ns_string n =
  if Float.abs n < 0.01
  then ""
  else if Float.abs n < 1000.
  then sprintf "%.2fns" n
  else
    let n = n /. 1000. in
    if Float.abs n < 1000.
    then sprintf "%.2fus" n
    else
      let n = n /. 1000. in
      if Float.abs n < 1000.
      then sprintf "%.2fms" n
      else
        Time.Span.to_string (Time.Span.of_ms n)

let float_to_string_cycles n =
  if Float.abs n < 0.1
  then ""
  else if Float.abs n < 1000.0
  then sprintf "%.1f" n
  else
    let n = n /. 1000.0 in
    if Float.abs n < 1000.0
    then sprintf "%.2fk" n
    else
      let n = n /. 1000.0 in
      if Float.abs n < 1000.0
      then sprintf "%.2fm" n
      else
        let n = n /. 1000.0 in
        if Float.abs n < 1000.0
        then sprintf "%.2fb" n
        else sprintf "%.2ft" (n /. 1000.)

let float_opt_to_string n_opt =
  Option.value_map n_opt ~default:"?" ~f:float_to_string


let float_opt_to_ns_string n_opt =
  Option.value_map n_opt ~default:"?" ~f:float_to_ns_string

(* tuples *)
let format_tuple (low, high) =
  sprintf "%s,%s" (float_to_string low) (float_to_string high)

let format_ns_tuple (low, high) =
  sprintf "%s,%s" (float_to_ns_string low) (float_to_ns_string high)

let format_plus_or_minus ~unit style (minus, plus)  =
  let f = match unit,style  with
    | _, `Percentage -> fun n -> (float_to_perc_string n)
    | `Nanos, `Absolute -> float_to_ns_string
    | `Cycles, `Absolute -> float_to_string_cycles
    | _ -> failwith "Don't know how to pretty print predictor!"
  in
  if Float.abs plus < 0.01
  && Float.abs minus < 0.01
  then ""
  else
    let plus,minus =
       (f plus), (f minus)
    in
    if (plus="" && minus="") then ""
    else sprintf "%s +%s" minus plus

(* easier for function composition *)
let compose_float_to_string f x =
  float_to_string (f x)

let compose_float_opt_to_string f x =
  float_opt_to_string (f x)

let compose_float_to_ns_string f x =
  float_to_ns_string (f x)

let compose_float_opt_to_ns_string f x =
  float_opt_to_ns_string (f x)

