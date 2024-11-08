open Core
open Poly

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

let get_units = function
  | `Runs -> Display_units.Count
  | `Cycles -> Display_units.Cycles
  | `Nanos -> Display_units.Time
  | `Compactions -> Display_units.Gc
  | `Minor_collections -> Display_units.Gc
  | `Major_collections -> Display_units.Gc
  | `Promoted -> Display_units.Words
  | `Minor_allocated -> Display_units.Words
  | `Major_allocated -> Display_units.Words
  | `One -> Display_units.Count
  | `Extra _ -> Display_units.Count
;;

let conv =
  [ `Nanos, "Time", "Time"
  ; `Cycles, "Cycls", "Cycles"
  ; `Runs, "Run", "Runs per sampled batch"
  ; `Minor_collections, "mGC", "Minor Collections"
  ; `Major_collections, "mjGC", "Major Collections"
  ; `Compactions, "Comp", "Compactions"
  ; `Minor_allocated, "mWd", "Minor Words"
  ; `Major_allocated, "mjWd", "Major Words"
  ; `Promoted, "Prom", "Promoted Words"
  ; `One, "One", "Constant predictor for estimating measurement overhead"
  ]
;;

let summarize () =
  String.concat
    ~sep:"\n\t"
    (List.map conv ~f:(fun (_, s1, s2) -> sprintf "%-5s - %s" s1 s2))
;;

let to_short_string var =
  match var with
  | `Extra str -> str
  | var ->
    let opt = List.find_map conv ~f:(fun (v, s, _) -> if v = var then Some s else None) in
    Option.value_exn opt ~message:"Bug: Unable to find short string for variable."
;;

let of_short_string str =
  let opt = List.find_map conv ~f:(fun (v, s, _) -> if str = s then Some v else None) in
  Option.value_exn
    opt
    ~message:
      (sprintf
         "Unable to variable for string %s. Expected one of %s."
         str
         (String.concat ~sep:", " (List.map conv ~f:(fun (_, s, _) -> s))))
;;

let to_string var =
  let opt = List.find_map conv ~f:(fun (v, _, s) -> if v = var then Some s else None) in
  Option.value_exn opt ~message:"Bug: Unable to find string for variable."
;;

let make_col_name resp pred =
  match pred with
  | `One -> sprintf "%s Overhd" (to_short_string resp)
  | `Cycles -> to_short_string resp ^ "/Cycle"
  | _ -> to_short_string resp ^ "/" ^ to_short_string pred
;;
