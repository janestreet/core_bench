(** A module internal to [Core_bench]. Please look at {!Bench}.

   Contains the measurements of one run of a benchmark. *)
open Core.Std

type t = {
  mutable runs              : int;
  mutable cycles            : int;
  mutable nanos             : int;
  mutable compactions       : int;
  mutable minor_allocated   : int;
  mutable major_allocated   : int;
  mutable promoted          : int;
  mutable major_collections : int;
  mutable minor_collections : int;
} [@@deriving sexp, fields]

let create () = {
  runs              = 0;
  cycles            = 0;
  nanos             = 0;
  compactions       = 0;
  minor_allocated   = 0;
  major_allocated   = 0;
  promoted          = 0;
  major_collections = 0;
  minor_collections = 0;
}



let field_names_to_string () =
  let field_name str f = str ^ " " ^ (Field.name f) ^ ";" in
  (Fields.fold ~init:"#" ~runs:field_name ~cycles:field_name ~nanos:field_name
     ~compactions:field_name ~minor_allocated:field_name ~major_allocated:field_name
     ~promoted:field_name ~minor_collections:field_name ~major_collections:field_name)

let field_values_to_string t =
  let value_str str field = str ^ (Int.to_string (Field.get field t)) ^ " " in
  Fields.fold ~init:""
    ~runs:value_str
    ~nanos:value_str
    ~minor_collections:value_str
    ~major_collections:value_str
    ~compactions:value_str
    ~cycles:value_str
    ~minor_allocated:value_str
    ~major_allocated:value_str
    ~promoted:value_str

let of_field_values_string line =
  let parts = String.split ~on:' ' (String.strip line) in
  let t = create () in
  let set_value vs field =
    match vs with
    | v :: vs -> (Option.value_exn (Field.setter field)) t (Int.of_string v); vs
    | [] -> failwith "Too few columns in saved metrics file."
  in
  begin match (Fields.fold ~init:parts
                 ~runs:set_value
                 ~nanos:set_value
                 ~minor_collections:set_value
                 ~major_collections:set_value
                 ~compactions:set_value
                 ~cycles:set_value
                 ~minor_allocated:set_value
                 ~major_allocated:set_value
                 ~promoted:set_value) with
  | [] -> ()
  | ls -> failwithf "Too many data values in saved metrics file: %s"
    (String.concat ~sep:"," ls) ()
  end;
  t

let max stats ~len ~field =
  let x = field stats.(0) in
  let rec loop i x =
    if i < len
    then loop (i+1) (Int.max x (field stats.(i)))
    else x
  in loop 1 x

let floatify = Fn.compose Float.of_int

let accessor = function
  | `Runs              -> floatify runs
  | `Minor_collections -> floatify minor_collections
  | `Major_collections -> floatify major_collections
  | `Compactions       -> floatify compactions
  | `Nanos             -> floatify nanos
  | `Cycles            -> floatify cycles
  | `Minor_allocated   -> floatify minor_allocated
  | `Major_allocated   -> floatify major_allocated
  | `Promoted          -> floatify promoted
  | `One               -> fun _ -> 1.

