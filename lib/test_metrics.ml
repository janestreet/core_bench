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
} with fields

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

let max stats ~len ~field =
  let x = field stats.(0) in
  let rec loop i x =
    if i < len
    then loop (i+1) (Int.max x (field stats.(i)))
    else x
  in loop 1 x


module Variable = struct
  type t =
  [ `Runs
  | `Cycles
  | `Nanos
  | `Compactions
  | `Minor_collections
  | `Major_collections
  | `Promoted
  | `Minor_allocated
  | `Major_allocated
  | `One (* the "variable" that is always 1 *)
  ] with sexp

  let of_string = function
    | "r" | "runs" -> `Runs
    | "m" | "minor_collections" -> `Minor_collections
    | "c" | "compactions" -> `Compactions
    | "n" | "nanos" -> `Nanos
    (* not including other variables here, since they shouldn't be available for user *)
    | unknown -> failwithf "Unknown predictor variable: %s" unknown ()

  let to_short_singular_string  = function
    | `Runs              -> "Run"
    | `Minor_collections -> "mGC"
    | `Major_collections -> "mjGC"
    | `Compactions       -> "Cpn"
    | `Nanos             -> "Ns"
    | `Cycles            -> "Cy"
    | _ -> failwith "Unknown predictor variable"

  let to_string  = function
    | `Runs              -> "Runs"
    | `Minor_collections -> "Minor GCs"
    | `Major_collections -> "Najor GCs"
    | `Compactions       -> "Compations"
    | `Nanos             -> "Nanos"
    | `Cycles            -> "Cycles"
    | _ -> failwith "Unknown predictor variable"

  let floatify = Fn.compose Float.of_int

  let accessor = function
    | `Runs        -> floatify runs
    | `Minor_collections   -> floatify minor_collections
    | `Major_collections   -> floatify major_collections
    | `Compactions -> floatify compactions
    | `Nanos       -> floatify nanos
    | `Cycles      -> floatify cycles
    | `Minor_allocated -> floatify minor_allocated
    | `Major_allocated -> floatify major_allocated
    | `Promoted    -> floatify promoted
    | `One -> fun _ -> 1.

end
