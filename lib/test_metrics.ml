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

let save test ~results max_used =
  let filename = Test.Basic_test.make_filename test in
  Verbosity.print_high "%s: Writing %d samples to file: %s.%!\n"
    (Test.Basic_test.name test) max_used filename;
  let header1 = "# " ^ test.Test.Basic_test.name in
  let header2 =
    let field_name str f = str ^ " " ^ (Field.name f) ^ ";" in
    (Fields.fold ~init:"#" ~runs:field_name ~cycles:field_name ~nanos:field_name
       ~compactions:field_name ~minor_allocated:field_name ~major_allocated:field_name
       ~promoted:field_name ~minor_collections:field_name ~major_collections:field_name)
  in
  let ls = List.rev (Array.foldi results ~init:[] ~f:(fun i ls _ ->
    if i < max_used then
      let value_str str f = str ^ (Int.to_string (Field.get f results.(i))) ^ " " in
      let line =
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
      in
      line :: ls
    else
      ls)) in
  Out_channel.write_lines filename (header1 :: header2 :: ls)

let load filename =
  match In_channel.read_lines filename with
  | header1 :: _header2 :: data -> begin
    let name = String.subo ~pos:2 header1 in
    let metrics = List.map data ~f:(fun line ->
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
      t)
    in
    let metrics = Array.of_list metrics in
    let fake_test = Test.Basic_test.create ~name (fun () -> ()) in
    fake_test, metrics, Array.length metrics
  end
  | _ -> failwith "Bad header format for saved metrics file."


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
