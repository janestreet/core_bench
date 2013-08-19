open Core.Std

module T = struct
  type t =
  [ `Name
  | `Cycles
  | `Nanos
  | `Confidence
  | `Allocated
  | `Percentage
  | `GC
  | `Speedup
  | `Samples
  ] with sexp, compare
end

module Map = Map.Make (T)
include T

let name_desc_assoc_list =
  [("name"      , `Name            , "Name of the test.");
   ("cycles"    , `Cycles          , "Number of CPU cycles (RDTSC) taken.");
   ("time"      , `Nanos           , "Number of nano secs taken.");
   ("confidence", `Confidence      , "95% confidence interval and R^2 error for predictors.");
   ("alloc"     , `Allocated       , "Allocation of major, minor and promoted words.");
   ("gc"        , `GC              , "Show major and minor collections per 1000 runs.");
   ("percentage", `Percentage      , "Relative execution time as a percentage.");
   ("speedup"   , `Speedup         , "Relative execution cost as a speedup.");
   ("samples"   , `Samples         , "Number of samples collected for profiling.");
  ]

let name_assoc_list =
  List.map name_desc_assoc_list ~f:(fun (name, tag, _) -> (name, tag))

let column_description_table =
  let max =
    let length (str, _, _) = String.length str in
    List.reduce_exn ~f:Int.max
      (List.map name_desc_assoc_list ~f:length)
  in
  let extend x =
    let slack = max - String.length x in
    x ^ String.make slack ' '
  in
  String.concat ~sep:"\n\t"
    (List.map name_desc_assoc_list ~f:(fun (name, _, desc) ->
      sprintf "%s - %s"
        (extend name) desc))

let of_string col : [ t | `If_not_empty of t ] =
  let col, non_empty =
    match String.chop_prefix col ~prefix:"+" with
    | Some col -> col, true
    | None -> col, false
  in
  let col =
    match (List.Assoc.find ~equal:String.equal name_assoc_list col) with
    | Some col -> col
    | None -> failwithf "Invalid column name: %s" col ()
  in
  if non_empty
  then `If_not_empty col
  else col

let arg = Command.Spec.Arg_type.create of_string


