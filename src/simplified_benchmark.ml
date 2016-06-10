open Core.Std

module R = Analysis_result.Regression
module C = Analysis_result.Coefficient

module Regression_info = struct
  type t =
    { value : float
    ; predictors : string list
    }
  [@@deriving typerep]
end

(* each result independently stores redundant info because Kibana can only handle flat
   document structure, i.e. no nesting *)
module Result = struct
  type t =
    { name                   : string
    ; libname                : string
    ; version                : string
    ; x_library_inlining     : bool
    ; ocaml_version          : string
    ; host_name              : string
    ; epoch_time_run         : int
    ; time_per_run           : float
    ; minor_words_per_run    : float
    ; major_words_per_run    : float
    ; promoted_words_per_run : float
    }
  [@@deriving typerep]

  let to_json =
    let `generic to_json = Json_typerep.Jsonrep.V2.json_of_t typerep_of_t in
    to_json

end

module Results = struct
  type t = Result.t list
  [@@deriving typerep]

  let to_json =
    let `generic to_json = Json_typerep.Jsonrep.V2.json_of_t typerep_of_t in
    to_json


  let to_esbulk t =
    (List.map t ~f:(fun res ->
       String.concat
         [ {|{"create":{"_index":"benchtest","_type":"test"}}|}
         ; "\n"
         ; Json_wheel_internal.Std.Json_io.string_of_json ~compact:true (Result.to_json res)
         ])
     |> String.concat ~sep:"\n")

end

let extract ?(libname="") (results : Analysis_result.t list) =
  let estimate regr = C.estimate (R.coefficients regr).(0) in
  let check_time_preds regr =
    let preds = R.predictors regr in
    Array.length preds = 1 && Array.exists preds ~f:((=) `Runs)
  in
  let check_overhead_preds regr =
    let preds = R.predictors regr in
    Array.length preds = 2 && Array.exists preds ~f:((=) `One)
    && Array.exists preds ~f:((=) `Runs)
  in
  let cur_time = (Time_ns.now () |> Time_ns.to_int_ns_since_epoch) in
  let simplified_results = List.map results ~f:(fun res ->
    let jtest =
      Result.({
        name = Analysis_result.name res
      ; libname = libname
      ; version = Version_util.version
      ; x_library_inlining = Version_util.x_library_inlining
      ; ocaml_version = Version_util.ocaml_version
      ; host_name = Unix.gethostname ()
      ; epoch_time_run = cur_time
      ; time_per_run = 0.
      ; minor_words_per_run = 0.
      ; major_words_per_run = 0.
      ; promoted_words_per_run = 0.
      }) in
    Array.fold (Analysis_result.regressions res) ~init:jtest ~f:(fun acc regr ->
      let value = estimate regr in
      match (R.responder regr) with
      | `Nanos ->
        if check_time_preds regr
        then { acc with time_per_run = value }
        else acc
      | `Minor_allocated ->
        if check_overhead_preds regr
        then { acc with minor_words_per_run = value }
        else acc
      | `Major_allocated ->
        if check_overhead_preds regr
        then { acc with major_words_per_run = value }
        else acc
      | `Promoted ->
        if check_overhead_preds regr
        then { acc with promoted_words_per_run = value }
        else acc
      | _ -> acc
    )
  ) in
  simplified_results

let to_json ?libname results =
  extract ?libname results |> Results.to_json

let to_elastic_bulk_format ?libname results =
  extract ?libname results |> Results.to_esbulk
