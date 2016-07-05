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
    { benchmark_name                  : string
    ; benchmark_name_with_index       : string
    ; full_benchmark_name             : string
    ; file_name                       : string
    ; module_name                     : string
    ; library_name                    : string
    ; version                         : string
    ; hg_revision                     : string option
    ; x_library_inlining              : bool
    ; ocaml_version                   : string
    ; machine_where_benchmark_was_run : string
    ; epoch_time_of_run               : int
    ; time_of_hg_revision             : string option
    ; time_r_square                   : float
    ; time_per_run_nanos              : float
    ; ci95_upper_bound                : float
    ; ci95_lower_bound                : float
    ; minor_words_per_run             : float
    ; major_words_per_run             : float
    ; promoted_words_per_run          : float
    }
  [@@deriving typerep]

  let to_json =
    let `generic to_json = Json_typerep.Jsonrep.V2.json_of_t typerep_of_t in
    to_json

  let of_json =
    let `generic of_json = Json_typerep.Jsonrep.V2.t_of_json typerep_of_t in
    of_json

end

module Results = struct
  type t = Result.t list
  [@@deriving typerep]

  let to_json =
    let `generic to_json = Json_typerep.Jsonrep.V2.json_of_t typerep_of_t in
    to_json

  let of_json =
    let `generic of_json = Json_typerep.Jsonrep.V2.t_of_json typerep_of_t in
    of_json


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
  let get_bench_name_with_index res =
    let tmp = List.nth_exn (String.split ~on:']' (Analysis_result.name res)) 1 in
    String.drop_prefix tmp 1
  in
  let estimate regr = C.estimate (R.coefficients regr).(0) in
  let get_ci regr = C.ci95 (R.coefficients regr).(0) in
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
  let version = Version_util.version in
  let simplified_results = List.map results ~f:(fun res ->
    let jtest =
      Result.({
        full_benchmark_name = Analysis_result.name res
      ; benchmark_name = Analysis_result.test_name res
      ; benchmark_name_with_index = get_bench_name_with_index res
      ; file_name = Analysis_result.file_name res
      ; module_name = Analysis_result.module_name res
      ; library_name = libname
      ; version = version
      ; hg_revision = None
      ; x_library_inlining = Version_util.x_library_inlining
      ; ocaml_version = Version_util.ocaml_version
      ; machine_where_benchmark_was_run = Unix.gethostname ()
      ; epoch_time_of_run = cur_time
      ; time_of_hg_revision = None
      ; time_r_square = 0.
      ; time_per_run_nanos = 0.
      ; ci95_upper_bound = 0.
      ; ci95_lower_bound = 0.
      ; minor_words_per_run = 0.
      ; major_words_per_run = 0.
      ; promoted_words_per_run = 0.
      }) in
    Array.fold (Analysis_result.regressions res) ~init:jtest ~f:(fun acc regr ->
      let acc =
        match R.r_square regr with
        | Some rsq -> { acc with time_r_square = rsq }
        | None -> acc
      in
      let value = estimate regr in
      match (R.responder regr) with
      | `Nanos ->
        if check_time_preds regr
        then
          begin match get_ci regr with
          | None -> { acc with time_per_run_nanos = value }
          | Some ci ->
            let (ci_minus, ci_plus) = Analysis_result.Ci95.ci95_rel_err ci ~estimate:value
            in { acc with time_per_run_nanos = value
                        ; ci95_upper_bound = ci_plus
                        ; ci95_lower_bound = ci_minus
               }
          end
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
