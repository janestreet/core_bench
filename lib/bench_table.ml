open Core.Std


module Ascii_table = Textutils.Ascii_table
module Variable    = Test_metrics.Variable


open To_string


(* Printing functions : all of these functions are related to the table display which is
   the main output of bench. *)

(* Memoize *)
let memoize_by_test_id f =
  let cache = Test.Id.Table.create () in
  (fun (test, results, len) ->
    match Hashtbl.find cache test.Test.Basic_test.test_id with
    | Some v -> v
    | None ->
      let v = f (test, results, len) in
      Hashtbl.set cache ~key:test.Test.Basic_test.test_id ~data:v;
      v)

(* Retrieve multivariable regression *)
let get_mv_regr ~predictors ?bootstrap_trials ~responder =
   (fun (test,values,len) ->
     Mv_regression.do_regression_memoize
       ~predictors:(Array.of_list predictors)
       ~responder
       ?bootstrap_trials
       ~test
       (Partial_array.create ~values ~len)
   )

let get_mv_coefficient ~predictors ~responder ?bootstrap_trials predictor arg =
  let (mv_result,_) = get_mv_regr ~predictors ?bootstrap_trials ~responder arg in
  match (Array.find mv_result ~f:(fun (p,_) -> p=predictor)) with
  | None -> None
  | Some (_,c) -> Some c

(* formatting functions for displaying different columns *)
let make_name (test, _results, _len) =
  Test.Basic_test.name test

let make_samples =
  memoize_by_test_id (fun (_test, results, len) ->
    let samples = Int.to_string len in
    let max_runs = float_to_string
      (Float.of_int
         (Test_metrics.max ~field:Test_metrics.runs ~len results)) in
    sprintf "%s/%s" max_runs samples)

let make_allocated ~responder arg =
  match (get_mv_coefficient ~predictors:[`Runs] ~responder `Runs arg) with
  | None -> ""
  | Some coefficient ->
    let coefficient_est =  Mv_regression.Coefficient.estimate coefficient in
    float_to_string coefficient_est


let make_collections_per_k ~responder arg =
  match (get_mv_coefficient ~predictors:[`Runs] ~responder `Runs arg) with
  | None -> ""
  | Some coefficient ->
    let coefficient_est =  Mv_regression.Coefficient.estimate coefficient in
    float_to_string (coefficient_est *. 1000.)


(* formatting function for multivariate regression *)
let make_mv_coefficient ~predictors ~responder ?bootstrap_trials predictor arg =
  match (get_mv_coefficient ~predictors ~responder ?bootstrap_trials predictor arg) with
  | None -> ""
  | Some coefficient ->
    let coefficient_est =  Mv_regression.Coefficient.estimate coefficient in
    (* For Runs-column, always want result in ns *)
      match predictor with
      | `Runs -> float_to_string coefficient_est
      | _ ->
        match responder with
        | `Nanos -> float_to_ns_string coefficient_est
        | `Cycles -> float_to_string_cycles coefficient_est

let make_95ci (style : [ `Absolute | `Percentage ]) ~predictors ~responder predictor arg =
  let get coefficient =
    match style with
    | `Absolute -> Mv_regression.Coefficient.confidence95_abs_err coefficient
    | `Percentage -> Mv_regression.Coefficient.confidence95_rel_err coefficient
  in
  match (get_mv_coefficient ~predictors ~bootstrap_trials:3000 ~responder predictor arg) with
  | None -> ""
  | Some coefficient ->
    format_plus_or_minus style (get coefficient) ~unit:responder

let make_rsquare ~predictors ~responder arg =
  let (_, r_square) = get_mv_regr ~predictors ~bootstrap_trials:3000 ~responder arg in
  float_to_string r_square


let get_cycles ~predictors arg =
  match (get_mv_coefficient ~predictors ~responder:`Cycles `Runs arg) with
  | None -> 0.
  | Some cycles_coeff ->
    Mv_regression.Coefficient.estimate cycles_coeff

let make_percentage ~predictors max_cycles arg =
  let cycles = get_cycles ~predictors arg in
  let perc = 100.0 *. cycles /. max_cycles in
  sprintf "%0.2f" perc

let make_speedup ~predictors max_cycles arg =
  let cycles = get_cycles ~predictors arg in
  let perc = max_cycles /. cycles in
  sprintf "%0.2f" perc


(***************************************************************)


let print
    ?limit_width_to
    ?display
    ?(ascii_table=false)
    ?(ci_absolute=false)
    ~columns
    ~predictors
    data =
  let left, right = Ascii_table.Align.(left, right) in
  (* Map displayed columns to `If_not_empty or `Yes. *)
  let displayed =
    List.fold columns ~init:Column.Map.empty ~f:(fun cmap column ->
      match column with
      | `If_not_empty c -> Column.Map.add cmap ~key:c ~data:`If_not_empty
      | #Column.t as c  -> Column.Map.add cmap ~key:c ~data:`Yes)
  in
  let max_cycles =
    if Column.Map.mem displayed `Percentage || Column.Map.mem displayed `Speedup
    then List.reduce_exn (List.map data ~f:(get_cycles ~predictors)) ~f:Float.max
    else 0.0
  in
  let col tags name make align =
    let tags = List.map tags ~f:(fun t ->
      Option.value (Column.Map.find displayed t) ~default:`No)
    in
    let show =
      if List.exists tags ~f:(fun t -> t=`No)
      then `No
      else if List.exists tags ~f:(fun t -> t=`Yes)
      then `Yes
      else `If_not_empty
    in
    (* Ascii_table calls [make] even if show = `No, which results in a lot of wasted
       computation in this case.  Thus we redefine [make] here. *)
    let make =
      match show with
      | `No -> fun _ -> ""
      | `Yes | `If_not_empty -> make
    in
    Ascii_table.Column.create name make ~align ~show
  in
  let make_rsquare ~preds ~resp pred =
    make_rsquare ~predictors:preds ~responder:resp pred
  in
  let make_95ci ~preds ~resp pred =
    if ci_absolute
    then make_95ci `Absolute   ~predictors:preds ~responder:resp pred
    else make_95ci `Percentage ~predictors:preds ~responder:resp pred
  in
  let make_coeff ~preds ~resp pred =
    make_mv_coefficient ~responder:resp ~predictors:preds pred
  in
  let bars = if ascii_table then `Ascii else `Unicode in
  let rname = function `Nanos -> "Time" | `Cycles -> "Cycles" in
  let colname ~resp ~pred = rname resp  ^ "/" ^ Variable.to_short_singular_string pred in
  let rsq_colname resp = rname resp ^ " R^2" in
  let responder_columns =
    let preds = predictors in
    List.concat_map [`Nanos; `Cycles] ~f:(fun resp ->
      let resp_col = (resp :> Column.t) in
      (col [resp_col; `Confidence] (rsq_colname resp) (make_rsquare ~preds ~resp) right)
      :: (List.concat_map predictors ~f:(fun pred ->
        [col [resp_col] (colname ~resp ~pred) (make_coeff ~resp ~preds pred) right;
         col [resp_col; `Confidence] "95% ci" (make_95ci ~resp ~preds pred) right])))
  in
  let columns =
    (col [`Name] "Name" make_name left) ::
      responder_columns @
      [col [`Allocated] "Minor" (make_allocated ~responder:`Minor_allocated) right;
       col [`Allocated] "Major" (make_allocated ~responder:`Major_allocated) right;
       col [`Allocated] "Promoted" (make_allocated ~responder:`Promoted) right;
       col [`GC] "#minor GCs/k" (make_collections_per_k ~responder:`Minor_collections) right;
       col [`GC] "#Major GCs/k" (make_collections_per_k ~responder:`Major_collections) right;
       col [`GC] "#Compactions/k" (make_collections_per_k ~responder:`Compactions) right;
       col [`Samples] "Runs/Samples" make_samples right;
       col [`Percentage] "% of max" (make_percentage ~predictors max_cycles) right;
       col [`Speedup] "Speedup" (make_speedup ~predictors max_cycles) right;
      ]
  in
  Ascii_table.output ?display ~oc:stdout ~bars ?limit_width_to columns data
