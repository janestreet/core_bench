open Core.Std
module Variable = Test_metrics.Variable

let random_indices_in_place arr =
  let len = Array.length arr in
  for i = 0 to len - 1 do
    arr.(i) <- Random.int len
  done

(* If we ever expose this function, we should check that low_quantile and high_quantile
   are in the interval [0,1].
*)
let quantile_of_array arr ?(failures=0) ~len ~low_quantile ~high_quantile =
  Array.sort arr ~len ~cmp:Float.compare;
  let index q =
    Float.iround_towards_zero_exn (Float.of_int len *. q +. 0.5 *. Float.of_int failures)
  in
  (* [extended_get i] retrieves entry [i] from [arr], pretending that
     [arr.(i) = infinity] when [i > len - 1], and that [arr.(i) = neg_infinity]
     when [i < failures].
     It assumes [i >= 0] and that entries with [i < failures] are already [neg_infinity].
  *)
  let extended_get i = if i >= len then Float.infinity else arr.(i) in
  (* For the low_quantile calculation, if the index is too large (too many failures),
     return the last entry of sorted array to preserve monotonicity *)
  let low = extended_get (Int.min (index low_quantile) (len - 1)) in
  let high = extended_get (Int.max (index high_quantile) failures) in
  low, high



module Coefficient = struct
  type t = {
    estimate : float;
    (* 95% confidence interval, stored as (left endpoint, right endpoint) *)
    confidence95 : float * float;
  } with fields

  (* 95% confidence interval expressed in (absolute) error form.
     E.g., if estimate = 50. and confidence95 = (49., 52.),
     then confidence95_abs_err returns (-1., 2.).
  *)
  let confidence95_abs_err t =
    let (low, high) = t.confidence95 in
    let est = t.estimate in
    (low -. est, high -. est)

  (* 95% confidence interval in relative error form (with 2.5 = 250%, etc.).
     E.g., if estimate = 50. and confidence95 = (49., 52.),
     then confidence95_rel_err returns (-0.02, 0.04).
  *)
  let confidence95_rel_err t =
    let (low, high) = t.confidence95 in
    let est = t.estimate in
    ((low -. est) /. est, (high -. est) /. est)

end

module Bootstrap = struct

  let print =
    let printed_warnings = String.Hash_set.create () in
    fun str ->
      if not (Hash_set.exists printed_warnings ~f:(fun s -> s=str))
      then (Hash_set.add printed_warnings str;
            Verbosity.print_high "%s" str)
      else ()


  (* If [bootstrap_trials] is 0, bootstrapping is suppressed and the confidence
     intervals are (-infinity, infinity). This is the default behaviour. If the user
     asks for bootstrap-values, bootstrapping will be triggered. *)
  let ols ?(bootstrap_trials=0) predictor_matrix response_vector
      : Coefficient.t array =
    let ols_result =
      match Linear_algebra.ols predictor_matrix response_vector with
      | Ok v -> v
      | _ -> failwith "Error while calculating MV-linear-regression"
    in
    let samples_len = Array.length response_vector in
    let predictors_len = Array.length ols_result in
    let selected_indices = Array.create ~len:samples_len 0 in
      (* bootstrap_coefficients is stored as array of columns *)
    let bootstrap_coefficients =
      Array.init predictors_len ~f:(fun _ -> Array.create ~len:bootstrap_trials 0.)
    in
    let bootstrap_fails = ref 0 in
    (* Each bootstrap replication/trial samples with replacement from the rows we have. *)
    for i = 0 to bootstrap_trials - 1 do
      random_indices_in_place selected_indices;
        (* copy data from predictor_matrix to use let ols destroy its input *)
      let bt_predictor_matrix = Array.map selected_indices ~f:(fun index ->
        Array.copy predictor_matrix.(index))
      in
      let bt_response_vector = Array.map selected_indices ~f:(fun index ->
        response_vector.(index))
      in
      match (Linear_algebra.ols ~in_place:true bt_predictor_matrix bt_response_vector) with
      | Ok bt_ols_result ->
        for p = 0 to predictors_len - 1 do
          bootstrap_coefficients.(p).(i) <- bt_ols_result.(p);
        done
      | _ ->
        incr bootstrap_fails;
        for p = 0 to predictors_len - 1 do
            (*  initialize to neg_infinity so that [quantile_of_array] can recognize which
                values came from failed bootstrap replications *)
          bootstrap_coefficients.(p).(i) <- Float.neg_infinity;
        done
    done;
    Array.init predictors_len ~f:(fun i ->
      let confidence95 =
        if bootstrap_trials = 0
        then (Float.neg_infinity, Float.infinity)
        else
          quantile_of_array (bootstrap_coefficients.(i))
            ~failures:!bootstrap_fails
            ~len:bootstrap_trials
            ~low_quantile:0.025
            ~high_quantile:0.975
      in
      let estimate = ols_result.(i) in
      { Coefficient. estimate; confidence95 }
    )

end



let r_square predictors_matrix responder_vector coefficients =
  let sum_responder = Array.fold responder_vector ~init:0. ~f:(+.) in
  let mean = sum_responder /. Float.of_int (Array.length responder_vector) in
  let tot_ss = ref 0. in
  let res_ss = ref 0. in
  let predicted i =
    let x = ref 0. in
    for j = 0 to Array.length coefficients - 1 do
      x := !x +. predictors_matrix.(i).(j) *. (Coefficient.estimate coefficients.(j))
    done;
    !x
  in
  for i = 0 to Array.length responder_vector - 1 do
    tot_ss := !tot_ss +. (responder_vector.(i) -. mean) ** 2.;
    res_ss := !res_ss +. (responder_vector.(i) -. predicted i) ** 2.;
  done;
  1. -. !res_ss /. !tot_ss

type mv_coefficients_and_rsquare =
  (Variable.t * Coefficient.t) array * float

(* A note about the constant [threshold = 10] (for the number of nonzero values
   each predictor must have) below:

   We are interested in producing 95% confidence intervals.  As a result, we want enough
   nonzero entries so that at least 95% of bootstrap replications succeed.  The
   probability that a particular row is omitted in a particular bootstrap replication is
   about 1/e = 0.36788.  If there are n nonzero entries in a column, the probability
   that they're all omitted is 0.36788^n; we want that to be less than 0.05.  n = 3 is
   sufficiently large for that.

   Of course, there are multiple columns to worry about. Supposing conservatively that the
   user wants to use up to 20 predictors, and noting that the probability that we get
   failure in some column is bounded above by the sum of the probabilities for the
   individual columns, we want 0.36788^n < 0.05/20.  n = 6 is sufficiently large for
   that.  (In practice, the predictors will tend to be correlated, so the upper bound
   obtained by summing is conservative.)

   Something else to worry about is that the fundamental assumption made when using
   bootstrapping--that the empirical distribution is a good approximation of the
   underlying distribution--starts to break down if we have very few nonzero values.
   So, for a little "breathing room", n = 10 should be sufficient.

   This should yield far fewer than 5% of bootstrap trials failing, so the following
   is a relatively minor point.  In the presence of failures, our 95% confidence
   interval still encompasses 95% of _all_ trials, including failures.  We position
   the confidence interval so that it is centered within the interval of all trials
   that succeeded.  E.g., if there were 1000 trials, and no failures, we would
   ordinarily take the values at indices 25 and 975 as endpoints for the 95% confidence
   interval; if there are 20 failures, we will (with all the failures sorted to
   the front of the array) instead take the values at indices 35 and 985.
*)

let do_regression ?bootstrap_trials ~predictors ~responder ~test data
    : mv_coefficients_and_rsquare =
  let good_predictors =
    let threshold = 10 in
    let nonzero_table = Int.Table.create () in
    let values = Partial_array.values data in
    for p = 0 to Array.length predictors - 1 do
      Hashtbl.set nonzero_table ~key:p ~data:0;
    done;
    for i = 0 to Partial_array.len data - 1 do
      for p = 0 to Array.length predictors - 1 do
        if (Variable.accessor predictors.(p)) values.(i) <> 0.
        then Hashtbl.incr nonzero_table p
      done
    done;
    Array.filteri predictors ~f:(fun i p ->
      let nonzero_entries = Hashtbl.find_exn nonzero_table i in
      if (nonzero_entries < threshold)
      then
        (let warning_string =
           if (nonzero_entries<>0)
           then sprintf
             "Ignoring %s for %s - %d nonzero values, at least %d required.\n"
             (Variable.to_string p)
             (test.Test.Basic_test.name)
             nonzero_entries
             threshold
           else sprintf "Ignoring %s for %s - they are always zero.\n"
             (Variable.to_string p) (test.Test.Basic_test.name)
         in
         Bootstrap.print warning_string;
         false)
      else true
    )
  in
  let pred_accessors = Array.map good_predictors ~f:Variable.accessor in
  let get_pred_row test_metrics =
    Array.map pred_accessors ~f:(fun accessor -> accessor test_metrics)
  in
  let predictor_matrix = Partial_array.map_to_array data ~f:get_pred_row in
  let response_vector =
    Partial_array.map_to_array data ~f:(Variable.accessor responder)
  in
  let coefficients =
    Bootstrap.ols ?bootstrap_trials predictor_matrix response_vector
  in
  let mv_coefficients =
    Array.init (Array.length good_predictors) ~f:(fun i ->
      (good_predictors.(i), coefficients.(i))
    )
  in
  let r_square = r_square predictor_matrix response_vector coefficients in
  (mv_coefficients, r_square)


(* memoize on test-id, predictors, bootstrap_Trials and responder for mv-regressions *)
let cache = Int.Table.create ()

let do_regression_memoize ?bootstrap_trials ~predictors ~responder ~test data =
  let key = Hashtbl.hash (test.Test.Basic_test.test_id)
    + Hashtbl.hash (predictors)
    + Hashtbl.hash (responder)
    + Hashtbl.hash (Option.value bootstrap_trials ~default:0)
  in
  match (Hashtbl.find cache key) with
  | Some v -> v
  | None ->
    let v =
      do_regression ?bootstrap_trials ~predictors ~responder ~test data
    in
    Hashtbl.set cache ~key ~data:v;
    v
