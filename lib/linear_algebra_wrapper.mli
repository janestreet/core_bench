open Core.Std

val ols
  :  Measurement.t
  -> resp:Variable.t
  -> preds:Variable.t array
  -> float array Or_error.t

val r_square
  :  Measurement.t
  -> resp:Variable.t
  -> preds:Variable.t array
  -> coeffs:float array
  -> float

val bootstrap
  :  trials:int
  -> Measurement.t
  -> resp:Variable.t
  -> preds:Variable.t array
  -> Analysis_result.Ci95.t array Or_error.t



