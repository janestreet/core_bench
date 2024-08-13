open Core

(** Vectors *)
module Vec = struct
  type t = float array [@@deriving sexp]

  let copy = Array.copy
  let create0 len = Array.create ~len 0.

  let sumsq t =
    let accum = ref 0. in
    for i = 0 to Array.length t - 1 do
      let t_i = t.(i) in
      accum := !accum +. (t_i *. t_i)
    done;
    !accum
  ;;

  let norm t = sqrt (sumsq t)

  let almost_equal ~tol t1 t2 =
    Array.length t1 = Array.length t2
    && Array.for_all2_exn t1 t2 ~f:(fun x y -> Float.( <= ) (Float.abs (x -. y)) tol)
  ;;
end

(** Matrices *)
module Mat = struct
  type t = float array array [@@deriving sexp]

  let copy t = Array.map t ~f:Array.copy

  let create0 ~rows ~cols =
    let make_row _ = Array.create ~len:cols 0. in
    Array.init rows ~f:make_row
  ;;

  let create_per_row ~rows ~cols ~f =
    let make_row _ = Array.init cols ~f in
    Array.init rows ~f:make_row
  ;;

  (** The norm of a column of a matrix. *)
  let col_norm t column =
    let accum = ref 0. in
    for i = 0 to Array.length t - 1 do
      let entry = t.(i).(column) in
      accum := !accum +. (entry *. entry)
    done;
    sqrt !accum
  ;;

  (** The inner product of columns j1 and j2 *)
  let col_inner_prod t j1 j2 =
    let accum = ref 0. in
    for i = 0 to Array.length t - 1 do
      accum := !accum +. (t.(i).(j1) *. t.(i).(j2))
    done;
    !accum
  ;;

  let get_column t j =
    let len = Array.length t in
    Array.init len ~f:(fun i -> t.(i).(j))
  ;;

  let almost_equal ~tol t1 t2 =
    Array.length t1 = Array.length t2
    && Array.for_all2_exn t1 t2 ~f:(Vec.almost_equal ~tol)
  ;;
end

let qr_in_place a =
  (* Our implementation will just do Gram-Schmidt. *)
  let m = Array.length a in
  if m = 0
  then [||], [||] (* empty QR decomposition *)
  else (
    let n = Array.length a.(0) in
    let r = Mat.create0 ~rows:n ~cols:n in
    for j = 0 to n - 1 do
      (* handle column j *)
      let alpha = Mat.col_norm a j in
      r.(j).(j) <- alpha;
      let one_over_alpha = 1. /. alpha in
      (* Rescale this column so that it's a unit vector. *)
      for i = 0 to m - 1 do
        a.(i).(j) <- a.(i).(j) *. one_over_alpha
      done;
      for j2 = j + 1 to n - 1 do
        let c = Mat.col_inner_prod a j j2 in
        r.(j).(j2) <- c;
        (* Now, subtract c * column j from column j2. *)
        for i = 0 to m - 1 do
          a.(i).(j2) <- a.(i).(j2) -. (c *. a.(i).(j))
        done
      done
    done;
    a, r)
;;

(** [qr A] returns the QR-decomposition of [A] as a pair (Q,R). [A] must have
    at least as many rows as columns and have full rank.

    If [in_place] (default: [false]) is [true], then [A] is overwritten with [Q].
*)
let qr ?(in_place = false) a =
  let a = if in_place then a else Mat.copy a in
  qr_in_place a
;;

(** [triu_solve R b] solves R x = b where [R] is an m x m upper-triangular matrix
    and [b] is an m x 1 column vector.  *)
let triu_solve r b =
  let m = Array.length b in
  if m <> Array.length r
  then
    Or_error.error_string
      "triu_solve R b requires R to be square with same number of rows as b"
  else if m = 0
  then Ok [||]
  else if m <> Array.length r.(0)
  then Or_error.error_string "triu_solve R b requires R to be square"
  else (
    let sol = Vec.copy b in
    for i = m - 1 downto 0 do
      sol.(i) <- sol.(i) /. r.(i).(i);
      for j = 0 to i - 1 do
        sol.(j) <- sol.(j) -. (r.(j).(i) *. sol.(i))
      done
    done;
    if Array.exists sol ~f:Float.is_nan
    then Or_error.error_string "triu_solve detected NaN result"
    else Ok sol)
;;

(** [mul A B] computes the matrix product [A * B].  If [transa] (default: [false])
    is [true], then we compute A' * B where A' denotes the transpose of A. *)

(* val mul : ?transa:bool -> m -> m -> m *)

(** [mul_mv A x] computes the product [A * x] (where [A] is a matrix and [x] is
    a column vector).

    [transa] is as with [mul].
*)
let mul_mv ?(transa = false) a x =
  (* we let c denote either a or a', depending on whether transa is set. *)
  let rows = Array.length a in
  if rows = 0
  then [||]
  else (
    let cols = Array.length a.(0) in
    (* (m, n, c_get) will be (rows of c, columns of c, accessor for c). *)
    let m, n, c_get =
      if transa
      then (
        let c_get i j = a.(j).(i) in
        cols, rows, c_get)
      else (
        let c_get i j = a.(i).(j) in
        rows, cols, c_get)
    in
    if n <> Array.length x then failwith "Dimension mismatch";
    let result = Vec.create0 m in
    for i = 0 to m - 1 do
      result.(i)
      <- Array.foldi x ~init:0. ~f:(fun j accum x_j -> accum +. (c_get i j *. x_j))
    done;
    result)
;;

(** [ols A b] computes the ordinary least-squares solution to A x = b.
    [A] must have at least as many rows as columns and have full rank.

    This can be used to compute solutions to non-singular square systems,
    but is somewhat sub-optimal for that purpose.

    The algorithm is to factor A = Q * R and solve R x = Q' b where Q' denotes
    the transpose of Q.
*)
let ols ?(in_place = false) a b =
  let q, r = qr ~in_place a in
  triu_solve r (mul_mv ~transa:true q b)
;;
