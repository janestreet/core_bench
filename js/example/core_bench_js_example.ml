open! Core
open Js_of_ocaml

let%bench "Int.to_string" = Int.to_string 5
let%bench "Float.to_string" = Float.to_string 5.0
let%bench "sprintf %.12g" = sprintf "%.12g" 5.0

let fts : float -> string =
  let f = Js.Unsafe.pure_js_expr {| (function (f) {return f.toString();}) |} in
  fun x ->
    Js.Unsafe.fun_call f [| Js.Unsafe.inject (Js.number_of_float x) |] |> Js.to_string
;;

let c_fts : float -> string =
  let f = Js_of_ocaml.Js.Unsafe.pure_js_expr {| (function (f) {return ""+f;}) |} in
  fun x ->
    Js.Unsafe.fun_call f [| Js.Unsafe.inject (Js.number_of_float x) |] |> Js.to_string
;;

let%bench "float.toString()" = fts 5.0
let%bench "\"\" + float" = c_fts 5.0
let%bench "sprintf \"%dpx\" float" = sprintf "%dpx" 5
let%bench "(Int.to_string int) ^ \"px\"" = Int.to_string 5 ^ "px"
