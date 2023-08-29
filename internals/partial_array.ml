open Core

type 'a t =
  { values : 'a array
  ; mutable len : int
  }

let create ~values ~len = { values; len }
let map_to_array t ~f = Array.init t.len ~f:(fun i -> f t.values.(i))
