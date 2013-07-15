open Core.Std
open Core_bench.Std

let t1 = Bench.Test.create_indexed
  ~name:"ArrayCreateInt"
  ~args:[100;200;300;400;1000;10000]
  (fun len -> Staged.stage
    (fun () ->
      ignore (Array.create ~len 0)))

let tests = [ t1 ]

let command = Bench.make_command tests

