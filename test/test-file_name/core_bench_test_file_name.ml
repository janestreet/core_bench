open! Core
open Core_bench

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create
           ~name:"name"
           ~test_name:"test_name"
           ~file_name:"file_name"
           (fun () -> 1 + 2)
       ])
;;
