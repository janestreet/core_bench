open Core_bench.Std
open Core_extended.Std

let get_float () =
  if Random.bool ()
  then 10.0
  else 10.0

let get_int64 () =
  if Random.bool ()
  then Int64.of_int 10
  else 10L

let t1 = Bench.Test.create ~name:"Id"
  (fun () -> ())

let t2 = Bench.Test.create ~name:"TimeNow"
  (fun () -> ignore (Time.now ()))

let t3 = Bench.Test.create ~name:"RDTSC"
  (fun () -> ignore (Time_stamp_counter.Cycles.now ()))

let t4 = Bench.Test.create ~name:"int64_of_float"
  (let fl = get_float () in
   (fun () -> ignore (Int64.of_float fl)))

let t5 = Bench.Test.create ~name:"int64_to_float"
  (let fl = get_int64 () in
   (fun () -> ignore (Int64.to_float fl)))

let tests = [ t1; t2; t3; t4; t5 ]


let command = Bench.make_command tests


