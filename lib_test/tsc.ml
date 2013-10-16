open Core.Std
open Core_bench.Std
open Core_extended.Std

let t1 = Bench.Test.create ~name:"Time.now"
  (fun () -> ignore (Time.now ()))

let t2 = Bench.Test.create ~name:"TSC.now"
  (fun () -> ignore (Time_stamp_counter.now ()))

let t3 =
  let c = Time_stamp_counter.now () in
  Bench.Test.create ~name:"TSC.to_time"
  (fun () ->
    ignore (Time_stamp_counter.to_time c))

let t4 =
  Bench.Test.create ~name:"TSC.to_time (TSC.now ())"
  (fun () ->
    ignore (Time_stamp_counter.to_time
              (Time_stamp_counter.now ())))

let t5 =
  let c = Time_stamp_counter.now () in
  Bench.Test.create ~name:"TSC.to_nanos_since_epoch"
  (fun () ->
    ignore (Time_stamp_counter.to_nanos_since_epoch c))

let t6 =
  Bench.Test.create ~name:"TSC.to_nanos_since_epoch(TSC.now ())"
  (fun () ->
    ignore (Time_stamp_counter.to_nanos_since_epoch
              (Time_stamp_counter.now ())))

let t7 =
  Bench.Test.create ~name:"Calibrator.calibrate"
    (fun () ->
       ignore (Time_stamp_counter.Calibrator.calibrate ()))


let tests = [ t1; t2; t3; t4; t5; t6; t7]

let command = Bench.make_command tests
