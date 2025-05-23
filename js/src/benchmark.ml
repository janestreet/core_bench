open Core
open Core_bench_internals

let stabilize_gc () =
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0
    then failwith "unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.Stat.live_words <> last_heap_live_words
    then loop (failsafe - 1) stat.Gc.Stat.live_words
  in
  loop 10 0
;;

(* The main benchmarking function *)
let measure =
  let module RC = Run_config in
  let module M = Measurement_sample in
  fun run_config test ->
    (* test function *)
    let (Test.Basic_test.T f) = test.Test.Basic_test.f in
    let f = f `init in
    (* the samples *)
    let max_samples = 3_000 in
    let results = Array.init max_samples ~f:(fun _ -> M.create ()) in
    (* counters *)
    let index = ref 0 in
    let runs = ref 0 in
    let total_runs = ref 0 in
    (* get the old Gc settings *)
    let old_gc = Gc.get () in
    stabilize_gc ();
    (* THE MAIN TEST LOOP *)
    let init_t1 = Time_float.now () in
    let quota = RC.quota run_config in
    let quota_max_count = Quota.max_count quota in
    while
      (not (Quota.fulfilled quota ~start:init_t1 ~num_calls:!total_runs))
      && !index < Array.length results
    do
      let current_runs = !runs in
      let current_index = !index in
      (* Stabilize gc if required.

         We stabilize the gc through the first pass through this loop anyway. If we don't do
         this the incoming GC state (some data may be on the minor heap that is partly full)
         will cause an early collection or two which will not happen subsequently. These
         early collections are just noise.

         When benchmarking functions that do not allocate any memory, this early noise is
         the only significant input. In these cases, these spurious early collections will
         give the allocation stats (major and promoted words) a slight negative value. *)
      if RC.stabilize_gc_between_runs run_config && current_runs <> 0 then stabilize_gc ();
      (* make any Gc changes required. *)
      if RC.no_compactions run_config
      then Gc.set { (Gc.get ()) with Gc.Control.max_overhead = 1_000_000 };
      (* pre-run measurements *)
      let gc1_minor_words, gc1_promoted_words, gc1_major_words = Gc.counters () in
      let gc1 = Gc.quick_stat () in
      let t1 = Time_now.nanosecond_counter_for_timing () in
      (* MEASURE A SINGLE SAMPLE *)
      for _ = 1 to current_runs do
        ignore (f () : (* existential type from GADT *) _)
      done;
      (* END OF MEASUREMENT *)

      (* post-run measurements *)
      let t2 = Time_now.nanosecond_counter_for_timing () in
      let gc2_minor_words, gc2_promoted_words, gc2_major_words = Gc.counters () in
      let gc2 = Gc.quick_stat () in
      total_runs := !total_runs + current_runs;
      (* reset the old Gc now that we are done with measurements *)
      Gc.set old_gc;
      (* save measurements *)
      let s = results.(current_index) in
      s.M.runs <- current_runs;
      s.M.cycles <- Int63.zero;
      s.M.nanos <- Int63.(t2 - t1);
      s.M.minor_allocated
      <- Float.iround_towards_zero_exn (gc2_minor_words -. gc1_minor_words);
      s.M.major_allocated
      <- Float.iround_towards_zero_exn (gc2_major_words -. gc1_major_words);
      s.M.promoted
      <- Float.iround_towards_zero_exn (gc2_promoted_words -. gc1_promoted_words);
      s.M.compactions <- gc2.Gc.Stat.compactions - gc1.Gc.Stat.compactions;
      s.M.major_collections
      <- gc2.Gc.Stat.major_collections - gc1.Gc.Stat.major_collections;
      s.M.minor_collections
      <- gc2.Gc.Stat.minor_collections - gc1.Gc.Stat.minor_collections;
      incr index;
      (* determine the next number of runs *)
      let next =
        match RC.sampling_type run_config with
        | `Linear k -> current_runs + k
        | `Geometric scale ->
          let next_geometric =
            Float.iround_towards_zero_exn (Float.of_int current_runs *. scale)
          in
          Int.max next_geometric (current_runs + 1)
      in
      (* if [next] would put us over the quota, we decrease as necessary *)
      let next = Int.min next (quota_max_count - !total_runs) in
      assert (next >= 0);
      (* otherwise the loop guard is broken *)
      runs := next
    done;
    let end_time = Time_float.now () in
    (* END OF MAIN TEST LOOP *)
    let total_samples = !index in
    let largest_run = !runs in
    let measurement =
      Measurement.create
        ~name:(Test.Basic_test.name test)
        ~test_name:(Test.Basic_test.test_name test)
        ~file_name:(Test.Basic_test.file_name test)
        ~module_name:(Test.Basic_test.module_name test)
        ~largest_run
        ~samples:(Array.sub results ~pos:0 ~len:total_samples)
    in
    Verbosity.print_high
      "%s: Total time taken %s (%d samples, max runs %d).\n%!"
      (Test.Basic_test.name test)
      (Time_float.Span.to_string (Time_float.diff end_time init_t1))
      total_samples
      largest_run;
    (* if (RC.save_sample_data run_config)
     * then M.save test ~results total_samples; *)
    measurement
;;

(* Run multiple benchmarks and aggregate the results. If forking is enabled then this
   function will fork and run each benchmark in a new child process. *)
let measure_all ?postprocess run_config tests =
  let postprocess =
    match postprocess with
    | None -> Fn.id
    | Some postprocess ->
      fun measurement ->
        postprocess measurement;
        measurement
  in
  Random.self_init () ~allow_in_tests:true;
  let module RC = Run_config in
  Verbosity.set_verbosity (RC.verbosity run_config);
  (match RC.quota run_config with
   | Num_calls trials ->
     Verbosity.print_low
       "Estimated testing time unknown (%d benchmarks x %d trials). Change using '-quota'.\n\
        %!"
       (List.length tests)
       trials
   | Span span ->
     let est_time = Time_float.Span.scale span (Float.of_int (List.length tests)) in
     Verbosity.print_low
       "Estimated testing time %s (%d benchmarks x %s). Change using '-quota'.\n%!"
       (Time_float.Span.to_string est_time)
       (List.length tests)
       (Time_float.Span.to_string span));
  List.map tests ~f:(fun test -> test |> measure run_config |> postprocess)
;;
