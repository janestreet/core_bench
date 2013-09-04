(* This module is the heart of bench : This is where all the timing and measurements
   happen. *)
open Core.Std

(* Minimal RDTSC bindings. This will be deprecated when Time_stamp_counter is moved to
   Core.  *)
module Cycles = struct
  external now : unit -> int = "bench_rdtsc" "noalloc"
end

let stabilize_gc () =
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0 then
      failwith "unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.Stat.live_words <> last_heap_live_words
    then loop (failsafe - 1) stat.Gc.Stat.live_words
  in
  loop 10 0


let exceeded_allowed_time allowed_time_span t1 =
  let t2 = Time.now () in
  Time.diff t2 t1 > allowed_time_span


(* The main benchmarking function *)
let run_one_benchmark =
  fun ~stabilize_gc_between_runs
    ~sampling_type
    ~save_sample_data
    ~time_quota
    ~no_compactions
    test
  ->
  (* test function *)
  let f = test.Test.Basic_test.f in

  (* the samples *)
  let max_samples = 3_000 in
  let results = Array.init max_samples ~f:(fun _ -> Test_metrics.create ()) in

  (* counters *)
  let index = ref 0 in
  let runs = ref 0 in

  (* get the old Gc settings *)
  let old_gc = Gc.get () in

  (* THE MAIN TEST LOOP *)
  let init_t1 = Time.now () in
  while not (exceeded_allowed_time time_quota init_t1) && !index < Array.length results do
    let current_runs = !runs in
    let current_index = !index in

    (* Stabilize gc if required.

       We stabilize the gc through the first pass through this loop anyway. If we don't do
       this the incoming GC state (some data may be on the minor heap that is partly full)
       will cause an early collection or two which will not happen subsequently. These
       early collections are just noise.

       While benchmarking functions that do not allocate any memory this early noise is
       the only significant input. In these cases, these spurious early collections will
       give the allocation stats (major and promoted words) a slight negative value. *)
    if stabilize_gc_between_runs || current_runs = 0 then
      stabilize_gc ();

    (* make any Gc changes required. *)
    if no_compactions then Gc.set { (Gc.get ()) with Gc.Control.max_overhead = 1_000_000 };

    (* pre-run measurements *)
    let gc1 = Gc.quick_stat () in
    let t1 = Time.now () in
    let c1 = Cycles.now () in

    (* MEASURE A SINGLE SAMPLE *)
    for _i = 1 to current_runs do
      f ();
    done;
    (* END OF MEASUREMENT *)

    (* post-run measurements *)
    let c2 = Cycles.now () in
    let t2 = Time.now () in
    let gc2 = Gc.quick_stat () in

    (* reset the old Gc now that we are done with measurements *)
    Gc.set old_gc;

    (* save measurements *)
    let s = results.(current_index) in
    s.Test_metrics.runs  <- current_runs;
    s.Test_metrics.cycles  <- c2 - c1;
    s.Test_metrics.nanos  <- (Float.iround_towards_zero_exn
                          (Time.Span.to_ns (Time.diff t2 t1)));
    s.Test_metrics.minor_allocated <- Float.iround_towards_zero_exn
      (gc2.Gc.Stat.minor_words -. gc1.Gc.Stat.minor_words);
    s.Test_metrics.major_allocated <- Float.iround_towards_zero_exn
      (gc2.Gc.Stat.major_words -. gc1.Gc.Stat.major_words);
    s.Test_metrics.promoted <- Float.iround_towards_zero_exn
      (gc2.Gc.Stat.promoted_words -. gc1.Gc.Stat.promoted_words);
    s.Test_metrics.compactions <-
      (gc2.Gc.Stat.compactions - gc1.Gc.Stat.compactions);
    s.Test_metrics.major_collections <-
      (gc2.Gc.Stat.major_collections - gc1.Gc.Stat.major_collections);
    s.Test_metrics.minor_collections <-
      (gc2.Gc.Stat.minor_collections - gc1.Gc.Stat.minor_collections);
    incr index;

    (* determine the next number of runs *)
    let next =
      match sampling_type with
      | `Linear k -> current_runs + k
      | `Geometric scale ->
        let next_geometric =
          Float.iround_towards_zero_exn ((Float.of_int current_runs) *. scale) in
        Int.max next_geometric (current_runs + 1)
    in
    runs := next;

  done;
  let end_time = Time.now () in
  (* END OF MAIN TEST LOOP *)

  let total_samples = !index in
  let largest_run = !runs in
  Verbosity.print_high "%s: Total time taken %s (%d samples, max runs %d).\n%!"
    (Test.Basic_test.name test)
    (Time.Span.to_string (Time.diff end_time init_t1))
    total_samples
    largest_run;
  if save_sample_data
  then Test_metrics.save test ~results total_samples;
  results, total_samples


(* Run multiple benchmarks and aggregate the results. If forking is enabled then this
   function will fork and run each benchmark in a new child process. *)
let run_benchmarks
    ?(verbosity=`Low)
    ?(no_compactions=Defaults.no_compactions)
    ?(save_sample_data=Defaults.save_sample_data)
    ?(time_quota=Defaults.time_quota)
    ?(sampling_type=`Geometric Defaults.geometric_scale)
    ?(stabilize_gc_between_runs=Defaults.stabilize_gc_between_runs)
    ?(fork_each_benchmark=Defaults.fork_each_benchmark)
    tests =
  Random.self_init ();
  Verbosity.set_verbosity verbosity;
  let est_time = Time.Span.of_float
    ((Time.Span.to_float time_quota) *. (Float.of_int (List.length tests))) in
  printf "Estimated testing time %s (change using -quota SECS).\n%!"
    (Time.Span.to_string est_time);
  if (fork_each_benchmark) then
    let fds = List.map tests ~f:(fun _ -> Unix.pipe ()) in
    let () =
      Caml.List.iter2 (fun test (_fdr, fdw) ->
        match Caml.Unix.fork () with
        | 0 ->
          let x =
            run_one_benchmark
              ~no_compactions
              ~save_sample_data
              ~time_quota
              ~sampling_type
              ~stabilize_gc_between_runs
              test
          in
          let open Caml in
          let oc = Unix.out_channel_of_descr fdw in
          Marshal.to_channel oc x [];
          exit 0
        | pid ->
          ignore (Caml.Unix.waitpid [] pid)) tests fds
    in
    List.map2_exn tests fds ~f:(fun test (fdr, _fdw) ->
      let open Caml in
      let ic = Unix.in_channel_of_descr fdr in
      let results, len = Marshal.from_channel ic in
      test, results, len)
  else
    List.map tests ~f:(fun test ->
      let results, len =
        run_one_benchmark
          ~no_compactions
          ~save_sample_data
          ~time_quota
          ~sampling_type
          ~stabilize_gc_between_runs
          test
      in
      test, results, len)


