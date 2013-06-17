open Core.Std
module Ascii_table = Textutils.Ascii_table

open To_string


let debug = false

let map_array_to_list arr ~len ~f =
  List.rev (Array.foldi arr ~init:[] ~f:(fun i ls x ->
    if i < len
    then (f x)::ls
    else ls))


(*************************************************************)
(* Minimal RDTSC bindings. This will be deprecated when Time_stamp_counter is moved to
   Core.  *)
module Cycles = struct
  external now : unit -> int = "bench_rdtsc" "noalloc"
end

(*************************************************************)
(* Verbosity control                                         *)

let verbosity = ref `Low

let set_verbosity v =
  verbosity := v

let print_high s = match !verbosity with
  | `High -> printf s
  | `Low -> Printf.ifprintf stdout s

(************************************************************)
(* Simple stats and linear regression                       *)

module Rstats = struct
  type t = {
    mutable sum : float;
    mutable sqsum : float;
    mutable count : float;
  }

  let create () = {
    sum   = 0.0;
    sqsum = 0.0;
    count = 0.0;
  }

  let update_in_place ?(w=1.0) t x =
    t.sum   <- t.sum   +. w *. x;
    t.sqsum <- t.sqsum +. w *. x *. x;
    t.count <- t.count +. w

  let mean   t = t.sum   /. t.count
  let sqmean t = t.sqsum /. t.count

  let slope ~x ~y ~xy =
    let x_mean = mean x in
    (mean xy -. x_mean *. mean y)
    /. (sqmean x -. (x_mean *. x_mean))

  let _slope_through_origin ~x ~xy =
    mean xy /. sqmean x

  let slope_intercept_and_mean ~len ~get_x ~get_y =
    (* calculate the slope *)
    let x  = create () in
    let xy = create () in
    let y  = create () in
    for i = 0 to len - 1 do
      let fx = Float.of_int (get_x i) in
      let fy = Float.of_int (get_y i) in
      let w = 1.0 in
      update_in_place x  ~w fx;
      update_in_place y  ~w fy;
      update_in_place xy ~w (fx *. fy);
    done;
    let est_slope = slope ~x ~y ~xy in
    (* calculate the error *)
    let y_mean = mean y in
    let y_int = y_mean -. mean x *. est_slope  in
    est_slope, y_int, y_mean

  let slope  ~len ~get_x ~get_y =
    let (slope, _, _) = slope_intercept_and_mean ~len ~get_x ~get_y in
    slope

  let r_square ~len ~get_x ~get_y ~slope ~y_int ~y_mean =
    let stderr = create () in
    let mean_err = create () in
    for i = 0 to len - 1 do
      let fx = Float.of_int (get_x i) in
      let fy = Float.of_int (get_y i) in
      let est_y = fx *. slope +. y_int in
      update_in_place stderr (fy -. est_y);
      update_in_place mean_err (fy -. y_mean);
    done;
    1.0 -. (sqmean stderr) /. (sqmean mean_err)

  let slope_and_r_square ~len ~get_x ~get_y =
    let slope, y_int, y_mean = slope_intercept_and_mean ~len ~get_x ~get_y in
    let rsq = r_square ~len ~get_x ~get_y ~slope ~y_int ~y_mean in
    slope, rsq
end

(************************************************************)
(* Simple helper functions for stats                        *)

let sample_with_replacement arr ~samples ~max =
  for i = 0 to samples - 1 do
    arr.(i) <- Random.int max
  done

let quantile_of_array arr ~len ~low_quantile ~high_quantile =
  Array.sort arr ~cmp:Float.compare;
  let index q = Float.iround_towards_zero_exn ((Float.of_int len) *. q) in
  let low = arr.(index low_quantile) in
  let high = arr.(index high_quantile) in
  low, high

(************************************************************)
module Test = struct
  module Id : Unique_id.Id = Unique_id.Int(Unit)

  type t = {
    test_id : Id.t;
    name    : string option;
    f       : unit -> unit;
  }

  let create ?name f =
    { name; f; test_id = Id.create () }

  let name t = t.name
  let name_or_unknown t =
    Option.value ~default:"unknown" t.name
end

(************************************************************)

module Test_metrics = struct
  type t = {
    mutable runs              : int;
    mutable cycles            : int;
    mutable nanos             : int;
    mutable compactions       : int;
    mutable minor_allocated   : int;
    mutable major_allocated   : int;
    mutable promoted          : int;
    mutable major_collections : int;
    mutable minor_collections : int;
  } with fields

  let create () = {
    runs              = 0;
    cycles            = 0;
    nanos             = 0;
    compactions       = 0;
    minor_allocated   = 0;
    major_allocated   = 0;
    promoted          = 0;
    major_collections = 0;
    minor_collections = 0;
  }

  (* slope estimates total cost *)
  let slope stats ~len ~field =
    Rstats.slope ~len
      ~get_x:(fun i -> stats.(i).runs)
      ~get_y:(fun i -> field stats.(i))

  (* compute both the slope and the standard error *)
  let slope_and_r_square stats ~len ~field =
    Rstats.slope_and_r_square ~len
      ~get_x:(fun i -> stats.(i).runs)
      ~get_y:(fun i -> field stats.(i))

  (* gc_free_slope estimates nominal cost -- i.e. the slope of runs that don't incur a
     major GC. See *)
  let gc_free_slope stats ~len ~field =
    let valid_samples = ref 0 in
    let samples = Array.create ~len 0 in
    for i = 0 to len - 1 do
      if stats.(i).major_collections = 0 then begin
        samples.(!valid_samples) <- i;
        incr valid_samples;
      end
    done;
    (* if there are too few samples, then we can't report this. *)
    if !valid_samples < 10
    then None
    else Some (Rstats.slope ~len:!valid_samples
                 ~get_x:(fun i -> stats.(samples.(i)).runs)
                 ~get_y:(fun i -> field stats.(samples.(i))))

  let max stats ~len ~field =
    let x = field stats.(0) in
    let rec loop i x =
      if i < len
      then loop (i+1) (Int.max x (field stats.(i)))
      else x
    in loop 1 x

  let bootstrap ?(bootstrap_trials=2000) ?(save_file="bootstrap.txt") stats ~len ~field =
    let est = Array.create ~len:bootstrap_trials 0.0 in
    let est_err = Array.create ~len:bootstrap_trials 0.0 in
    let selected_indexes = Array.create ~len 0 in
    for i = 0 to bootstrap_trials - 1 do
      sample_with_replacement selected_indexes ~samples:len ~max:len;
      let v, err = Rstats.slope_and_r_square ~len
        ~get_x:(fun i -> stats.(selected_indexes.(i)).runs)
        ~get_y:(fun i -> field stats.(selected_indexes.(i))) in
      est.(i) <- v;
      est_err.(i) <- err;
    done;
    if debug then begin
      printf "Saving bootstrap data to: %s\n%!" save_file;
      Out_channel.write_lines save_file
        (map_array_to_list est ~len ~f:(fun x -> sprintf "%.4f" x));
    end;
    (* do a quantile estimation between 2.5% and 97.5% *)
    let low, high =
      quantile_of_array est ~len:bootstrap_trials
        ~low_quantile:0.025 ~high_quantile:0.975 in
    let low_err, high_err =
      quantile_of_array est_err ~len:bootstrap_trials
        ~low_quantile:0.025 ~high_quantile:0.975 in
    ((low, high), (low_err, high_err))
end

(***************************************************************************************)
(* Printing functions : all of these functions are related to the table display which is
   the main output of bench. *)

(* Memoize *)
let memoize_by_test_id f =
  let cache = Test.Id.Table.create () in
  (fun (test, results, len) ->
    match Hashtbl.find cache test.Test.test_id with
    | Some v -> v
    | None ->
      let v = f (test, results, len) in
      Hashtbl.set cache ~key:test.Test.test_id ~data:v;
      v)

(* Get the value of each column type and format *)
let get_slope ~field =
  memoize_by_test_id (fun (_test, results, len) ->
    (Test_metrics.slope ~field ~len results))

let get_slope_1k ~field =
  memoize_by_test_id (fun (_test, results, len) ->
    (Test_metrics.slope ~field ~len results) *. 1000.0)

let get_gc_free_slope ~field =
  memoize_by_test_id (fun (_test, results, len) ->
    (Test_metrics.gc_free_slope ~field ~len results))

let get_value_and_r_square field =
  memoize_by_test_id (fun (_test, results, len) ->
    Test_metrics.slope_and_r_square ~field ~len results)

let get_value_and_r_square_ci95 field fieldname =
  memoize_by_test_id (fun (test, results, len) ->
    let save_file = Test.name_or_unknown test ^ "-" ^ fieldname ^ "-bootstrap.txt" in
    Test_metrics.bootstrap ~save_file ~field ~len results)

(* formatting functions for displaying different columns *)
let make_name (test, _results, _len) =
  Test.name_or_unknown test

let make_samples =
  memoize_by_test_id (fun (_test, results, len) ->
    let samples = Int.to_string len in
    let max_runs = float_to_string
      (Float.of_int
         (Test_metrics.max ~field:Test_metrics.runs ~len results)) in
    sprintf "%s/%s" max_runs samples)

(* formatting functions for gc and memory related stats *)
let make_minor_allocated   = compose_float_to_string
  (get_slope ~field:Test_metrics.minor_allocated)
let make_major_allocated   = compose_float_to_string
  (get_slope ~field:Test_metrics.major_allocated)
let make_promoted          = compose_float_to_string
  (get_slope ~field:Test_metrics.promoted)
let make_minor_collections = compose_float_to_string
  (get_slope_1k ~field:Test_metrics.minor_collections)
let make_major_collections = compose_float_to_string
  (get_slope_1k ~field:Test_metrics.major_collections)
let make_compactions = compose_float_to_string
  (get_slope_1k ~field:Test_metrics.compactions)

(* formatting functions for nominal timing *)
let make_nominal_cycles = compose_float_opt_to_string
  (get_gc_free_slope ~field:Test_metrics.cycles)
let make_nominal_nanos = compose_float_opt_to_string
  (get_gc_free_slope ~field:Test_metrics.nanos)


let make_spread ~low_high:(low, high) ~mid =
  format_plus_or_minus (high -. mid, low -. mid)

(* formatting functions for total timing in terms of cycles *)
let get_value_and_r_square_cycles = get_value_and_r_square Test_metrics.cycles
let get_value_and_r_square_95ci_cycles = get_value_and_r_square_ci95 Test_metrics.cycles "cycles"

let make_cycles arg =
  float_to_string (fst (get_value_and_r_square_cycles arg))
let make_cycles_95ci arg =
  make_spread ~mid:(fst (get_value_and_r_square_cycles arg))
    ~low_high:(fst (get_value_and_r_square_95ci_cycles arg))
let make_cycles_error arg =
  float_to_string (snd (get_value_and_r_square_cycles arg))
let make_cycles_error_95ci arg =
  make_spread ~mid:(snd (get_value_and_r_square_cycles arg))
    ~low_high:(snd (get_value_and_r_square_95ci_cycles arg))


(* formatting functions for total timing in terms of time in nanos *)
let get_value_and_r_square_nanos = get_value_and_r_square Test_metrics.nanos
let get_value_and_r_square_95ci_nanos = get_value_and_r_square_ci95 Test_metrics.nanos "nanos"

let make_nanos arg =
  float_to_string (fst (get_value_and_r_square_nanos arg))
let make_nanos_95ci arg =
  make_spread ~mid:(fst (get_value_and_r_square_nanos arg))
    ~low_high:(fst (get_value_and_r_square_95ci_nanos arg))
let make_nanos_error arg =
  float_to_string (snd (get_value_and_r_square_nanos arg))
let make_nanos_error_95ci arg =
  make_spread ~mid:(snd (get_value_and_r_square_nanos arg))
    ~low_high:(snd (get_value_and_r_square_95ci_nanos arg))



(* formatting functions for reporting relative speed *)
let get_cycles args =
  fst (get_value_and_r_square_cycles args)

let make_percentage max_cycles args =
  let cycles = get_cycles args in
  let perc = 100.0 *. cycles /. max_cycles in
  sprintf "%0.2f" perc

let make_speedup max_cycles args =
  let cycles = get_cycles args in
  let perc = max_cycles /. cycles in
  sprintf "%0.2f" perc


(***************************************************************)

module Column = struct
  type t =
    [ `Name
    | `Cycles
    | `Nanos
    | `Nominal_cycles
    | `Nominal_nanos
    | `Bootstrap_cycles
    | `Bootstrap_nanos
    | `Allocated
    | `Percentage
    | `GC
    | `Speedup
    | `Samples
    ] with sexp, compare

  let name_desc_assoc_list =
    [("name"      , `Name            , "Name of the test.");
     ("cycles"    , `Cycles          , "Number of CPU cycles (RDTSC) taken.");
     ("cycles-err", `Bootstrap_cycles, "95% confidence interval and R^2 error for cycles.");
     ("~cycles"   , `Nominal_cycles  , "Cycles taken excluding major GC costs.");
     ("time"      , `Nanos           , "Number of nano secs taken.");
     ("time-err"  , `Bootstrap_nanos , "95% confidence interval and R^2 error for time.");
     ("~time"     , `Nominal_nanos   , "Time (ns) taken excluding major GC costs.");
     ("alloc"     , `Allocated       , "Allocation of major, minor and promoted words.");
     ("gc"        , `GC              , "Show major and minor collections per 1000 runs.");
     ("percentage", `Percentage      , "Relative execution time as a percentage.");
     ("speedup"   , `Speedup         , "Relative execution cost as a speedup.");
     ("samples"   , `Samples         , "Number of samples collected for profiling.");
    ]

  let name_assoc_list =
    List.map name_desc_assoc_list ~f:(fun (name, tag, _) -> (name, tag))

  let column_description_table =
    let max =
      let length (str, _, _) = String.length str in
      List.reduce_exn ~f:Int.max
        (List.map name_desc_assoc_list ~f:length)
    in
    let extend x =
      let slack = max - String.length x in
      x ^ String.make slack ' '
    in
    String.concat ~sep:"\n\t"
      (List.map name_desc_assoc_list ~f:(fun (name, _, desc) ->
        sprintf "%s - %s"
          (extend name) desc))

  let of_string col =
    let col, non_empty =
      match String.chop_prefix col ~prefix:"+" with
      | Some col -> col, true
      | None -> col, false
    in
    let col =
      match (List.Assoc.find ~equal:String.equal name_assoc_list col) with
      | Some col -> col
      | None -> failwithf "Invalid column name: %s" col ()
    in
    if non_empty
    then `If_not_empty col
    else col

  let arg = Command.Spec.Arg_type.create of_string
end

module CMap = Map.Make (struct
  type t = Column.t with sexp
  let compare = Column.compare_t
end)

let print
    ?limit_width_to
    ?display
    ~columns
    data =
  let left, right = Ascii_table.Align.(left, right) in
  (* Map displayed columns to `If_not_empty or `Yes. *)
  let displayed =
    List.fold columns ~init:CMap.empty ~f:(fun cmap column ->
      match column with
      | `If_not_empty c -> CMap.add cmap ~key:c ~data:`If_not_empty
      | #Column.t as c  -> CMap.add cmap ~key:c ~data:`Yes)
  in
  let max_cycles =
    if CMap.mem displayed `Percentage || CMap.mem displayed `Speedup
    then List.reduce_exn (List.map data ~f:get_cycles) ~f:Float.max
    else 0.0
  in
  let col tag name make align =
    let show = Option.value (CMap.find displayed tag) ~default:`No in
    (* Ascii_table calls [make] even if show = `No, which results in a lot of wasted
       computation in this case.  Thus we redefine [make] here. *)
    let make =
      match show with
      | `No -> (fun _ -> "")
      | `Yes | `If_not_empty -> make
    in
    Ascii_table.Column.create name make ~align ~show
  in
  let columns = [
    col `Name "Name" make_name left;
    col `Cycles "Cycles" make_cycles right;
    col `Bootstrap_cycles "95% ci" make_cycles_95ci right;
    col `Bootstrap_cycles "Cycles R^2" make_cycles_error right;
    col `Bootstrap_cycles "95% ci" make_cycles_error_95ci right;
    col `Nominal_cycles "~Cycles" make_nominal_cycles right;
    col `Nanos "Time (ns)" make_nanos right;
    col `Bootstrap_nanos "95% ci" make_nanos_95ci right;
    col `Bootstrap_nanos "Time R^2" make_nanos_error right;
    col `Bootstrap_nanos "95% ci" make_nanos_error_95ci right;
    col `Nominal_nanos "~Time" make_nominal_nanos right;
    col `Allocated "Minor" make_minor_allocated right;
    col `Allocated "Major" make_major_allocated right;
    col `Allocated "Promoted" make_promoted right;
    col `GC "Minor GCs" make_minor_collections right;
    col `GC "Major GCs" make_major_collections right;
    col `GC "Compactions" make_compactions right;
    col `Samples "Runs/Samples" make_samples right;
    col `Percentage "% of max" (make_percentage max_cycles) right;
    col `Speedup "Speedup" (make_speedup max_cycles) right;
  ] in
  Ascii_table.output ?display ~oc:stdout ?limit_width_to columns data

(**************************************************************)

let write_data_array filename ~results max_used =
  let ls = List.rev (Array.foldi results ~init:[] ~f:(fun i ls _ ->
    if i < max_used then
      let line = sprintf "%d %d %d %d"
        results.(i).Test_metrics.runs
        results.(i).Test_metrics.minor_collections
        results.(i).Test_metrics.major_collections
        results.(i).Test_metrics.nanos
        (* add more fields here on a need basis *)
      in
      line :: ls
    else
      ls)) in
  Out_channel.write_lines filename ls


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

(*******************************************************************************)
(* The heart of bench : This is where all the timing and measurements happen. *)

let bench_basic =
  fun ~stabilize_gc_between_runs
    ~sampling_type
    ~save_sample_data
    ~time_quota
    ~gc_prefs:_
    ~no_compactions
    test
  ->
  (* test function *)
  let f = test.Test.f in

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
  print_high "%s: Total time taken %s (%d samples, max runs %d).\n%!"
    (Test.name_or_unknown test)
    (Time.Span.to_string (Time.diff end_time init_t1))
    total_samples
    largest_run;
  if save_sample_data then begin
    let filename = Option.value_exn test.Test.name ^ ".txt" in
    print_high "%s: Writing %d samples to file: %s.%!\n"
      (Test.name_or_unknown test) total_samples filename;
    write_data_array filename ~results total_samples;
  end;
  results, total_samples

(***************************************************************)

module Defaults = struct

  (* default columns *)
  let columns_as_string = [
    "+name";
    "time";
    "percentage";
  ]
  let columns = List.map ~f:Column.of_string columns_as_string

  (* how to measure *)
  let geometric_scale   = 1.01
  let stabilize_gc_between_runs = false
  let no_compactions = false

  (* how long to measure *)
  let time_quota_float = 10.0
  let time_quota       = Time.Span.of_sec time_quota_float

  (* saving generated data *)
  let save_sample_data = false

  (* width of the output table *)
  let limit_width_to = 150

  (* default display *)
  let display_as_string = "short"
  let string_to_display =
    let module Display = Ascii_table.Display in
    function
    | "short" -> Display.short_box
    | "tall"  -> Display.tall_box
    | "line"  -> Display.line
    | "blank" -> Display.blank
    | s -> failwithf "Invalid display name: %s" s ()
  let display = string_to_display display_as_string
end

(*****************************************************************************)

let run_benchmarks
    ?(verbosity=`Low)
    ?gc_prefs
    ?(no_compactions=Defaults.no_compactions)
    ?(save_sample_data=Defaults.save_sample_data)
    ?(time_quota=Defaults.time_quota)
    ?(sampling_type=`Geometric Defaults.geometric_scale)
    ?(stabilize_gc_between_runs=Defaults.stabilize_gc_between_runs)
    tests =
  Random.self_init ();
  set_verbosity verbosity;
  let est_time = Time.Span.of_float
    ((Time.Span.to_float time_quota) *. (Float.of_int (List.length tests))) in
  printf "Estimated testing time %s (change using -quota SECS).\n%!"
    (Time.Span.to_string est_time);
  List.map tests ~f:(fun test ->
    let results, len =
      bench_basic
        ~gc_prefs
        ~no_compactions
        ~save_sample_data
        ~time_quota
        ~sampling_type
        ~stabilize_gc_between_runs
        test
    in
    test, results, len)
;;

(*****************************************************************************)
(* User facing bench function                                                *)

let bench
    (* printing parameters *)
    ?(limit_width_to=Defaults.limit_width_to)
    ?(columns=Defaults.columns)
    ?(display=Defaults.display)
    (* benchmarking parameters *)
    ?verbosity
    ?no_compactions
    ?save_sample_data
    ?time_quota
    ?sampling_type
    ?stabilize_gc_between_runs
    tests =
  print ~limit_width_to ~columns ~display
    (run_benchmarks
       ?verbosity
       ?no_compactions
       ?save_sample_data
       ?time_quota
       ?sampling_type
       ?stabilize_gc_between_runs
       tests)
;;

(*****************************************************************************)
(* Command maker                                                             *)

module Command = struct

  (* This type is written out here so that if Bench ever changes its interface, this will
     throw a type error. If you edit the type below, also edit the command generation code
     below. *)
  let bench
    :  ?limit_width_to:int
    -> ?columns:[ Column.t | `If_not_empty of Column.t ] list
    -> ?display:Ascii_table.Display.t
    -> ?verbosity:[ `High | `Low ]
    -> ?no_compactions:bool
    -> ?save_sample_data:bool
    -> ?time_quota:Time.Span.t
    -> ?sampling_type:[`Geometric of float | `Linear of int]
    -> ?stabilize_gc_between_runs:bool
    -> Test.t list
    -> unit
    = bench

  let readme () = sprintf "\
Columns that can be specified are:
\t%s

R^2 error indicates how noisy the benchmark data is. A value of
1.0 means the amortized cost of benchmark is almost exactly predicated
and 0.0 means the reported values are not reliable at all.
Also see: http://en.wikipedia.org/wiki/Coefficient_of_determination

Major and Minor GC stats indicate how many collections happen per 1000
runs of the benchmarked function.

The following columns will be displayed by default:
\t%s

To specify that a column should be displayed only if it has a non-trivial value,
prefix the column name with a '+'."
    Column.column_description_table
    (String.concat ~sep:" " Defaults.columns_as_string)

  let make tests =
    Command.basic
      ~summary:(sprintf "Benchmark for %s"
                  (String.concat ~sep:", "
                     (List.map tests ~f:Test.name_or_unknown)))
      ~readme
      Command.Spec.(
        (* flags *)
        empty
        +> flag "-width" (optional_with_default Defaults.limit_width_to int)
          ~doc:(sprintf "WIDTH width limit on column display (default %d)."
                  Defaults.limit_width_to)
        +> flag "-display" (optional_with_default Defaults.display_as_string string)
          ~doc:(sprintf "STYLE Table style (short, tall, line or blank). Default %s."
                  Defaults.display_as_string)
        +> flag "-v" no_arg ~doc:" High verbosity level."
        +> flag "-quota" (optional_with_default Defaults.time_quota_float float)
          ~doc:(sprintf "SECS Time quota allowed per test (default %s)."
                  (Time.Span.to_string Defaults.time_quota))
        +> flag "-no-compactions" no_arg ~doc:" Disable GC compactions."
        ++ step (fun m linear geometric ->
          let sampling_type =
            match linear, geometric with
            | None, None -> `Geometric Defaults.geometric_scale
            | None, Some s -> `Geometric s
            | Some k, None -> `Linear k
            | Some _, Some _ ->
              failwith "Cannot specify both -linear and -geometric"
          in
          m ~sampling_type)
        +> flag "-linear" (optional int)
          ~doc:"INCREMENT Use linear sampling to explore number of runs, example 1."
        +> flag "-geometric" (optional float)
          ~doc:(sprintf "SCALE Use geometric sampling. (default %0.2f)"
                  Defaults.geometric_scale)
        +> flag "-save" no_arg ~doc:" Save benchmark data to <test name>.txt files."
        +> flag "-stabilize-gc" no_arg ~doc:" Stabilize GC between each sample capture."
        +> flag "-clear-columns" no_arg ~doc:" Don't display default columns. Only show \
        user specified ones."
        +> anon (sequence ("COLUMN" %: Column.arg))
      )
      (fun limit_width_to display verbosity time_quota no_compactions ~sampling_type
          save_sample_data stabilize_gc_between_runs
          clear_columns anon_columns () ->
        let display = Defaults.string_to_display display in
        let verbosity =
          if verbosity
          then `High
          else `Low
        in
        let columns =
          if clear_columns
          then []
          else Defaults.columns
        in
        let columns = columns @ anon_columns in
        bench
          ~limit_width_to
          ~columns
          ~display
          ~verbosity
          ~time_quota:(Time.Span.of_float time_quota)
          ~sampling_type
          ~save_sample_data
          ~stabilize_gc_between_runs
          ~no_compactions
          tests
      )
end

let make_command tests = Command.make tests


(* Older CRs are moved down here: *)

