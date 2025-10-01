(** "Hooks" to run at various points in the benchmark process. *)
type ('benchmark_ctx, 'arg) t = ('benchmark_ctx, 'arg) Core_bench_internals.Test.Hooks.t =
  { around_benchmark : 'r. f:('benchmark_ctx @ local -> 'r) @ local once -> 'r
  (** Function run around each benchmark. The ['benchmark_ctx] argument is passed to the
      [around_measurement] function and to the benchmark itself. *)
  ; around_measurement :
      'r. 'benchmark_ctx @ local -> f:('arg @ local -> 'r) @ local once -> 'r
  (** Function run around each batch of measurements in the benchmark. the ['arg] argument
      is passed to the benchmark itself *)
  }

(** The default hooks, which do nothing and pass [unit] to the benchmark. *)
val default : ([ `init ], unit) t
