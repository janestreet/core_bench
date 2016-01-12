open Core.Std
let () = Inline_benchmarks.Runner.main ~libname:(Sys.getenv_exn "BENCH_LIB")
