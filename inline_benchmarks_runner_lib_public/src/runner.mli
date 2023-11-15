open! Core

(** [main] is the function that is called by the [inline_benchmarks_runner.exe] generated
    per library.  *)
val main : libname:string -> unit

val command : libname:string -> Command.t
