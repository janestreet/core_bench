(** A module internal to [Core_bench]. Please look at {!Bench}. *)

open! Core
module Id : Unique_id.Id

module Hooks : sig
  (** "Hooks" to run at various points in the benchmark process. *)
  type ('benchmark_ctx, 'arg) t =
    { around_benchmark : 'r. f:('benchmark_ctx -> 'r) -> 'r
    (** Function run around each benchmark. The ['benchmark_ctx] argument is passed to the
        [around_measurement] function and to the benchmark itself. *)
    ; around_measurement : 'r. 'benchmark_ctx -> f:('arg -> 'r) -> 'r
    (** Function run around each batch of measurements in the benchmark. the ['arg]
        argument is passed to the benchmark itself *)
    }

  (** The default hooks, which do nothing, use [`init] as the measurement context, and
      pass [unit] to the benchmark. *)
  val default : ([ `init ], unit) t
end

module Basic_test : sig
  type packed_f =
    | T :
        { hooks : ('benchmark_ctx, 'arg) Hooks.t
        ; f : 'benchmark_ctx -> ('arg -> 'r) Staged.t
        }
        -> packed_f

  type t =
    { test_id : Id.t
    ; name : string
    ; test_name : string
    ; file_name : string
    ; module_name : string
    ; key : int
    ; arg : string option
    ; group_key : int option
    ; f : packed_f
    }

  val create_with_initialization
    :  name:string
    -> ?test_name:string
    -> ?file_name:string
    -> ?module_name:string
    -> ?group_key:int option
    -> ?arg:string option
    -> key:int
    -> hooks:('benchmark_ctx, 'arg) Hooks.t
    -> ('benchmark_ctx -> 'arg -> unit)
    -> t

  val test_id : t -> Id.t
  val name : t -> string
  val test_name : t -> string
  val file_name : t -> string
  val module_name : t -> string
  val key : t -> int

  (** a name describing this arg *)
  val arg : t -> string option

  val group_key : t -> int option
  val f : t -> packed_f
  val make_filename : t -> string
end

type t

val name : t -> string
val test_name : t -> string
val file_name : t -> string
val module_name : t -> string
val tests : t -> Basic_test.t list
val expand : t list -> Basic_test.t list

val create
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> ?key:int
  -> (unit -> 'a)
  -> t

val create_with_hooks
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> ?key:int
  -> hooks:([< `init ], 'arg) Hooks.t
  -> ('arg -> 'r)
  -> t

val create_with_initialization
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> ?key:int
  -> ([ `init ] -> unit -> 'a)
  -> t

val create_with_initialization_and_hooks
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> ?key:int
  -> hooks:('benchmark_ctx, 'arg) Hooks.t
  -> ('benchmark_ctx -> 'arg -> 'r)
  -> t

val create_parameterised
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> args:(string * 'param) list
  -> ?key:int
  -> ('param -> (unit -> 'a) Staged.t)
  -> t

val create_parameterised_with_hooks
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> args:(string * 'param) list
  -> ?key:int
  -> hooks:('benchmark_ctx, 'arg) Hooks.t
  -> ('param -> 'benchmark_ctx -> ('arg -> 'r) Staged.t)
  -> t

val create_indexed
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> args:int list
  -> ?key:int
  -> (int -> (unit -> 'a) Staged.t)
  -> t

val create_indexed_with_hooks
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> args:int list
  -> ?key:int
  -> hooks:('benchmark_ctx, 'arg) Hooks.t
  -> (int -> 'benchmark_ctx -> ('arg -> 'r) Staged.t)
  -> t

val create_group
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> t list
  -> t
