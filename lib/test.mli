(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core.Std

module Id : Unique_id.Id

module Basic_test : sig
  type t = {
    test_id : Id.t;
    name : string;
    key : int;
    arg : int option;
    group_key : int option;
    f : unit -> unit;
  }

  val test_id : t -> Id.t
  val name : t -> string
  val key : t -> int
  val arg : t -> int option
  val group_key : t -> int option
  val f : t -> unit -> unit

  val create :
    name:string ->
    ?group_key:int option ->
    ?arg:int option -> key:int -> (unit -> unit) -> t

  val make_filename : t -> string
end

type t

val name  : t -> string
val tests : t -> Basic_test.t list

val expand : t list -> Basic_test.t list

val create : name:string -> ?key:int -> (unit -> unit) -> t

val create_indexed
  :  name:string
  -> args:int list
  -> ?key:int
  -> (int -> (unit -> unit) Staged.t)
  -> t

val create_group
  :  name:string
  -> t list
  -> t



