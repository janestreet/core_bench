open! Core.Std
open Import
(** A module internal to [Core_Bench].

    Converts an analysis result to a simplified json representation *)

module Result : sig
  type t =
    { full_benchmark_name             : string
    ; benchmark_name                  : string
    ; file_name                       : string
    ; module_name                     : string
    ; library_name                    : string
    ; version                         : string
    ; hg_revision                     : string option
    ; x_library_inlining              : bool
    ; ocaml_version                   : string
    ; machine_where_benchmark_was_run : string
    ; epoch_time_of_run               : int
    ; time_of_hg_revision             : string option
    ; time_r_square                   : float
    ; time_per_run_nanos              : float
    ; ci95_upper_bound                : float
    ; ci95_lower_bound                : float
    ; minor_words_per_run             : float
    ; major_words_per_run             : float
    ; promoted_words_per_run          : float
    }

  val to_json : t -> Json_type.t
  val of_json : Json_type.t -> t
end

module Results : sig
  type t = Result.t list

  val to_json : t -> Json_type.t
  val of_json : Json_type.t -> t
end

val to_json
  :  ?libname : string
  -> Analysis_result.t list
  -> Json_type.t

val to_elastic_bulk_format
  : ?libname : string
  -> Analysis_result.t list
  -> string
