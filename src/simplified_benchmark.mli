open! Core.Std
open Import
(** A module internal to [Core_Bench].

    Converts an analysis result to a simplified json representation *)

module Result : sig
  type t =
    { (* [benchmark_name] is just the user defined name when defining an inline test
       e.g. "addition test" *)
      benchmark_name                  : string
    (* [benchmark_name_with_index] is the user defined name along with the index number
       for indexed tests e.g. "addition test:1000" *)
    ; benchmark_name_with_index       : string
    (* [full_benchmark_name] includes file, module name, given name, and index
       concactenated e.g. "[test.ml] addition test:1000" *)
    ; full_benchmark_name             : string
    ; file_name                       : string
    ; module_name                     : string
    ; library_name                    : string
    (* version contains the full string received from Version_util.version
       e.g. "ssh://hg//hg/jane/submissions_1e88f63603b3" *)
    ; version                         : string
    (* hg_revision contains only the action revision id used by hg
       e.g. "1e88f63603b3" *)
    ; hg_revision                     : string option
    ; x_library_inlining              : bool
    ; ocaml_version                   : string
    (* machine_where_benchmark_was_run stores the name of the performance machine used
       for the benchmarks e.g. "igm-qwd-iperf1" *)
    ; machine_where_benchmark_was_run : string
    (* epoch_time_of_run is the epoch time of when exactly the benchmarks were run,
       in nanoseconds *)
    ; epoch_time_of_run               : int
    (* time_of_hg_revision is the time at which the hg revision was created *)
    ; time_of_hg_revision             : string option
    (* Various stats computed by bench. *)
    ; time_r_square                   : float
    ; time_per_run_nanos              : float
    ; ci95_upper_bound   : float
    ; ci95_lower_bound   : float
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
