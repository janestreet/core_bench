open! Core.Std
open! Async.Std

module Revision : sig
  type t

  include Stringable.S with type t := t
end

module Revision_info : sig
  type t

  val revision      : t -> Revision.t
  val revision_time : t -> Time.t
end

(** Store the given benchmarks into ElasticSearch *)
val upload_benchmarks
  :  elasticsearch_url : string
  -> benchmarks        : Core_bench.Simplified_benchmark.Results.t
  -> unit Deferred.t

(** Get the most recent revision for a given library  *)
val get_most_recent_revision
  :  ?except           : string list
  -> library_name      : string
  -> elasticsearch_url : string
  -> unit
  -> Revision.t Deferred.t

val get_second_most_recent_revision
  :  library_name      : string
  -> elasticsearch_url : string
  -> Revision.t Deferred.t

(** Get a list of available revision ids from a given library *)
val get_all_revisions
  :  ?last_n           : int
  -> library_name      : string
  -> elasticsearch_url : string
  -> unit
  -> Revision_info.t list Deferred.t

(** Gets all the available libraries for a given revision id in ElasticSearch  *)
val get_all_library_names
  :  revision          : string
  -> elasticsearch_url : string
  -> string list Deferred.t

(** Gets the benchmark results for the given parameters  *)
val get_benchmarks_from_revisions
  :  revisions            : string list
  -> full_benchmark_names : string list
  -> library_name         : string
  -> elasticsearch_url    : string
  -> Core_bench.Simplified_benchmark.Result.t list Deferred.t

(** [get_total_count] returns the total number of benchmark data results for a given
    benchmark and library name, i.e. the number of revisions that a given benchmark is
    available in. *)
val get_number_of_saved_benchmarks
  :  ?revisions          : string list
  -> library_name        : string
  -> full_benchmark_name : string
  -> elasticsearch_url   : string
  -> unit
  -> int Deferred.t

(** [get_valid_count] returns the number of results for a given benchmark and library name
    which are >= than 1, specifically used when generating log graphs *)
val get_number_of_benchmarks_with_result_above_one
  :  ?revisions          : string list
  -> library_name        : string
  -> field               : Core_bench.Simplified_benchmark.Field_type.t
  -> full_benchmark_name : string
  -> elasticsearch_url   : string
  -> unit
  -> int Deferred.t
