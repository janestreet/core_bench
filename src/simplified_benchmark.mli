(** A module internal to [Core_Bench].

    Converts an analysis result to a simplified json representation *)

val to_json : ?libname:string -> Analysis_result.t list -> Json_wheel_internal.Std.Json_type.t

val to_elastic_bulk_format : ?libname:string -> Analysis_result.t list -> string
