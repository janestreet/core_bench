(** A module internal to [Core_bench]. Please look at {!Bench}.

    Tabular display of [Analysis_result]s. *)

open! Core

module Warnings : sig
  val display : display_config:Display_config.t -> unit
end

val make_columns
  :  Display_config.Table.t
  -> Display_config.Warning_destination.t
  -> Analysis_result.t list
  -> Analysis_result.t Ascii_table_kernel.Column.t list

val make_csv_columns
  :  Display_config.Table.t
  -> Display_config.Warning_destination.t
  -> Analysis_result.t list
  -> Analysis_result.t Delimited_kernel.Write.t
