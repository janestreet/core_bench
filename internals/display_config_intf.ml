(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open! Core

module Definitions = struct
  module Table = struct
    module How_to_print = struct
      module Human_readable = struct
        type t =
          | Pretty of Ascii_table_kernel.Display.t
          | Ascii
      end

      type t =
        | Csv of { streaming : bool }
        | Human_readable of
            { table_format : Human_readable.t
            ; show_all_values : bool
            ; verbosity : Verbosity.t
            }
    end

    type t =
      { how_to_print : How_to_print.t
      ; limit_width_to : int
      ; show_absolute_ci : bool
      ; show_percentage : bool
      ; show_speedup : bool
      ; show_samples : bool
      ; show_overheads : bool
      ; max_name_length : int
      ; analysis_configs : Analysis_config.t list
      }
    [@@deriving fields ~getters]
  end

  type t =
    | Show_as_sexp
    | Show_as_table of Table.t
end

module type Display_config = sig
  include module type of struct
    include Definitions
  end

  module Table : sig
    include module type of struct
      include Table
    end

    module How_to_print : sig
      include module type of struct
        include How_to_print
      end

      module Human_readable : sig
        include module type of struct
          include Human_readable
        end

        val param : t Command.Param.t
        val display : t -> Ascii_table_kernel.Display.t
      end

      val param : t Command.Param.t
    end

    val param : t Command.Param.t
    val show_all_values : t -> bool
  end

  val default : t
  val param : t Command.Param.t
  val verbosity : t -> Verbosity.t
  val analysis_configs : t -> Analysis_config.t list
end
