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

  module Benchmark_display = struct
    type t =
      | Show_as_sexp
      | Show_as_table of Table.t
  end

  module Warning_destination = struct
    type t =
      | Suppress
      | Stdout
      | Stderr
  end

  type t =
    { benchmark_display : Benchmark_display.t
    ; warning_destination : Warning_destination.t
    }
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

        val display : t -> Ascii_table_kernel.Display.t
      end

      val param : t Command.Param.t
    end

    val show_all_values : t -> bool
  end

  module Benchmark_display : sig
    include module type of struct
      include Benchmark_display
    end
  end

  module Warning_destination : sig
    include module type of struct
      include Warning_destination
    end

    val print_warning : t -> string -> unit
  end

  val default : t
  val param : t Command.Param.t
  val verbosity : t -> Verbosity.t
  val analysis_configs : t -> Analysis_config.t list
  val print_warning : t -> string -> unit
end
