type t =
  { don't_display_table : bool
  ; limit_width_to : int
  ; display : Ascii_table_kernel.Display.t
  ; ascii_table : bool
  ; show_output_as_sexp : bool
  ; show_absolute_ci : bool
  ; show_percentage : bool
  ; show_speedup : bool
  ; show_samples : bool
  ; show_all_values : bool
  ; show_overheads : bool
  ; max_name_length : int
  }
[@@deriving fields ~getters]

let create
  ?(don't_display_table = false)
  ?(max_name_length = Defaults.max_name_length)
  ?(limit_width_to = Defaults.limit_width_to)
  ?(display = Defaults.display)
  ?(ascii_table = false)
  ?(show_output_as_sexp = false)
  ?(show_absolute_ci = false)
  ?(show_percentage = false)
  ?(show_speedup = false)
  ?(show_samples = false)
  ?(show_all_values = false)
  ?(show_overheads = false)
  ()
  =
  { don't_display_table
  ; limit_width_to
  ; display
  ; ascii_table
  ; show_output_as_sexp
  ; show_absolute_ci
  ; show_percentage
  ; show_samples
  ; show_speedup
  ; show_all_values
  ; show_overheads
  ; max_name_length
  }
;;
