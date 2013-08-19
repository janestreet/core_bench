open Core.Std
module Ascii_table = Textutils.Ascii_table

(* default columns *)
let columns_as_string = [
  "+name";
  "+time";
  "percentage";
]
let columns = List.map ~f:Column.of_string columns_as_string

(* how to measure *)
let geometric_scale   = 1.01
let stabilize_gc_between_runs = false
let no_compactions = false

(* how long to measure *)
let time_quota_float = 10.0
let time_quota       = Time.Span.of_sec time_quota_float

(* saving generated data *)
let save_sample_data = false

(* width of the output table *)
let limit_width_to = 170

(* Fork each benchmark and run in separate process *)
let fork_each_benchmark = false

(* Predictors to use for mv-regression *)
let predictors = [ `Runs ]
(* The default input-string is empty, since r is assumed and not exposed to user *)
let predictors_string = ""

(* default display *)
let display_as_string = "short"
let string_to_display =
  let module Display = Ascii_table.Display in
  function
  | "short"  -> Display.short_box
  | "tall"   -> Display.tall_box
  | "line"   -> Display.line
  | "blank"  -> Display.blank
  | "column" -> Display.column_titles
  | s -> failwithf "Invalid display name: %s" s ()

let display = string_to_display display_as_string
