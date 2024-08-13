open Core
open Core_bench_internals
open Core_bench_internals.Display

let display ?libname ~display_config results =
  let { benchmark_display; warning_destination } : Display_config.t = display_config in
  match benchmark_display with
  | Show_as_sexp ->
    Simplified_benchmark.to_sexp ?libname ~hostname:(Core_unix.gethostname ()) results
    |> Sexp.to_string
    |> print_endline
  | Show_as_table table_config ->
    (match table_config.how_to_print with
     | Csv _ ->
       let cols = make_csv_columns table_config warning_destination results in
       Delimited.Write.to_string ~write_header:true ~line_breaks:`Unix cols results
       |> print_string
     | Human_readable { table_format; _ } ->
       let cols = make_columns table_config warning_destination results in
       Ascii_table.output
         ~oc:stdout
         ~limit_width_to:table_config.limit_width_to
         ~bars:
           (match table_format with
            | Ascii -> `Ascii
            | Pretty _ -> `Unicode)
         ~display:(Display_config.Table.How_to_print.Human_readable.display table_format)
         cols
         results);
    Warnings.display ~display_config
;;
