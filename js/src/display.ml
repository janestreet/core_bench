open Core
open Core_bench_internals
open Core_bench_internals.Display

let display ?libname ~(display_config : Display_config.t) results =
  match display_config with
  | Show_as_sexp ->
    Simplified_benchmark.to_sexp ?libname results |> Sexp.to_string |> print_endline
  | Show_as_table table_config ->
    (match table_config.how_to_print with
     | Csv _ ->
       let cols = make_csv_columns table_config results in
       Delimited_kernel.Write.to_string ~write_header:true ~line_breaks:`Unix cols results
       |> print_string
     | Human_readable { table_format; _ } ->
       let cols = make_columns table_config results in
       print_endline
       @@ Ascii_table_kernel.to_string_noattr
            ~limit_width_to:table_config.limit_width_to
            ~bars:
              (match table_format with
               | Ascii -> `Ascii
               | Pretty _ -> `Unicode)
            ~display:
              (Display_config.Table.How_to_print.Human_readable.display table_format)
            cols
            results);
    Warnings.display ()
;;
