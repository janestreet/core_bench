open! Core
include Display_config_intf.Definitions

module Table = struct
  include Table

  module How_to_print = struct
    include How_to_print

    module Human_readable = struct
      include Human_readable

      let default = Pretty Defaults.display

      let param =
        let open Command.Param in
        choose_one_non_optional
          ~if_nothing_chosen:(Default_to default)
          [ flag
              "-ascii"
              (no_arg_required Ascii)
              ~doc:" Display data in simple ascii based tables. [.a..]"
          ; (let%map.Command display_style =
               flag
                 "-display"
                 (optional_with_default Defaults.display_as_string string)
                 ~doc:
                   (sprintf
                      "STYLE Table style (short, tall, line, blank or column) (default \
                       %s). [p...]"
                      Defaults.display_as_string)
             in
             Pretty (Defaults.string_to_display display_style))
          ]
      ;;

      let display = function
        | Ascii -> Ascii_table_kernel.Display.column_titles
        | Pretty display -> display
      ;;
    end

    let default =
      Human_readable
        { table_format = Human_readable.default
        ; show_all_values = false
        ; verbosity = Low
        }
    ;;

    let param =
      let open Command.Param in
      choose_one_non_optional
        ~if_nothing_chosen:(Default_to default)
        [ flag
            "-csv"
            (no_arg_required (Csv { streaming = false }))
            ~doc:" Output as csv. [..c.]"
        ; flag
            "-streamed-csv"
            (no_arg_required (Csv { streaming = true }))
            ~doc:
              " Output as csv and print each row when it is ready (instead of when all \
               benchmarks finish). Disables [percentage] column. [..c.]"
        ; (let%map.Command () = return ()
           and table_format = Human_readable.param
           and show_all_values =
             flag
               "-all-values"
               no_arg
               ~doc:" Show all column values, including very small ones. [pa..]"
           and verbosity = flag "-v" no_arg ~doc:" High verbosity level. [pa..]" in
           let verbosity : Verbosity.t = if verbosity then High else Low in
           Verbosity.set_verbosity verbosity;
           Human_readable { table_format; show_all_values; verbosity })
        ]
    ;;
  end

  let create_from_flags
    ~(how_to_print : How_to_print.t)
    ~clear_columns
    ~anon_columns
    ~regressions
    ~limit_width_to
    ~max_name_length
    ~show_absolute_ci
    ~show_overheads
    =
    let streaming =
      match how_to_print with
      | Csv { streaming } -> streaming
      | Human_readable _ -> false
    in
    let columns =
      if clear_columns then [] else Defaults.command_columns ~for_streaming:streaming
    in
    let columns = columns @ anon_columns in
    if streaming
       && List.find anon_columns ~f:(function
            | Display_column `Percentage -> true
            | _ -> false)
          |> Option.is_some
    then failwith "[-streamed-csv] incompatible with [percentage] column";
    let analysis_configs, columns =
      let f =
        let open Bench_command_column in
        function
        | Analysis analysis -> First analysis
        | Display_column col -> Second col
      in
      List.partition_map columns ~f
    in
    let analysis_configs = List.concat analysis_configs in
    let analysis_configs =
      let to_name i = sprintf " [%d]" (i + 1) in
      analysis_configs
      @ List.mapi regressions ~f:(fun i reg ->
        let regression_name = to_name i in
        Verbosity.print_low "Regression%s = %s\n%!" regression_name reg;
        Analysis_config.parse reg ~regression_name)
    in
    { how_to_print
    ; limit_width_to
    ; max_name_length
    ; show_samples = List.mem columns `Samples ~equal:Display_column.equal
    ; show_percentage = List.mem columns `Percentage ~equal:Display_column.equal
    ; show_speedup = List.mem columns `Speedup ~equal:Display_column.equal
    ; show_absolute_ci
    ; show_overheads
    ; analysis_configs
    }
  ;;

  let default =
    create_from_flags
      ~how_to_print:How_to_print.default
      ~limit_width_to:Defaults.limit_width_to
      ~show_absolute_ci:false
      ~show_overheads:false
      ~max_name_length:Defaults.max_name_length
      ~clear_columns:false
      ~anon_columns:[]
      ~regressions:[]
  ;;

  let param =
    let%map_open.Command () = return ()
    and how_to_print = How_to_print.param
    and limit_width_to =
      flag
        "-width"
        (optional_with_default Defaults.limit_width_to int)
        ~doc:
          (sprintf
             "WIDTH width limit on column display (default %d). [pac.]"
             Defaults.limit_width_to)
    and show_overheads =
      flag "-overheads" no_arg ~doc:" Show measurement overheads, when applicable. [pac.]"
    and show_absolute_ci =
      flag
        "-ci-absolute"
        no_arg
        ~doc:" Display 95% confidence interval in absolute numbers. [pac.]"
    and clear_columns =
      flag
        "-clear-columns"
        no_arg
        ~doc:" Don't display default columns. Only show user specified ones. [pac.]"
    and regressions =
      flag
        "-regression"
        (listed string)
        ~doc:"REGR Specify additional regressions (See -? help). [pac.]"
    and max_name_length =
      flag_optional_with_default_doc
        "name-length-max"
        int
        [%sexp_of: int]
        ~default:Defaults.max_name_length
        ~doc:" max width of name column [pac.]"
    and anon_columns = anon (sequence ("COLUMN" %: Bench_command_column.arg)) in
    create_from_flags
      ~how_to_print
      ~clear_columns
      ~anon_columns
      ~regressions
      ~limit_width_to
      ~max_name_length
      ~show_absolute_ci
      ~show_overheads
  ;;

  let show_all_values { how_to_print; _ } =
    match how_to_print with
    | Csv _ -> true
    | Human_readable { show_all_values; _ } -> show_all_values
  ;;
end

module Benchmark_display = struct
  include Benchmark_display

  let default = Show_as_table Table.default

  let param =
    let open Command.Param in
    choose_one_non_optional
      ~if_nothing_chosen:(Default_to default)
      [ flag "-sexp" (no_arg_required Show_as_sexp) ~doc:" Output as sexp. [...s]"
      ; (let%map.Command table = Table.param in
         Show_as_table table)
      ]
  ;;
end

module Warning_destination = struct
  include Warning_destination

  let default = Stderr

  let param =
    let%map_open.Command () = return ()
    and t_opt =
      choose_one_non_optional
        ~if_nothing_chosen:Return_none
        [ flag
            "-suppress-warnings"
            (no_arg_some Suppress)
            ~doc:" Suppress warnings when clean output needed"
        ; flag "-warnings-to-stderr" (no_arg_some Stderr) ~doc:" Send warnings to STDERR"
        ; flag "-warnings-to-stdout" (no_arg_some Stdout) ~doc:" Send warnings to STDOUT"
        ]
    in
    t_opt |> Option.join |> Option.value ~default
  ;;

  let print_warning t warn =
    match t with
    | Suppress -> ()
    | Stdout -> printf "%s\n%!" warn
    | Stderr -> eprintf "%s\n%!" warn
  ;;
end

let default =
  { benchmark_display = Benchmark_display.default
  ; warning_destination = Warning_destination.default
  }
;;

let param =
  let%map_open.Command () = return ()
  and benchmark_display = Benchmark_display.param
  and warning_destination = Warning_destination.param in
  { benchmark_display; warning_destination }
;;

let verbosity { benchmark_display; _ } : Verbosity.t =
  match benchmark_display with
  | Show_as_sexp -> Quiet
  | Show_as_table { how_to_print; _ } ->
    (match how_to_print with
     | Csv _ -> Quiet
     | Human_readable { verbosity; _ } -> verbosity)
;;

let analysis_configs { benchmark_display; _ } : Analysis_config.t list =
  match benchmark_display with
  | Show_as_sexp ->
    List.concat
      [ [ Analysis_config.nanos_vs_runs ]
      ; Analysis_config.allocations_vs_runs
      ; Analysis_config.gc_vs_runs
      ]
  | Show_as_table { analysis_configs; _ } -> analysis_configs
;;

let print_warning { warning_destination; _ } warn =
  Warning_destination.print_warning warning_destination warn
;;
