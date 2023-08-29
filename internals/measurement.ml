open Core

type t =
  { name : string
  ; test_name : string
  ; file_name : string
  ; module_name : string
  ; largest_run : int
  ; samples : Measurement_sample.t array
  }
[@@deriving sexp, fields ~getters]

let create ~name ~test_name ~file_name ~module_name ~largest_run ~samples =
  { name; test_name; file_name; module_name; largest_run; samples }
;;

let sample_count t = Array.length t.samples

let save t ~filename =
  Verbosity.print_high
    "%s: Writing %d samples to file: %s.%!\n"
    t.name
    (sample_count t)
    filename;
  let header1 = "# " ^ String.escaped t.name in
  let header2 = Measurement_sample.field_names_to_string () in
  let ls =
    List.rev
      (Array.foldi t.samples ~init:[] ~f:(fun i ls cur ->
         if i < sample_count t
         then Measurement_sample.field_values_to_string cur :: ls
         else ls))
  in
  Out_channel.write_lines filename (header1 :: header2 :: ls)
;;

let load ~filename =
  match In_channel.read_lines filename with
  | header1 :: _header2 :: data ->
    let name = String.subo ~pos:2 header1 |> Scanf.unescaped in
    let test_name = "" in
    let file_name = "" in
    let module_name = "" in
    let samples =
      Array.of_list (List.map data ~f:Measurement_sample.of_field_values_string)
    in
    let sample_count = Array.length samples in
    let largest_run =
      Measurement_sample.max samples ~len:sample_count ~field:Measurement_sample.runs
    in
    { name; test_name; file_name; module_name; largest_run; samples }
  | _ -> failwith "Bad header format for saved metrics file."
;;
