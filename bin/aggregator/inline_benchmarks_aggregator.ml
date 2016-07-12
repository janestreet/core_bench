(* In order to verify results after running "jenga .save_benchmarks" for a library,
   you can use the following method using curl.

   Create a json file, e.g. query.json. Put the following json object in it:
----------------------------------------------------------
{
  "size": 0,
  "query": {
    "bool": {
      "must": [
      { "match": { "library_name": "<YOUR LIBRARY NAME>"}}
      ]
    }
  },
  "aggs": {
    "revisions": {
      "terms": {
        "field": "time_of_hg_revision",
        "order": { "_term": "desc" },
        "size": 0
      }
    }
  }
}
----------------------------------------------------------
   while replacing <YOUR LIBRARY NAME> with the name of the library you've just run jenga
   on.
   Then run the following command:
   "curl -XGET 'ldn-qdv-ielastic1:9200/benchtest/test/_search?pretty' --data-binary @query.json"
   The results will look like this:
----------------------------------------------------------
{
  ...
  "aggregations" : {
    "revisions" : {
      ...
      "buckets" : [ {
        "key" : 1467052742000,
        "key_as_string" : "2016-06-27 18:39:02",
        "doc_count" : 7
      },
      ...
      ]
    }
  }
}
----------------------------------------------------------
   and the buckets array will contain objects such that the "key_as_string" field denotes
   the time_of_hg_revision of a particular benchmark, the first of which should roughly
   match when you ran the benchmark or last committed.
*)

open Core.Std
open Async.Std

module Result = Core_bench.Simplified_benchmark.Result
module Results = Core_bench.Simplified_benchmark.Results
module Json = Json_wheel_internal.Std
module ES = Core_bench_elasticsearch_common_lib.Std.Elasticsearch

module Config = struct

  type t =
    (* the machine that the benchmarks are to be run. *)
    { benchmark_machine : string
    (* flags the user wishes to pass through to the inline_benchmarks_runner script such
       as [-quota 1] using the config file*)
    ; extra_flags : string option
    (* flags the user wishes to pass through to [mktemp] called on remote server *)
    ; tmpdir_flags : string option
    (* base url of the elasticsearch server *)
    ; elasticsearch_url : string
    (* username to use when sshing into the perfbox *)
    ; ssh_username : string option
    }
    [@@deriving of_sexp]

end

let fix_result_json res =
  let res = { res with Result.version = Version_util.version } in
  let (rev_id, time_of_rev) =
    let rev_id = String.slice res.Result.version 30 (String.length res.version) in
    let hg_version = String.slice res.Result.version 30 42 in
    let open Hg_lib in
    let full_time = Hg.Simple.log ~revs:[hg_version] () |> List.hd_exn
                    |> Hg.Changeset_info.date
    in let time_string = Time.format full_time "%Y-%m-%d %H:%M:%S" ~zone:Time.Zone.utc
    in (Some rev_id, Some time_string)
  in
  { res with time_of_hg_revision = time_of_rev; hg_revision = rev_id }

let copy_and_run_on_perfbox
      ~box_name
      ~extra_flags
      ~tmpdir_flags
      ~elasticsearch_url
      ~ssh_username
      ~verbose
  =
  let%bind dir = Sys.getcwd () in
  let runner_script = dir ^/ "inline_benchmarks_runner" in
  let runner_exe = dir ^/ "inline_benchmarks_runner.exe" in
  let ssh_full str =
    Async_shell.ssh_full
      ~ssh_options:Core_extended.Shell.noninteractive_no_hostkey_checking_options
      ?user:ssh_username
      ~host:box_name
      "%s" str
  in
  let%bind temp_dirname = ssh_full ("mktemp -d " ^ tmpdir_flags) in
  let temp_dirname = String.strip ~drop:((=) '\n') temp_dirname in
  if verbose
  then (
    printf "created tmpdir in remote box: %s\n" temp_dirname;
    print_endline "copying scripts to remote machine");
  let%bind () = Async_shell.scp ?user:ssh_username ~host:box_name runner_script temp_dirname in
  let%bind () = Async_shell.scp ?user:ssh_username ~host:box_name runner_exe temp_dirname in
  if verbose
  then print_endline "running tests";
  let remote_script = Filename.concat temp_dirname "inline_benchmarks_runner" in
  let script_cmd =
    String.concat ["sh "; remote_script; " -json +time "; extra_flags] in
  if verbose
  then print_endline script_cmd;
  let%bind results_json = ssh_full script_cmd in
  let benchmarks =
    let benchmarks = Json.Json_io.json_of_string results_json |> Results.of_json in
    List.map benchmarks ~f:fix_result_json
  in
  ES.upload_benchmarks ~elasticsearch_url ~benchmarks

let load_config_file filename = Config.t_of_sexp (Sexp.load_sexp filename)

let run ~verbose ~filename ~extra_flags_opt () =
  let config = load_config_file filename in
  let extra_flags =
    let cli_extra_flags =
      Option.value_map ~default:"" extra_flags_opt
        ~f:(fun args ->
          List.map args ~f:Filename.quote
          |> String.concat ~sep:" ")
    in
    let config_extra_flags = Option.value ~default:"" config.extra_flags in
    sprintf "%s %s" config_extra_flags cli_extra_flags
  in
  let tmpdir_flags = Option.value ~default:"" config.tmpdir_flags in
  copy_and_run_on_perfbox
    ~box_name:config.benchmark_machine
    ~extra_flags
    ~tmpdir_flags
    ~elasticsearch_url:config.elasticsearch_url
    ~ssh_username:config.ssh_username
    ~verbose

let () =
  let command =
  Command.async
    ~summary:"aggregate and send benchmark data to ElasticSearch"
    Command.Spec.(
      empty
      ++ step (fun m verbose -> m ~verbose)
      +> flag "-verbose" no_arg ~doc:" Verbose mode"
      ++ step (fun m filename -> m ~filename)
      +> anon ("CONFIG" %: file)
      ++ step (fun m extra_flags_opt -> m ~extra_flags_opt)
      +> flag "--" escape
           ~doc:" Extra flags passed to inline_benchmark_runner"
    )
    run
  in
  Command.run command
