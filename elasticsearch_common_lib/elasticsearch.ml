open! Core.Std
open! Async.Std

module B = Core_bench.Simplified_benchmark.Result
module Json = Json_wheel_internal.Std

(* uses curl for http GET requests since Cohttp_async_lib doesn't support GET req with
   body, which is required by ElasticSearch DSL
*)
let base_search_url elasticsearch_url =
  elasticsearch_url ^/ "/_search?pretty"

let base_count_url elasticsearch_url =
  elasticsearch_url ^/ "/_count?pretty"

module Field_type = Core_bench.Simplified_benchmark.Field_type

module Revision = String

module Revision_info = struct
  type t =
    { revision : Revision.t
    ; revision_time : Time.t
    }
  [@@deriving fields]
end

(* An example of a search result can be found here:
   https://www.elastic.co/guide/en/elasticsearch/reference/current/search-uri-request.html
   A simplified model, which this deserialization is based on, is like this:
   { ...
     "hits": {
       ...
       "hits": [
         ...
         "_source": <benchmark_json>
       ]
     }
   }
   where _source contains the JSON representation of the benchmark
   uploaded into ElasticSearch
*)
module Search_result = struct

  type t'' = { _source : B.t } [@@deriving typerep]

  type t' = { hits : t'' list } [@@deriving typerep]

  type t = { hits : t' } [@@deriving typerep]

  let of_json =
    let `generic of_json = Json_typerep.Jsonrep.V2.t_of_json typerep_of_t in
    of_json

  let benchmarks_of_json_str str =
    let json = Json.Json_io.json_of_string str in
    let t = of_json json in
    let t' = t.hits in
    List.map t'.hits ~f:(fun t'' ->
      t''._source)

end

(* This result uses a nested terms bucket aggregation search, an example of which can be found here:
   https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-terms-aggregation.html
   A simplified model is as such:
   { ...
     "aggregations": {
       "revisions": {
         "buckets": [
           {
             "key": <revision id>,
             "time": {
               "value_as_string": <time_of_hg_revision>
             }
           }
           ...
         ]
       }
     }
   }
*)
module Revisions_search_result = struct

  type time' = { value_as_string : string } [@@deriving typerep]

  type bucket' = { key : string ; time : time'} [@@deriving typerep]

  type revisions' = { buckets : bucket' list } [@@deriving typerep]

  type aggregations' = { revisions : revisions' } [@@deriving typerep]

  type t' = { aggregations : aggregations' } [@@deriving typerep]

  let revisions_of_json_str str =
    let json = Json.Json_io.json_of_string str in
    let `generic of_json = Json_typerep.Jsonrep.V2.t_of_json typerep_of_t' in
    let t' = of_json json in
    List.map t'.aggregations.revisions.buckets ~f:(fun bucket ->
      { Revision_info.revision = bucket.key
      ; revision_time = Time.of_string bucket.time.value_as_string
      })
end

(* This uses another terms bucket aggregations search, unnested
   A simplified model is as such :
   { ...
     "aggregations": {
       "libraries": {
         "buckets": [
           { "key": <library_name> }, ...
         ]
       }
     }
   }
*)
module Libraries_search_result = struct

  type bucket' = { key : string } [@@deriving typerep]

  type libraries' = { buckets : bucket' list } [@@deriving typerep]

  type aggregations' = { libraries : libraries' } [@@deriving typerep]

  type t' = { aggregations : aggregations' } [@@deriving typerep]

  let libraries_of_json_str str =
    let json = Json.Json_io.json_of_string str in
    let `generic of_json = Json_typerep.Jsonrep.V2.t_of_json typerep_of_t' in
    let t' = of_json json in
    List.map t'.aggregations.libraries.buckets ~f:(fun bucket ->
      bucket.key
    )

end

let curl_get url query_json =
  Async_shell.run_full
    "curl"
    [ "-XGET"
    ; url
    ; "-d"
    ; query_json
    ]

module Bulk = struct

  (* Documentation for how this Json is used found here -
     https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html

     Each document uploaded needs two parts - the action and the payload, both of which
     need to be a single line json document. The action specifies, in this case, to
     update/replace a document with a given id. The payload is simply the json document we
     wish to upload (that being the benchmark result) along with "doc_as_upsert: true",
     which tells ElasticSearch to replace any document with the same id.  *)
  let create_upsert ~id ~json_doc =
    let open Json in
    let action_json =
      Json_type.Object
        [ "update", Json_type.Object
                      [ "_id", Json_type.String id
                      ]
        ]
    in
    let payload_json =
      Json_type.Object
        [ "doc", json_doc
        ; "doc_as_upsert", Json_type.Bool true
        ]
    in
    String.concat
      [ Json.Json_io.string_of_json ~compact:true action_json
      ; "\n"
      ; Json.Json_io.string_of_json ~compact:true payload_json
      ; "\n"
      ]
end

let upload_benchmarks ~elasticsearch_url ~benchmarks =
  let esbulk =
    (List.map benchmarks ~f:(fun b ->
       let id = b.B.full_benchmark_name ^ b.B.version in
       Bulk.create_upsert ~id ~json_doc:(B.to_json b))
     |> String.concat)
  in
  let base_uri = Ocaml_uri.Uri.of_string (elasticsearch_url ^/ "_bulk?pretty") in
  let body = Cohttp_async_lib.Cohttp_async.Body.of_string esbulk in
  let%bind (resp, body) = Cohttp_async_lib.Cohttp_async.Client.put ~body base_uri in
  let open Cohttp_async_lib.Cohttp_async in
  begin match Cohttp_external.Response.status resp with
  | `OK | `Created -> return ()
  | status_code -> (
      let%map response = Body.to_string body in
      let status_code =
        Cohttp_external.Code.sexp_of_status_code status_code
      in
      raise_s [%message "ElasticSearch returned bad status code"
                          (status_code : Sexp.t)
                          (response : string) ])
  end


let get_all_revisions ?(last_n=0) ~library_name ~elasticsearch_url () =
  let query_json =
    let open Json in
    Json_type.Object [
      "query", Json_type.Object [
        "bool", Json_type.Object [
          "must", Json_type.Array [
            Json_type.Object [
              "match", Json_type.Object [
                "library_name", Json_type.String library_name
              ]
            ]
          ]
        ]
      ];
      "aggs", Json_type.Object [
        "revisions", Json_type.Object [
          "terms", Json_type.Object [
            "field", Json_type.String "hg_revision";
            "order", Json_type.Object [
              "time", Json_type.String "desc"
            ];
            "size", Json_type.Int last_n
          ];
          "aggs", Json_type.Object [
            "time", Json_type.Object [
              "max", Json_type.Object [
                "field", Json_type.String "time_of_hg_revision"
              ]
            ]
          ]
        ]
      ]
    ] |> Json_io.string_of_json in
  let%map res = curl_get (base_search_url elasticsearch_url) query_json in
  let revisions = Revisions_search_result.revisions_of_json_str res in
  revisions


let get_all_library_names ~revision ~elasticsearch_url =
  let query_json =
    let open Json in
    Json_type.Object [
      "query", Json_type.Object [
        "bool", Json_type.Object [
          "must", Json_type.Array [
            Json_type.Object [
              "match", Json_type.Object [
                "hg_revision", Json_type.String revision
              ]
            ]
          ]
        ]
      ];
      "aggs", Json_type.Object [
        "libraries", Json_type.Object [
          "terms", Json_type.Object [
            "field", Json_type.String "library_name";
            "order", Json_type.Object [
              "_term", Json_type.String "desc"
            ];
            "size", Json_type.Int 0
          ]
        ]
      ]
    ] |> Json_io.string_of_json
  in
  let%map res = curl_get (base_search_url elasticsearch_url) query_json in
  let libraries = Libraries_search_result.libraries_of_json_str res in
  libraries


let get_most_recent_revision
      ?(except=[])
      ~library_name
      ~elasticsearch_url
      ()
  =
  let query_json =
    let open Json in
    Json_type.Object [
      "query", Json_type.Object [
        "bool", Json_type.Object [
          "must", Json_type.Array [
            Json_type.Object [
              "match", Json_type.Object [
                "library_name", Json_type.String library_name
              ];
            ]
          ];
          "must_not", Json_type.Array (
            List.map except ~f:(fun e ->
              Json_type.Object [
                "term", Json_type.Object [
                  "hg_revision", Json_type.String e
                ]
              ]
            )
          )
        ]
      ];
      "size", Json_type.Int 1;
      "sort", Json_type.Array [
        Json_type.Object [
          "time_of_hg_revision", Json_type.Object [
            "order", Json_type.String "desc"
          ]
        ]
      ]
    ] |> Json_io.string_of_json
  in
  let%map res = curl_get (base_search_url elasticsearch_url) query_json in
  let benchmark = match List.hd (Search_result.benchmarks_of_json_str res) with
    | Some benchmark -> benchmark
    | None -> failwith "No results from search"
  in
  Option.value_exn benchmark.B.hg_revision

let get_second_most_recent_revision
      ~library_name
      ~elasticsearch_url
  =
  let%bind recent_rev = get_most_recent_revision ~library_name ~elasticsearch_url () in
  get_most_recent_revision ~library_name ~except:[recent_rev] ~elasticsearch_url ()

let get_benchmarks_from_revisions
      ~revisions
      ~full_benchmark_names
      ~library_name
      ~elasticsearch_url
  =
  let query_json =
    let open Json in
    let make_match_obj term value =
      Json_type.Object [
        "match", Json_type.Object [
          term, Json_type.String value
        ]
      ]
    in
    let lib_match = make_match_obj "library_name" library_name in
    let benchmark_name_matches =
      List.map full_benchmark_names ~f:(make_match_obj "full_benchmark_name")
    in
    let revision_matches =
      List.map revisions ~f:(make_match_obj "hg_revision")
    in
    let should_field =
      lib_match :: benchmark_name_matches @ revision_matches
    in
    Json_type.Object [
      (* magic number (max result size) until I figure out how to get unlimited search
         results *)
      "size", Json_type.Int 10000;
      "query", Json_type.Object [
        "bool", Json_type.Object [
          "should", Json_type.Array should_field;
          "minimum_should_match", Json_type.Int 3
        ]
      ];
      "sort", Json_type.Object [
        "full_benchmark_name", Json_type.Object [
          "order", Json_type.String "asc"
        ];
        "time_of_hg_revision", Json_type.Object [
          "order", Json_type.String "desc"
        ]
      ]
    ] |> Json_io.string_of_json in
  let%map res = curl_get (base_search_url elasticsearch_url) query_json in
  Search_result.benchmarks_of_json_str res

module Count_resp = struct
  type t =
    { count : int
    }
  [@@deriving typerep, fields]

  let of_json =
    let `generic of_json = Json_typerep.Jsonrep.V2.t_of_json typerep_of_t in
    of_json
end

let make_count_req ?field ?revisions ~library_name ~elasticsearch_url full_benchmark_name =
  let count_json =
    let module J = Json.Json_type in
    let should_fields =
      let make_term_obj field value =
        J.Object [ "term", J.Object [ field, J.String value ]]
      in
      let make_range_obj field =
        J.Object [ "range", J.Object [ field, J.Object [ "gte", J.Int 1 ]]]
      in
      let revision_objs =
        match revisions with
        | Some revisions -> List.map revisions ~f:(make_term_obj "hg_revision")
        | None -> []
      in
      let rest = (make_term_obj "full_benchmark_name" full_benchmark_name)
                  :: (make_term_obj "library_name" library_name)
                  :: revision_objs
      in
      match field with
      | Some field -> (make_range_obj (Field_type.to_string field)) :: rest
      | None -> rest
    in
    let minimum_num_should_match =
      let from_field =
        match field with
        | Some _ -> 1
        | None -> 0
      in
      let from_revisions =
        match revisions with
        | Some _ -> 1
        | None -> 0
      in
      2 + from_revisions + from_field
    in
    J.Object [
      "filter", J.Object [
        "bool", J.Object [
          "should", J.Array should_fields;
          "minimum_number_should_match", J.Int minimum_num_should_match
        ]
      ]
    ] |> Json.Json_io.string_of_json
  in
  let%map res = curl_get (base_count_url elasticsearch_url) count_json in
  let count_res = Count_resp.of_json (Json.Json_io.json_of_string res) in
  count_res.Count_resp.count

let get_number_of_saved_benchmarks
      ?revisions
      ~library_name
      ~full_benchmark_name
      ~elasticsearch_url () =
  make_count_req ?revisions ~library_name ~elasticsearch_url full_benchmark_name

let get_number_of_benchmarks_with_result_above_one
      ?revisions
      ~library_name
      ~field
      ~full_benchmark_name
      ~elasticsearch_url () =
  make_count_req ?revisions ~field ~library_name ~elasticsearch_url full_benchmark_name
