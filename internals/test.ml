(** A module internal to [Core_bench]. Please look at {!Bench}.

    A [Test.t] represents a user specified benchmark. *)
open Core

module Id : Unique_id.Id = Unique_id.Int ()

module Hooks = struct
  type ('benchmark_ctx, 'arg) t =
    { around_benchmark : 'r. f:('benchmark_ctx @ local -> 'r) @ local once -> 'r
    ; around_measurement :
        'r. 'benchmark_ctx @ local -> f:('arg @ local -> 'r) @ local once -> 'r
    }

  let default =
    { around_benchmark = (fun ~f -> (f [@inlined hint]) `init)
    ; around_measurement = (fun `init ~f -> (f [@inlined hint]) ())
    }
  ;;
end

module Basic_test = struct
  type packed_f =
    | T :
        { hooks : ('benchmark_ctx, 'arg) Hooks.t
        ; f : 'benchmark_ctx @ local -> ('arg @ local -> 'r) Staged.t @ local
        }
        -> packed_f

  type t =
    { test_id : Id.t
    ; name : string
    ; test_name : string
    ; file_name : string
    ; module_name : string
    ; key : int
    ; arg : string option
    ; group_key : int option
    ; f : packed_f
    }
  [@@deriving fields ~getters]

  let create_with_initialization
    ~name
    ?(test_name = "")
    ?(file_name = "")
    ?(module_name = "")
    ?(group_key = None)
    ?(arg = None)
    ~key
    ~hooks
    f
    =
    { name
    ; test_name
    ; module_name
    ; file_name
    ; f = T { hooks; f = (fun [@inline] ctx -> exclave_ Staged.stage (f ctx)) }
    ; key
    ; group_key
    ; arg
    ; test_id = Id.create ()
    }
  ;;

  let make_filename t =
    let name = String.tr ~target:' ' ~replacement:'-' t.name in
    name ^ ".txt"
  ;;
end

type t =
  { name : string
  ; test_name : string
  ; file_name : string
  ; module_name : string
  ; tests : Basic_test.t list
  }
[@@deriving fields ~getters]

let create_with_initialization_and_hooks
  ~name
  ?(test_name = "")
  ?(file_name = "")
  ?(module_name = "")
  ?(key = 0)
  ~hooks
  bm
  =
  { name
  ; test_name
  ; module_name
  ; file_name
  ; tests =
      [ Basic_test.create_with_initialization
          ~name
          ~test_name
          ~module_name
          ~file_name
          ~key
          ~hooks
          bm
      ]
  }
;;

let create_with_initialization ~name ?test_name ?file_name ?module_name ?key bm =
  create_with_initialization_and_hooks
    ~name
    ?test_name
    ?file_name
    ?module_name
    ?key
    ~hooks:Hooks.default
    bm
;;

let create_with_hooks ~name ?test_name ?file_name ?module_name ?key ~hooks bm =
  create_with_initialization_and_hooks
    ~name
    ?test_name
    ?file_name
    ?module_name
    ?key
    ~hooks
    (fun `init -> bm)
;;

let create ~name ?test_name ?file_name ?module_name ?key bm =
  create_with_initialization ~name ?test_name ?file_name ?module_name ?key (fun `init ->
    bm)
;;

let create_parameterised_with_hooks
  ~name
  ?(test_name = "")
  ?(file_name = "")
  ?(module_name = "")
  ~args
  ?(key = 0)
  ~hooks
  bm
  =
  { name
  ; test_name
  ; module_name
  ; file_name
  ; tests =
      List.map args ~f:(fun ((label, param) as n) ->
        let individual_key = Stdlib.Hashtbl.seeded_hash key n in
        let name = name ^ ":" ^ label in
        { Basic_test.name
        ; test_name
        ; module_name
        ; file_name
        ; arg = Some label
        ; key = individual_key
        ; group_key = Some key
        ; f = Basic_test.T { f = (fun bm_ctx -> exclave_ bm param bm_ctx); hooks }
        ; test_id = Id.create ()
        })
  }
;;

let create_parameterised ~name ?test_name ?file_name ?module_name ~args ?key bm =
  create_parameterised_with_hooks
    ~name
    ?test_name
    ?file_name
    ?module_name
    ~args
    ?key
    ~hooks:Hooks.default
    (fun [@inline] param `init -> exclave_
       (* This unstage-stage dance is just to get the compiler to realise that the ()
          mode-crosses locality *)
       (bm param :> (unit @ local -> _) Staged.t))
;;

let create_indexed_with_hooks
  ~name
  ?test_name
  ?file_name
  ?module_name
  ~args
  ?key
  ~hooks
  bm
  =
  create_parameterised_with_hooks
    ~name
    ?test_name
    ?file_name
    ?module_name
    ?key
    ~hooks
    bm
    ~args:(List.map args ~f:(fun i -> Int.to_string i, i))
;;

let create_indexed ~name ?test_name ?file_name ?module_name ~args ?key bm =
  create_indexed_with_hooks
    ~name
    ?test_name
    ?file_name
    ?module_name
    ~args
    ?key
    ~hooks:Hooks.default
    (fun [@inline] idx `init -> exclave_ (bm idx :> (unit @ local -> _) Staged.t))
;;

let expand ts = List.concat (List.map ~f:tests ts)

let create_group ~name ?(test_name = "") ?(file_name = "") ?(module_name = "") ts =
  let ts = expand ts in
  { name
  ; test_name
  ; module_name
  ; file_name
  ; tests =
      List.map ts ~f:(fun test ->
        let name = name ^ "/" ^ test.Basic_test.name in
        { test with Basic_test.name })
  }
;;
