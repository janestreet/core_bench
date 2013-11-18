open Core.Std

module Id : Unique_id.Id = Unique_id.Int(Unit)
module Basic_test = struct
  type t = {
    test_id : Id.t;
    name    : string;
    key     : int;
    arg     : int option;
    group_key : int option;
    f       : unit -> unit;
  } with fields

  let create ~name ?(group_key=None) ?(arg=None) ~key f =
    { name; f; key; group_key; arg; test_id = Id.create () }

  let make_filename t =
    let name = String.tr ~target:' ' ~replacement:'-' t.name in
    name ^ ".txt"
end

type t = {
  name : string;
  tests: Basic_test.t list
} with fields

let create ~name ?(key=0) bm = {
  name;
  tests = [Basic_test.create ~name ~key bm];
}

let create_indexed ~name ~args ?(key=0) bm = {
  name;
  tests = List.map args ~f:(fun n ->
    let individual_key = Hashtbl.hash (key + n) in
    let name = name ^ ":" ^ (Int.to_string n) in
    { Basic_test.
      name;
      arg = Some n;
      key=individual_key; group_key= Some key;
      f = Staged.unstage (bm n);
      test_id = Id.create ()
    }
  )
}

let expand ts =
  List.concat (List.map ~f:tests ts)

