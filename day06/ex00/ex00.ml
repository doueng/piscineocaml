module StringOrder =
struct
  type t = string
  let compare (a : t) (b : t) : int = Pervasives.compare a b
end

module StringSet = Set.Make(StringOrder)

let () =
  let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")
