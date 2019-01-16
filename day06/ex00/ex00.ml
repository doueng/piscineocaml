module StringOrder =
struct
  type t = string
  let compare (a : t) (b : t) : int = if a = b then 0 else if a < b then (-1) else 1
end

module StringSet = Set.Make(StringOrder)

let () =
  let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")
