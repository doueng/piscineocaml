module StrHash =
struct
  type t = string
  let equal (a : t) (b : t) : bool = a = b
  (* djb2 hash function *)
  let hash (a : t) : int =
    let len = String.length a in
    let rec loop  (h : int) (i : int) : int =
      if i < len then
        loop (((h lsl 5) + h) + (int_of_char (String.get a i))) (i + 1)
      else
        h
    in
    loop 0 0
end

module StringHashtbl = Hashtbl.Make(StrHash)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
