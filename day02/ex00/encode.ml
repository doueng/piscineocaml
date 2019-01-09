let encode l =
  let rec loop curr l n new_list =
    match l with
    | [] -> new_list @ [n, curr] ;
    | hd :: tail ->
      if curr = "" then
        loop hd tail n new_list
      else if hd = curr then
        loop curr tail (n + 1) new_list
      else
        loop hd tail 1 (new_list @ [n, curr])
  in
  loop "" l 1 []

let () =
  let pp_tuple (n, c) =
    print_string c;
    print_int n;
    print_string ", "
  in
  let rec loop l =
    match l with
    | [] -> print_newline ();
    | hd :: tail -> pp_tuple hd; loop tail;
  in
  loop (encode ["a"; "a"; "a"; "b"; "c"; "c"]);
  loop (encode [""]);
  loop (encode []);
