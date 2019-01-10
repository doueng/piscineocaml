let encode (l : 'a list) : (int * 'a) list =
  let get_hd (lst : 'a list) : 'a =
    match lst with
    | [] -> failwith "Don't pass in an empty list!!!"
    | hd :: _ -> hd
  in
  let get_tail (lst : 'a list) : 'a list =
    match lst with
    | _ :: tail -> tail
    | _ -> lst
  in
  let rec loop (curr : 'a) (l : 'a list) (n : int) (new_list : (int * 'a) list) =
    match l with
    | [] -> (new_list @ [n, curr])
    | hd :: tail ->
      if hd = curr then
        loop curr tail (n + 1) new_list
      else
        loop hd tail 1 (new_list @ [n, curr])
  in
  loop (get_hd l) (get_tail l) 1 []

let () =
  let pp_tuple (n, c) =
    print_int n;
    print_string c;
    print_string ", "
  in
  let rec loop l =
    match l with
    | [] -> print_newline ();
    | hd :: tail -> pp_tuple hd; loop tail;
  in
  loop (encode ["a"; "a"; "a"; "b"; "b"; "b"]);
  loop (encode ["a"; "a"; "a"; "b"; "c"; "c"]);
  loop (encode [""]);

  let pp_tuple_int (n, i) =
    print_int n;
    print_int i;
    print_string ", "
  in
  let rec loop_int l =
    match l with
    | [] -> print_newline();
    | hd :: tail -> pp_tuple_int hd; loop_int tail;
  in
  loop_int (encode [1; 2 ;2; 3; 4; 4]);
  loop_int (encode [])
