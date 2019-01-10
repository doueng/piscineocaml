let encode (l : 'a list) : string list =
  let get_hd (lst : 'a list) : 'a =
    match lst with
    | hd :: _ -> hd
    | _ -> ""
  in
  let get_tail (lst : 'a list) : 'a list =
    match lst with
    | _ :: tail -> tail
    | _ -> lst
  in
  let rec loop (curr : 'a) (l : 'a list) (n : int) (new_list : string list) =
    match l with
    | [] -> (new_list @ [(string_of_int n)] @ [curr])
    | hd :: tail ->
      if hd = curr then
        loop curr tail (n + 1) new_list
      else
        loop hd tail 1 (new_list @ [(string_of_int n)] @ [curr])
  in
  loop (get_hd l) (get_tail l) 1 []

let rec list_to_string lst str =
  match lst with
  | [] -> str;
  | hd :: tl -> list_to_string tl (str ^ hd)

let sequence (n : int) : string =
  if n <= 0 then
    ""
  else
    let rec loop (i : int) : string list =
      if i = n then
        ["1"]
      else
        encode (loop (i + 1))
    in
    list_to_string (loop 1) ""

let () =
  print_endline (sequence (-1));
  print_endline (sequence 0);
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
  print_endline (sequence 7);
