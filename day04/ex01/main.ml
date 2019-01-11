let rec print_ints cards =
  match cards with
  | [] -> print_newline ();
  | hd :: tl ->
    print_int (Value.toInt hd);
    print_string ", ";
    print_ints tl

let get_hd lst =
  match lst with
  | hd :: _ -> hd;
  | _ -> invalid_arg "Empty lst"

let rec get_last lst =
  match lst with
  | [] -> invalid_arg "Empty lst"
  | hd :: sec :: tl -> get_last tl;
  | hd :: tl -> hd

let print_strings cards =
  let rec loop card =
    match card with
    | Value.As -> print_endline(Value.toString card);
    | _ ->
      print_endline(Value.toString card);
      loop (Value.next card)
  in
  loop (get_hd cards)

let print_strings_verbose cards =
  let rec loop card =
    match card with
    | Value.T2 -> print_endline(Value.toStringVerbose card);
    | _ ->
      print_endline(Value.toStringVerbose card);
      loop (Value.previous card)
  in
  loop (get_last cards)


let () =
  print_endline "Print ints";
  print_ints Value.all;
  print_newline ();
  print_endline "Print strings, using next";
  print_strings Value.all;
  print_newline ();
  print_endline "Print verbose strings, using previous";
  print_strings_verbose Value.all
