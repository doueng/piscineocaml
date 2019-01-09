let crossover (alst : 'a list) (blst : 'a list) : 'a list =
  let rec check_blst (to_check : 'a) (blst : 'a list)  : 'a list =
    match blst with
    | [] -> [];
    | hd :: _ when hd = to_check -> [to_check];
    | _ :: tail -> check_blst to_check tail
  in
  let rec iter_alst (alst : 'a list) (matches : 'a list) : 'a list =
    match alst with
    | [] -> matches;
    | hd :: tail -> iter_alst tail (matches @ (check_blst hd blst));
  in
  iter_alst alst []

let () =
  let print_int_lst lst =
    List.iter
      (fun n -> print_int n; print_string " ")
      lst;
    print_newline ();
  in
  print_int_lst (crossover [1; 2; 3; 4] [2; 4]);
  print_int_lst (crossover [1; 2; 3; 4] [2; 4; 3]);

  let print_string_lst lst =
    List.iter
      (fun s -> print_string (s ^ " "))
      lst;
    print_newline ();
  in
  print_string_lst (crossover ["a"; "b"; "c"] ["a"; "a";]);
  print_string_lst (crossover [] ["a"; "a";]);
  print_string_lst (crossover ["a"; "b"; "c"] []);
  print_string_lst (crossover [] []);
