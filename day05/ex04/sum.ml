let sum (x : float) (y : float) : float =
  x +. y

let () =
  print_endline ("0 + 0 = " ^ string_of_float(sum 0. 0.));
  print_endline ("1.1 + 2.3 = " ^ string_of_float(sum 1.1 2.3));
  print_endline ("-1.1 + -2.3 = " ^ string_of_float(sum ~-.1.1 ~-.2.3));
  print_endline ("-1.1 + 2.3 = " ^ string_of_float(sum ~-.1.1 2.3));
  print_endline ("0.1 + 0.2 = " ^ string_of_float(sum 0.1 0.2))
