let iter f x n =
  if n < 0 then
    -1
  else
    let rec loop i res =
      if i = 0 then
        res
      else
        loop (i - 1) (f res)
    in
    loop n x

let () =
  print_string "(x * x) 2 4 == ";
  print_endline (string_of_int (iter (fun x -> x * x) 2 4));
  print_string "(x * 2) 2 4 == ";
  print_endline (string_of_int (iter (fun x -> x * 2) 2 4));
  print_string "(x * x) 0 4 == ";
  print_endline (string_of_int (iter (fun x -> x * x) 0 4));
  print_string "(x * x) 4 0 == ";
  print_endline (string_of_int (iter (fun x -> x * x) 4 0));
  print_string "(x * x) 0 0 == ";
  print_endline (string_of_int (iter (fun x -> x * x) 0 0));
  print_string "(x * x) -2 4 == ";
  print_endline (string_of_int (iter (fun x -> x * x) (-2) 4));
  print_string "(x * x) (-2) (-4) == ";
  print_endline (string_of_int (iter (fun x -> x * x) (-2) (-4)));
  print_string "(x * x ) 2 (-4) == ";
  print_endline (string_of_int (iter (fun x -> x * x) 2 (-4)));
