let ackermann m n =
  if m < 0 || n < 0 then
    (-1)
  else
    let rec loop m n =
      if m = 0 then n + 1
      else if m > 0 && n = 0 then loop (m - 1) 1
      else loop (m - 1) (loop m (n - 1)) in
    loop m n

let () =
  print_endline (string_of_int (ackermann (-1) 7));
  print_endline (string_of_int (ackermann (-1) (-1)));
  print_endline (string_of_int (ackermann 7 (-1)));
  print_endline (string_of_int (ackermann 0 0));
  print_endline (string_of_int (ackermann 2 3));
  print_endline (string_of_int (ackermann 4 1));
