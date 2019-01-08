let fibonacci n =
  if n < 0 then
    -1
  else
    let rec loop n =
      if n = 0 then
        0
      else if n = 1 then
        1
      else
        (loop (n - 2)) + (loop (n - 1))
    in
    loop n

let () =
  print_endline (string_of_int (fibonacci (-42)));
  print_endline (string_of_int (fibonacci 1));
  print_endline (string_of_int (fibonacci 3));
  print_endline (string_of_int (fibonacci 6));
  print_endline (string_of_int (fibonacci 0));
