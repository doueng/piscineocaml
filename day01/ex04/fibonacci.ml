let fibonacci n =
  if n < 0 then
    (-1)
  else
    let rec loop i curr prev =
      if i = 0 then
        prev
      else
        loop (i - 1) (curr + prev) curr;
    in
    loop n 1 0

let () =
  print_endline (string_of_int (fibonacci (-42)));
  print_endline (string_of_int (fibonacci 1));
  print_endline (string_of_int (fibonacci 3));
  print_endline (string_of_int (fibonacci 6));
  print_endline (string_of_int (fibonacci 0));
