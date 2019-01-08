let repeat_string ?(str = "x") n =
  if n < 0 then
    "Error"
  else
    let rec concater i =
      if i = 0 then ""
      else str ^ (concater (i - 1)) in
    concater n

let () =
  print_endline (repeat_string (-1));
  print_endline (repeat_string 0);
  print_endline (repeat_string ~str:"Toto" 1);
  print_endline (repeat_string 2);
  print_endline (repeat_string ~str:"a" 5);
