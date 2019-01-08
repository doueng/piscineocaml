let ft_rot_in n s =
  let rotate c n floor =
    (char_of_int ((((int_of_char c) + n - floor) mod 26) + floor)) in
  let rotater c =
    if c >= 'a' && c <= 'z' then
      begin
        rotate c n (int_of_char 'a');
      end
    else if c >= 'A' && c <= 'Z' then
      begin
        rotate c n (int_of_char 'A');
      end
    else
      begin
        c;
      end
  in
  String.map rotater s

let () =
  print_endline (ft_rot_in 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_in 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_in 42 "0123456789");
  print_endline (ft_rot_in 2 "0123456789");
  print_endline (ft_rot_in 0 "Damned !");
  print_endline (ft_rot_in 42 "");
  print_endline (ft_rot_in 1 "NBzlk qnbjr !");
