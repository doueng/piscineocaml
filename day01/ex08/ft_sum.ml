let ft_sum f low high =
  if high < low then
    nan
  else
    let rec loop i sum =
      if i > high then
        sum
      else
        loop (i + 1) (sum +. (f i))
    in
    loop low 0.

let () =
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_newline ();
  print_float (ft_sum (fun i -> float_of_int (i * i)) 0 0);
  print_newline ();
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 0);
  print_newline ();
  print_float (ft_sum (fun i -> float_of_int (i * i)) 0 10);
  print_newline ();
  print_float (ft_sum (fun i -> float_of_int (i * i)) (-10) 10);
  print_newline ();
  print_float (ft_sum (fun i -> float_of_int (i * i)) (-10) (-10));
  print_newline ();
  print_float (ft_sum (fun i -> float_of_int (i * i)) 0 (-10));
  print_newline ();
  print_float (ft_sum (fun i -> float_of_int (i * i)) 5 5);
  print_newline ();
