let leibniz_pi delta =
  let my_pi i =
    4. *. (((-1.) ** i) /. ((2. *. i) +. 1.));
  in
  let pi =
    4. *. (atan 1.)
  in
  let ft_abs_float n =
    if n > 0. then n else (-1.) *. n;
  in
  if delta < 0. then
    -1
  else
    let rec loop i sum =
      if delta >= ft_abs_float (pi -. sum) then
        i
      else
        loop (i + 1) (sum +. (my_pi (float_of_int i)))
    in
    loop 0 0.

let () =
  print_int (leibniz_pi 0.001);
  print_newline ();
  print_int (leibniz_pi (-0.001));
  print_newline ();
  print_int (leibniz_pi 100.);
  print_newline ();
