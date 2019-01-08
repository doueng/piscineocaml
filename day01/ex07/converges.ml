let converges f x n =
  let rec loop i res =
    if res = (f res) then
      true
    else if i <= 0 then
      false
    else
      loop (i - 1) (f res)
  in
  loop n x

let () =
  let print_bool b =
    if b then print_endline " == true" else print_endline " == false"
  in
  print_string "(( * ) 2) 2 5";
  print_bool (converges (( * ) 2) 2 5);
  print_string "(fun x -> x / 2) 2 3";
  print_bool (converges (fun x -> x / 2) 2 3);
  print_string "(fun x -> x / 2) 2 2";
  print_bool (converges (fun x -> x / 2) 2 2);
  print_string "(fun x -> x / 2) 2 -2";
  print_bool (converges (fun x -> x / 2) 2 (-2));
  print_string "(fun x -> x / 2) 2 0";
  print_bool (converges (fun x -> x / 2) 2 0);
  print_string "(fun x -> x / 2) -2 0";
  print_bool (converges (fun x -> x / 2) (-2) 0);
