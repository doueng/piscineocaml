let print_comb x y z =
  if x < 10 && y < 10 && z < 10 then
    begin
      print_string ", ";
      print_int x;
      print_int y;
      print_int z;
    end

let ft_print_comb () =
  print_int 0;
  print_int 1;
  print_int 2;
  let rec loop x y z =
    print_comb x y z;
    if x >= 10 then
      begin
      end
    else if y >= 9 then
      begin
        loop (x + 1) (x + 2) (x + 3);
      end
    else if z >= 9 then
      begin
        loop x (y + 1) (y + 2);
      end
    else
      begin
        loop x y (z + 1);
      end
  in
  loop 0 1 3;
  print_string "\n"


let () =
  ft_print_comb ();
