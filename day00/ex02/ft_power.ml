let rec get_power n p res =
  if p <= 0 then
    begin
      res;
    end
  else
    begin
      get_power n (p - 1) (res * n);
    end

let ft_power n p =
  if p == 0 then
    begin
      1;
    end
  else
    begin
      get_power n p 1;
    end


let () =
  print_int (ft_power 2 4);
  print_char '\n';
  print_int (ft_power 3 0);
  print_char '\n';
  print_int (ft_power 0 5);
  print_char '\n';
