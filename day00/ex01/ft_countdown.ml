let rec ft_countdown x =
  if x <= 0 then
    begin
      print_char '0';
      print_char '\n';
    end
  else
    begin
      print_int x;
      print_char '\n';
      ft_countdown (x - 1);
    end

let () =
  ft_countdown 10;
  ft_countdown 0;
  ft_countdown (-10);
