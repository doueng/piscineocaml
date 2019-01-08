let ft_test_sign x =
  if x < 0 then
    begin
      print_endline "negative";
    end
  else
    begin
      print_endline "positive";
    end


let () =
  ft_test_sign 10;
  ft_test_sign 0;
  ft_test_sign (-10);
