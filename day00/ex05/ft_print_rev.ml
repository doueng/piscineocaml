let ft_print_rev s =
  let rec loop i =
    if i <= -1 then
      begin
        print_char '\n';
      end
    else
      begin
        print_char (String.get s i);
        loop (i - 1)
      end
  in
  loop ((String.length s) - 1)


let () =
  ft_print_rev "Hello world !";
  ft_print_rev "";
