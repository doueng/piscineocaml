let ft_print_alphabet () =
  let rec loop c =
    let n = int_of_char(c) in
    if n <= int_of_char('z') then
      begin
        print_char c;
        loop (char_of_int (n + 1));
      end
  in
  loop 'a';
  print_char '\n'

let () =
  ft_print_alphabet ();
