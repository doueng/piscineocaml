let () =
  let rec loop i =
    if i < 13 then
      begin
        let a = new Alkane.alkane i in
        print_endline a#to_string;
        loop (i + 1)
      end
  in
  loop 1;
  let tryInvalid n =
    try
      let invalid = new Alkane.alkane n in
      print_endline invalid#to_string
    with
    | invalid_arg -> print_endline "invalid"
  in
  tryInvalid 13;
  tryInvalid 0;
  tryInvalid (-1);
