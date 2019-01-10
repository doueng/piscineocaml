let gray n =
  let convert_to_gray n =
    n lxor (n lsr 1);
  in
  let rec add_padding gray_str len =
    if len >= n then
      gray_str
    else
      add_padding ("0" :: gray_str) (len + 1)
  in
  let rec gray_to_str gray_int gray_str len =
    if gray_int = 0 then
      add_padding gray_str len
    else
      gray_to_str
        (gray_int lsr 1)
        ((string_of_int (gray_int land 1)) :: gray_str)
        (len + 1)
  in
  let end_num =
    int_of_float (2. ** (float_of_int n))
  in
  let rec print_list lst =
    match lst with
    | [] -> print_string "";
    | hd :: tail ->
      print_string hd;
      print_list tail
  in
  let rec loop (i : int) : unit =
    match i with
    | i when i = end_num -> print_newline()
    | _ ->
      print_list
        (gray_to_str
           (convert_to_gray i)
           []
           0);
      if (i + 1) < end_num then
        print_string " "
      else
        print_string "";
      loop (i + 1);
  in
  loop 0

let () =
  gray (-1);
  gray (0);
  gray (1);
  gray (2);
  gray (3);
