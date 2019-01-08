let ft_is_palindrome s =
  let last_i = (String.length s) - 1 in
  let rec loop i e res =
    if i = e || res = false then
      begin
        res;
      end
    else
      begin
        loop (i + 1) (e - 1) ((String.get s i) = (String.get s e));
      end
  in
  loop 0 last_i (last_i >= 0)

let checker f s =
  print_string s;
  if (f s) then
    begin
      print_endline " == true";
    end
  else
    begin
      print_endline " == false";
    end

let () =
  checker ft_is_palindrome "radar";
  checker ft_is_palindrome "madam";
  checker ft_is_palindrome "car";
  checker ft_is_palindrome "c";
  checker ft_is_palindrome "";
