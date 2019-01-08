let ft_string_all f s =
  let len = String.length s in
  let rec loop i res =
    if i >= len || res = false then
      begin
        res
      end
    else
      begin
        loop (i + 1) (f (String.get s i))
      end
  in
  loop 0 (len > 0)


let is_digit c = c >= '0' && c <= '9'
let checker s =
  print_endline s;
  if (ft_string_all is_digit s) then
    begin
      print_endline "true";
    end
  else
    begin
      print_endline "false";
    end

let () =
  checker "0123456789";
  checker "01EA456789";
  checker "A123456789";
  checker "012345678B";
  checker "";
