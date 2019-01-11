let ft_rot_in n s =
  let rotate c n floor =
    (char_of_int ((((int_of_char c) + n - floor) mod 26) + floor)) in
  let rotater c =
    if c >= 'a' && c <= 'z' then
      begin
        rotate c n (int_of_char 'a');
      end
    else if c >= 'A' && c <= 'Z' then
      begin
        rotate c n (int_of_char 'A');
      end
    else
      begin
        c;
      end
  in
  String.map rotater s

let ft_abs (n : int) =
  if n < 0 then n * (-1) else n

let unrot42 (s : string) : string =
  ft_rot_in (26 - (42 mod 26)) s

let uncaesar (s : string) (n : int) : string =
  ft_rot_in (26 - ((ft_abs n) mod 26)) s

let xor (s : string) (key : int) : string =
  String.map (fun c -> char_of_int ((ft_abs key) lxor (int_of_char c))) s

let rec ft_uncrypt (s : string) (fns : (string -> string) list) : string =
  match fns with
  | [] -> s;
  | hd :: tl -> hd (ft_uncrypt s tl)

let () =
  print_endline "Uncrypt functions";
  print_endline (unrot42 (Cipher.rot42 "rot42"));
  print_endline (uncaesar (Cipher.caesar "Caesar -12" (-12)) (-12));
  print_endline (uncaesar (Cipher.caesar "Caesar 12" 12) 12);
  print_endline (uncaesar (Cipher.caesar "Caesar 0" 0) 0);
  print_endline (xor (Cipher.xor "xor -12" (-12)) (-12));
  print_endline (xor (Cipher.xor "xor 12" 12) 12);
  print_endline (xor (Cipher.xor "xor 0" 0) 0);
  let crypt_fns : (string -> string) list =
    [Cipher.rot42; (fun s -> Cipher.xor s 10); (fun s -> Cipher.caesar s 22)] in
  let uncrypt_fns : (string -> string) list =
    [unrot42; (fun s -> xor s 10); (fun s -> uncaesar s 22)] in
  print_endline (ft_uncrypt (Cipher.ft_crypt "ft_uncrypt ft_crypt with some numbers 1223" crypt_fns) uncrypt_fns);
  print_endline (ft_uncrypt (Cipher.ft_crypt "empty ft_uncrypt ft_crypt" []) [])
