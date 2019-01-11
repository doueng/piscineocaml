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

let rot42 (s : string) : string =
  ft_rot_in 42 s

let caesar (s : string) (n : int) : string =
  ft_rot_in (ft_abs n) s

let xor (s : string) (key : int) : string =
  String.map (fun c -> char_of_int ((ft_abs key) lxor (int_of_char c))) s

let rec ft_crypt (s : string) (fns : (string -> string) list) : string =
  match fns with
  | [] -> s;
  | hd :: tl -> ft_crypt (hd s) tl

let () =
  print_endline "Crypt functions";
  print_endline (rot42 "hello");
  print_endline (caesar "hello" 12);
  print_endline (caesar "hello" (-12));
  print_endline (caesar "hello" 0);
  print_endline (xor "hello" 12);
  print_endline (xor "hello" (-12));
  print_endline (xor "hello" 0);
  let crypt_fns : (string -> string) list =
    [rot42; (fun s -> xor s 10); (fun s -> caesar s 22)] in
  print_endline (ft_crypt "hello" crypt_fns);
  print_endline (ft_crypt "hello" [])
