module type MONOID =
sig
  type element
  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

module INT : (MONOID with type element = int) =
struct
  type element = int
  let zero1 = 0
  let zero2 = 0
  let mul = ( * )
  let add = ( + )
  let div = ( / )
  let sub = ( - )
end

module FLOAT : (MONOID with type element = float) =
struct
  type element = float
  let zero1 = 0.
  let zero2 = 0.
  let mul = ( *. )
  let add = ( +. )
  let div = ( /. )
  let sub = ( -. )
end

module type CALC =
  functor (M : MONOID) ->
  sig
    val add : M.element -> M.element -> M.element
    val sub : M.element -> M.element -> M.element
    val mul : M.element -> M.element -> M.element
    val div : M.element -> M.element -> M.element
    val power : M.element -> int -> M.element
    val fact : M.element -> M.element
  end

module Calc : CALC =
  functor (M : MONOID) ->
  struct
    let add = M.add
    let sub = M.sub
    let mul = M.mul
    let div = M.div
    let power (n : M.element) (p : int) : M.element =
      let rec loop n p res =
        if p <= 0 then
          res
        else
          loop n (p - 1) (M.mul n res)
      in
      if n = (M.zero1) || p <= 0 then (M.zero1)
      else loop n p (M.div n n)
    let fact (n : M.element) =
      let rec loop n res =
        if n = (M.div n n) then
          res
        else
          loop (M.sub n (M.div n n)) (M.mul res n)
      in
      if n = (M.zero1) then n
      else loop n (M.div n n)
  end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
  print_endline (string_of_int (Calc_int.add 3 3));
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
  let print_bool b = print_endline (string_of_bool b) in

  print_endline "ADD INT";
  let op = ( + ) in
  let myop = Calc_int.add in
  print_bool ((myop (-3) (-3)) = (op (-3) (-3)));
  print_bool ((myop (-3) 3) = (op (-3) 3));
  print_bool ((myop 3 (-6)) = (op 3 (-6)));
  print_bool ((myop 0 0) = (op 0 0));
  print_bool ((myop 10 3) = (op 10 3));
  print_endline "ADD FLOAT";
  let op = ( +. ) in
  let myop = Calc_float.add in
  print_bool ((myop (-.3.) (-.3.)) = (op (-.3.) (-.3.)));
  print_bool ((myop (-.3.) 3.) = (op (-.3.) 3.));
  print_bool ((myop 3. (-.6.)) = (op 3. (-.6.)));
  print_bool ((myop 0. 0.) = (op 0. 0.));
  print_bool ((myop 10. 3.) = (op 10. 3.));

  print_endline "SUB";
  let op = ( - ) in
  let myop = Calc_int.sub in
  print_bool ((myop (-3) (-3)) = (op (-3) (-3)));
  print_bool ((myop (-3) 3) = (op (-3) 3));
  print_bool ((myop 3 (-6)) = (op 3 (-6)));
  print_bool ((myop 0 0) = (op 0 0));
  print_bool ((myop 10 3) = (op 10 3));
  print_endline "SUB FLOAT";
  let op = ( -. ) in
  let myop = Calc_float.sub in
  print_bool ((myop (-.3.) (-.3.)) = (op (-.3.) (-.3.)));
  print_bool ((myop (-.3.) 3.) = (op (-.3.) 3.));
  print_bool ((myop 3. (-.6.)) = (op 3. (-.6.)));
  print_bool ((myop 0. 0.) = (op 0. 0.));
  print_bool ((myop 10. 3.) = (op 10. 3.));

  print_endline "MUL";
  let op = ( * ) in
  let myop = Calc_int.mul in
  print_bool ((myop (-3) (-3)) = (op (-3) (-3)));
  print_bool ((myop (-3) 3) = (op (-3) 3));
  print_bool ((myop 3 (-6)) = (op 3 (-6)));
  print_bool ((myop 0 0) = (op 0 0));
  print_bool ((myop 10 3) = (op 10 3));
  print_endline "MUL FLOAT";
  let op = ( *. ) in
  let myop = Calc_float.mul in
  print_bool ((myop (-.3.) (-.3.)) = (op (-.3.) (-.3.)));
  print_bool ((myop (-.3.) 3.) = (op (-.3.) 3.));
  print_bool ((myop 3. (-.6.)) = (op 3. (-.6.)));
  print_bool ((myop 0. 0.) = (op 0. 0.));
  print_bool ((myop 10. 3.) = (op 10. 3.));

  print_endline "DIV";
  let op = ( / ) in
  let myop = Calc_int.div in
  print_bool ((myop (-3) (-3)) = (op (-3) (-3)));
  print_bool ((myop (-3) 3) = (op (-3) 3));
  print_bool ((myop 3 (-6)) = (op 3 (-6)));
  print_bool ((myop 0 1) = (op 0 1));
  print_bool ((myop 10 3) = (op 10 3));
  print_endline "DIV FLOAT";
  let op = ( /. ) in
  let myop = Calc_float.div in
  print_bool ((myop (-.3.) (-.3.)) = (op (-.3.) (-.3.)));
  print_bool ((myop (-.3.) 3.) = (op (-.3.) 3.));
  print_bool ((myop 3. (-.6.)) = (op 3. (-.6.)));
  print_bool ((myop 0. 1.) = (op 0. 1.));
  print_bool ((myop 10. 3.) = (op 10. 3.));

  print_endline "POWER";
  let op = ( ** ) in
  let myop = Calc_int.power in
  print_bool ((myop (-3) (-3)) = int_of_float (op (-.3.) (-.3.)));
  print_bool ((myop (-3) 3) = int_of_float (op (-.3.) 3.));
  print_bool ((myop 3 (-6)) = int_of_float (op 3. (-.6.)));
  print_bool ((myop 0 1) = int_of_float (op 0. 1.));
  print_bool ((myop 3 1) = int_of_float (op 3. 1.));
  print_bool ((myop 10 3) = int_of_float (op 10. 3.));
  print_endline "POWER FLOAT";
  let op = ( ** ) in
  let myop = Calc_float.power in
  print_bool ((myop (-.3.) (-3)) = (op (-.3.) (-.3.)));
  print_bool ((myop (-.3.) 3) = (op (-.3.) 3.));
  print_bool ((myop 3. (-6)) = (op 3. (-.6.)));
  print_bool ((myop 0. 1) = (op 0. 1.));
  print_bool ((myop 10. 3) = (op 10. 3.));

  print_endline "FACTORIAL";
  let rec op n =
    if n = 0 then 1
    else n * op (n-1)
  in
  let myop = Calc_int.fact in
  print_bool (myop 3 = op 3);
  (* print_bool (myop 0 = op 0); *)
  print_bool (myop 10 = op 10);
