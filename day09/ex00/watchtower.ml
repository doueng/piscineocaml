module type WATCHTOWER =
sig
  type hour = int
  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end

module Watchtower : WATCHTOWER =
struct
  type hour = int

  let zero = 0

  let add (h1 : hour) (h2 : hour) : hour =
    let res = Pervasives.abs((h1 + h2) mod 12) in
    if res = 0 then 12 else res

  let sub (h1 : hour) (h2 : hour) : hour =
    let res = Pervasives.abs((h1 - h2) mod 12) in
    if res = 0 then 12 else res
end

let () =
  print_endline (string_of_int (Watchtower.add 10 1));
  print_endline (string_of_int (Watchtower.add 11 1));
  print_endline (string_of_int (Watchtower.add 12 0));
  print_endline (string_of_int (Watchtower.add 10 93849));
  print_endline (string_of_int (Watchtower.add 3 113123));
  print_endline (string_of_int (Watchtower.add 3 (-10)));

  print_endline (string_of_int (Watchtower.sub 10 1));
  print_endline (string_of_int (Watchtower.sub 11 1));
  print_endline (string_of_int (Watchtower.sub 12 0));
  print_endline (string_of_int (Watchtower.sub 10 93849));
  print_endline (string_of_int (Watchtower.sub 3 113123));
  print_endline (string_of_int (Watchtower.sub 3 (-10)));
