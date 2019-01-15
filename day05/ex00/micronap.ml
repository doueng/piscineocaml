(* compile with
 * ocamlopt unix.cmxa micronamp.ml *)

let my_sleep () = Unix.sleep 1

let sleeper () =
  let numSec = int_of_string (Sys.argv.(1)) in
  for i = 1 to numSec do
    my_sleep();
  done

let () =
  let numArgs = Array.length Sys.argv - 1 in
  if numArgs <> 1 then
    print_endline "Usage: ./a.out <Integer>"
  else
    sleeper()
