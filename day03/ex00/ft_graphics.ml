(* Graphics lineto moveto draw_string *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let () =
  Graphics.open_graph "800x600";
  Graphics.moveto 400 300;
  Graphics.draw_string "42";
  print_char (Graphics.read_key())
