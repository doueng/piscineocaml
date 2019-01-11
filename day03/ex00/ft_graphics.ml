type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

type point = {
  x : int;
  y : int
}

let print_point point =
  print_string (string_of_int point.x);
  print_string " : ";
  print_endline (string_of_int point.y)

let draw_square (x: int) (y: int) (size: int) : unit =
  let diff = size / 2 in
  let top_right : point = {x = (x + diff); y = (y + diff)} in
  let bottom_left : point = {x = (x - diff); y = (y - diff)} in

  Graphics.moveto top_right.x top_right.y;
  Graphics.lineto (top_right.x - size) top_right.y;
  Graphics.lineto bottom_left.x bottom_left.y;
  Graphics.lineto top_right.x bottom_left.y;
  Graphics.lineto top_right.x top_right.y

let draw_tree_node (node : 'a tree) =
  let loop (curr_point : point) (node : 'a tree) =
    draw_square curr_point.x curr_point.y 200;
    Graphics.moveto (curr_point.x - 20) curr_point.y;
    Graphics.draw_string (
      match node with
      | Nil -> "Nil";
      | Node (v, _, _) -> v);
  in
  let curr_point = {x = 200; y = 300}
  in
  Graphics.moveto curr_point.x curr_point.y;
  loop curr_point node;
  Graphics.moveto (curr_point.x + 100) (curr_point.y + 100);
  Graphics.lineto (curr_point.x + 300) (curr_point.y + 300);
  loop {x = (curr_point.x + 400); y = (curr_point.y + 400)} Nil;
  Graphics.moveto (curr_point.x + 100) (curr_point.y - 100);
  Graphics.lineto (curr_point.x + 300) (curr_point.y - 300);
  loop {x = (curr_point.x - 400); y = (curr_point.y - 400)} Nil


let () =
  Graphics.open_graph " 800x600 ";
  draw_tree_node (Node ("42", Nil, Nil));
  print_char (Graphics.read_key())
