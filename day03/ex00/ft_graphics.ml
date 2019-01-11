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
  let square_size = 30
  in
  let half_size = square_size / 2
  in
  let line_len = 30
  in
  let rec loop rec_node x y =
    draw_square x y square_size;
    Graphics.moveto (x - 10) y;
    Graphics.draw_string (
      match rec_node with
      | Nil -> "Nil";
      | Node (v, _, _) -> v);
    match rec_node with
    | Nil -> print_string "";
    | Node (v, l, r) ->
      Graphics.moveto (x + half_size) (y + half_size);
      Graphics.lineto (x + line_len) (y + line_len);
      loop l (x + line_len + half_size) (y + line_len + half_size);
      Graphics.moveto (x + half_size) (y - half_size);
      Graphics.lineto (x + line_len) (y -  line_len);
      loop r (x + line_len + half_size) (y - line_len - half_size);
  in
  loop node 200 300

let () =
  Graphics.open_graph "";
  draw_tree_node (Node ("42", Nil, Nil));
  print_char (Graphics.read_key())
