type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

type point = {
  x : int;
  y : int
}

let draw_square (x: int) (y: int) (size: int) : unit =
  let diff = size / 2 in
  let top_right : point = {x = (x + diff); y = (y + diff)} in
  let bottom_left : point = {x = (x - diff); y = (y - diff)} in
  Graphics.moveto top_right.x top_right.y;
  Graphics.lineto (top_right.x - size) top_right.y;
  Graphics.lineto bottom_left.x bottom_left.y;
  Graphics.lineto top_right.x bottom_left.y;
  Graphics.lineto top_right.x top_right.y

let size (tree : 'a tree) : int =
  let rec loop (node : 'a tree) : int =
    match node with
    | Nil -> 0;
    | Node (_, l, r) ->
      1 + (loop l) + (loop r);
  in
  loop tree

let height (tree : 'a tree) : int =
  let rec loop (node : 'a tree) (height : int) : int =
    match node with
    | Nil -> height;
    | Node (_, l, r) ->
      max (loop l (height + 1)) (loop r (height + 1))
  in
  loop tree 0

let draw_tree_node (node : string tree) : unit =
  let square_size = 30 in
  let half_size = square_size / 2 in
  let line_len = 30 in
  let multiply_int (n : int) =
    (int_of_float ((float_of_int n) *. 1.2) + 20)
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
      Graphics.lineto (multiply_int (x + line_len)) (y -  line_len);
      loop r (multiply_int (x + line_len + half_size)) (y - line_len - half_size);
  in
  match node with
  | Nil -> print_endline "Nil tree";
  | Node (v, l, r) ->
    let root_x = 20
    in
    let root_y = 150
    in
    draw_square root_x root_y square_size;
    Graphics.moveto (root_x - 10) root_y;
    Graphics.draw_string v;
    Graphics.moveto (root_x + half_size) (root_y - half_size);
    Graphics.lineto (100 - half_size) 100;
    Graphics.moveto (root_x + half_size) (root_y + half_size);
    Graphics.lineto (120 - half_size) 200;
    loop l 100 100;
    loop r 120 200

let () =
  Graphics.open_graph("");
  let tree = Node ("First",
                   (Node ("2", Nil, Nil)),
                   (Node ("3", (Node ("4", Nil, Nil)), Nil)))
  in
  print_endline ("Size: " ^ (string_of_int (size tree)));
  print_endline ("Height: " ^ (string_of_int (height tree)));
  draw_tree_node tree;
  let empty = Nil
  in
  draw_tree_node empty;
  print_endline ("Size: " ^ (string_of_int (size empty)));
  print_endline ("Height: " ^ (string_of_int (height empty)));
  print_char(Graphics.read_key())
