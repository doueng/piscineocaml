type cell = O | X | E
type smallBoard = cell list list
type board = smallBoard list list
type index = T0 | T1 | T2

let getIndexInt (i : index) : int =
  match i with
  | T0 -> 0;
  | T1 -> 1;
  | T2 -> 2

(* Should we use double list or just a row??? *)
let getCell (smallBoard : smallBoard) (row : index) (col : index) : cell =
  List.nth (List.nth smallBoard (getIndexInt col)) (getIndexInt row)

let printCell (cell : cell) : unit =
  print_string (
    match cell with
    | O -> " O "
    | X -> " X "
    | E -> " - ")

let printSmallBoard (smallBoard : smallBoard) : unit =
  let rec loop board =
    match board with
    | [] -> print_newline ();
    | hd :: tl ->
      List.iter printCell hd;
      print_newline();
      loop tl;
  in
  loop smallBoard


let createSmallBoard () : smallBoard =
  List.init 3 (fun _ -> List.init 3 (fun _ -> E))

let () =
  let small = createSmallBoard ()
  in
  printSmallBoard small;
  print_endline ""
