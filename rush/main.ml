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

let getCellString (cell : cell) : string =
  match cell with
  | O -> "O"
  | X -> "X"
  | E -> "-"

let printSmallBoard (smallBoard : smallBoard) (divider : string) : unit =
  let rec loop board =
    match board with
    | [] -> print_string "";
    | hd :: tl ->
      List.iteri (fun i cell ->
          match i <= 1 with
          | true ->
            print_string (getCellString cell);
            print_string " ";
          | false ->
            print_string (getCellString cell))
        hd;
      print_endline (divider);
      loop tl;
  in
  loop smallBoard

let printBoard (board : board) : unit =
  let rec loop brd =
    match brd with
    | [] -> print_string "";
    | hd :: tl ->
      List.iteri (fun i cell ->
          match i <= 1 with
          | true ->
            printSmallBoard hd " | ";
          | false ->
            printSmallBoard hd "")
        hd;
      loop tl;
  in
  loop board

let createSmallBoard () : smallBoard =
  List.init 3 (fun _ -> List.init 3 (fun _ -> E))

let createBoard () : board =
  List.init 3 (fun _ -> List.init 3 (fun _ -> createSmallBoard()))

let () =
  let board = createBoard()
  in
  printBoard (board);
  print_endline ""
