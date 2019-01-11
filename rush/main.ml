type cell = O | X | E
type board = {
  whoWon : cell;
  b : cell * cell * cell * cell * cell * cell * cell * cell * cell
}
type allBoards = board list

let getCellString (cell : cell) : string =
  match cell with
  | O -> "O"
  | X -> "X"
  | E -> "-"

let printRow (a : cell) (b : cell) (c : cell) : unit =
  print_string (getCellString a);
  print_string " ";
  print_string (getCellString b);
  print_string " ";
  print_string (getCellString c)

let printBoard (board : allBoards) : unit =
  let rec loop (b : allBoards) (iterator : int) : unit =
    match b with
    | [] -> print_newline();
    | hd :: tail ->
      let (a, b, c, d, e, f, g, h, i) = hd.b
      in
      printRow a b c;
      print_string " | ";
      printRow d e f;
      print_string " | ";
      printRow g h i;
      if iterator = 3 || iterator = 6 then
        print_endline "\n---------------------"
      else
        print_newline ();
      loop tail (iterator + 1)
  in
  loop board 1

let createBoard () : allBoards =
  List.init 9 (fun _ -> {whoWon = E;
                         b = (E, O, E, E, E, E, E, E, E)})

let () =
  let board = createBoard()
  in
  printBoard board;

  (* let rec loop row =
   *   match row with
   *   | [] -> print_newline();
   *   | hd :: tl ->
   *     printRow hd;
   *     print_newline ();
   *     loop tl
   * in
   * loop board *)

  (* type cell = O | X | E
   * type smallBoard = cell list list
   * type board = smallBoard list list
   * type index = T0 | T1 | T2
   *
   * let getIndexInt (i : index) : int =
   *   match i with
   *   | T0 -> 0;
   *   | T1 -> 1;
   *   | T2 -> 2
   *
   * (\* Should we use double list or just a row??? *\)
   * let getCell (smallBoard : smallBoard) (row : index) (col : index) : cell =
   *   List.nth (List.nth smallBoard (getIndexInt col)) (getIndexInt row)
   *
   * let getCellString (cell : cell) : string =
   *   match cell with
   *   | O -> "O"
   *   | X -> "X"
   *   | E -> "-"
   *
   * let printSmallBoard (smallBoard : smallBoard) (divider : string) : unit =
   *   let rec loop (board : smallBoard) =
   *     match board with
   *     | [] -> print_string "";
   *     | hd :: tl ->
   *       List.iteri (fun i cell ->
   *           match i <= 1 with
   *           | true ->
   *             print_string (getCellString cell);
   *             print_string " ";
   *           | false ->
   *             print_string (getCellString cell))
   *         hd;
   *       print_endline (divider);
   *       loop tl;
   *   in
   *   loop smallBoard
   *
   * let printBoard (board : board) : unit =
   *   let rec loop (brd : board) =
   *     match brd with
   *     | [] -> print_string "";
   *     | hd :: tl ->
   *       List.iteri (fun i cell ->
   *           match i <= 1 with
   *           | true ->
   *             printSmallBoard hd " | ";
   *           | false ->
   *             printSmallBoard hd "")
   *         hd;
   *       loop tl;
   *   in
   *   loop board
   *
   * let createSmallBoard () : smallBoard =
   *   List.init 3 (fun _ -> List.init 3 (fun _ -> E))
   *
   * let createBoard () : board =
   *   List.init 3 (fun _ -> List.init 3 (fun _ -> createSmallBoard()))
   *
   * let () =
   *   let board = createBoard()
   *   in
   *   printBoard (board);
   *   print_endline "" *)
