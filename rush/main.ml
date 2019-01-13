let getCellIndex (board : Tictac.board) (row : int) (col : int) : int =
  let getRightGrid = ((row / 3) * 27) + ((col / 3) * 9) in
  getRightGrid + ((row mod 3) * 3) + (col mod 3)

let rec printCells board =
  match board with
  | [] -> print_newline();
  | hd :: tl -> print_string (Printing.getCellString hd 0 0);
    printCells tl

let checkPlayerWin (board : Tictac.board) (lastCell : Tictac.cell) : Tictac.cell =
  let checkWin (a : Tictac.cell) (b : Tictac.cell) (c : Tictac.cell) : bool =
    a = b && b = c && (a = WO || a = WX) in
  let a = List.nth board (getCellIndex board 0 0) in
  let b = List.nth board (getCellIndex board 0 3) in
  let c = List.nth board (getCellIndex board 0 6) in
  let d = List.nth board (getCellIndex board 3 0) in
  let e = List.nth board (getCellIndex board 3 3) in
  let f = List.nth board (getCellIndex board 3 6) in
  let g = List.nth board (getCellIndex board 6 0) in
  let h = List.nth board (getCellIndex board 6 3) in
  let i = List.nth board (getCellIndex board 6 6) in
  (* check row *)
  if checkWin a b c then a
  else if checkWin d e f then d
  else if checkWin g h i then g
  (* check col *)
  else if checkWin a d g then a
  else if checkWin b e h then b
  else if checkWin c f i then c
  (* check diagonal *)
  else if checkWin a e i then a
  else if checkWin g e c then g
  else if (List.find_opt (fun c -> c = Tictac.E) board = None) then lastCell
  else E

let char_to_int c =
  int_of_string (String.make 1 c)

let checkError board str =
  if String.length str <> 3
  || String.get str 0 < '1'
  || String.get str 0 > '9'
  || String.get str 1 <> ' '
  || String.get str 2 < '1'
  || String.get str 2 > '9'
  then ( print_endline "Wrong input formating, please try again: " ; true )
  else if List.nth board (getCellIndex board (char_to_int(String.get str 0) - 1) (char_to_int(String.get str 2) - 1)) <> E
  then ( print_endline "Cell has already been played, please try again: "; true )
  else false

let updateIndex (board : Tictac.board) (row : int) (col : int) (newCell : Tictac.cell) : Tictac.board =
  List.mapi (fun i c -> if i = (getCellIndex board row col) then newCell else c) board

let rec getInput board : int list =
  let xy = read_line ()
  in
  if checkError board xy then getInput board
  else
    [char_to_int(String.get xy 0) - 1; char_to_int(String.get xy 2) - 1]

let rec mainLoop (board : Tictac.board) (player : Tictac.cell) =
  print_endline ((Printing.getCellString player 0 0) ^ "'s turn to play.");
  let input = (getInput board) in
  let updatedBoard = UpdateGrid.updateGrid
      (updateIndex board (List.nth input 0) (List.nth input 1) player)
      player in
  Printing.printBoard (Converter.convertBoard updatedBoard);
  let winner  = checkPlayerWin updatedBoard player in
  if winner <> E then
    Printing.printWinner winner
  else
    mainLoop updatedBoard (if player = X then O else X)

let () =
  print_endline "Welcome to tictactoe !!!\n Input x and y coorinates between 1 and 9";
  mainLoop (List.init 81 (fun _ -> Tictac.E)) O
