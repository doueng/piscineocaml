let getCellIndex (board : Tictac.board) (row : int) (col : int) : int =
  let getRightGrid = ((row / 3) * 27) + ((col / 3) * 9) in
  getRightGrid + ((row mod 3) * 3) + (col mod 3)

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
  else if
    (List.find_opt (fun c -> c = Tictac.E) board = None)
  then (if lastCell = O then WO else WX)
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

let updateFromIndex (board : Tictac.board) index (newCell : Tictac.cell) : Tictac.board =
  List.mapi (fun i c -> if i = index then newCell else c) board

let rec getInput board : int list =
  let xy = read_line () in
  if checkError board xy then getInput board
  else [char_to_int(String.get xy 0) - 1; char_to_int(String.get xy 2) - 1]

let rec mainLoop_X board player =
let input = (Ia.iaPlays board) in
  let updatedBoard = UpdateGrid.updateGrid
      (updateFromIndex board input player)
      player in
  print_endline "IA plays..." ;
  Printing.printBoard (Converter.convertBoard updatedBoard);
  let winner  = checkPlayerWin updatedBoard player in
  if winner <> E then
    Printing.printWinner winner
  else
    mainLoop_O updatedBoard Tictac.O true

and mainLoop_O (board : Tictac.board) (player : Tictac.cell) (ia: bool) =
  print_endline ((Printing.getCellString player 0 0) ^ "'s turn to play.");
  let input = (getInput board) in
  let updatedBoard = UpdateGrid.updateGrid
      (updateIndex board (List.nth input 0) (List.nth input 1) player)
      player in
  Printing.printBoard (Converter.convertBoard updatedBoard);
  let winner  = checkPlayerWin updatedBoard player in
  if winner <> E then
    Printing.printWinner winner
  else (
      if ia = true then mainLoop_X updatedBoard Tictac.X
      else mainLoop_O updatedBoard (if player = Tictac.O then Tictac.X else Tictac.O) false
  )

let () =
  print_endline "Welcome to tictactoe !!!\n Input x and y coorinates between 1 and 9";
  let rec prompt () =
      print_endline "Do you want to play against the AI ? [Y/n]";
      let board = List.init 81 (fun _ -> Tictac.E) in
      let answer = read_line () in
      if ((String.compare answer "Y") = 0) || ((String.compare answer "y") = 0) then
          mainLoop_O board Tictac.O true
      else if ((String.compare answer "N") = 0) || ((String.compare answer "n") = 0) then
          mainLoop_O board Tictac.O false
      else prompt ()
  in prompt () ;
  print_endline ("Thank you ! Wanna play again ? (Press Y to play again, any other key to quit)") ;
  let key = read_line () in
  if ((String.compare key "Y") = 0) || ((String.compare key "y") = 0) then prompt ()
  else ()
  

