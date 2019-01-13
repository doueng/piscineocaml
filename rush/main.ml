type cell = O | X | E | WX | WO
type board = cell list
let boardSize = 81

let getCellString (cell : cell) (row : int) (col : int) : string =
  match cell with
  | O -> "O"
  | X -> "X"
  | E -> "-"
  | WX -> List.nth ["\\"; " "; "/"; " "; "X"; " "; "/"; " "; "\\"] (((row mod 3) * 3) + col)
  | WO -> List.nth ["/"; "-"; "\\"; "|"; " "; "|"; "\\"; "-"; "/"] (((row mod 3) * 3) + col)

let rec printCells (board : board) : unit =
  match board with
  | [] -> print_newline();
  | hd :: tl -> print_string (getCellString hd 0 0);
    printCells tl

let getCellIndex (board : board) (row : int) (col : int) : int =
  let getRightGrid = ((row / 3) * 27) + ((col / 3) * 9) in
  getRightGrid + ((row mod 3) * 3) + (col mod 3)

let updateIndex (board : board) (row : int) (col : int) (newCell : cell) : board =
  List.mapi (fun i c -> if i = (getCellIndex board row col) then newCell else c) board

let printBoard (board : board) : unit =
  let rec loop (b : board) (numRows : int) : unit =
    match b with
    | [] -> print_string "";
    | a :: b :: c :: d :: e :: f :: h :: i :: g :: tl ->
      print_string (getCellString a numRows 0);
      print_string " ";
      print_string (getCellString b numRows 1);
      print_string " ";
      print_string (getCellString c numRows 2);
      print_string " | ";
      print_string (getCellString d numRows 0);
      print_string " ";
      print_string (getCellString e numRows 1);
      print_string " ";
      print_string (getCellString f numRows 2);
      print_string " | ";
      print_string (getCellString h numRows 0);
      print_string " ";
      print_string (getCellString i numRows 1);
      print_string " ";
      print_string (getCellString g numRows 2);
      if numRows = 2 || numRows = 5 then
        print_endline "\n---------------------"
      else
        print_newline();
      loop tl (numRows + 1)
    | _ -> print_newline ()
  in
  loop board 0

let getWinGrid (winner: cell) : cell list =
  List.init 9 (fun _ -> if winner = X then WX else WO)

(* LAST CELL *)
let updateGrid (board : board) (lastCell : cell) : cell list =
  let checkWin (a : cell) (b : cell) (c : cell) : bool =
    a = b && b = c && a <> E
  in
  let rec loop (b : board) (newBoard : board) : cell list =
    match b with
    | [] -> newBoard;
    | a :: b :: c ::
      d :: e :: f ::
      g :: h :: i :: tl->
      loop tl (
        (* check row *)
        newBoard @
        (if checkWin a b c then (getWinGrid a)
         else if checkWin d e f then (getWinGrid d)
         else if checkWin g h i then (getWinGrid g)
         (* check col *)
         else if checkWin a d g then (getWinGrid a)
         else if checkWin b e h then (getWinGrid b)
         else if checkWin c f i then (getWinGrid c)
         (* check diagonal *)
         else if checkWin a e i then (getWinGrid e)
         else if checkWin g e c then (getWinGrid e)
         (* check if all cells have been filled *)
         else if (List.find_opt (fun c -> c = E)
                    [a; b; c; d; e; f; g; h; i])
                 = None then (getWinGrid lastCell)
         else [a; b; c; d; e; f; g; h; i]));
    | _ -> newBoard;
  in
  loop board []

(* let getCellIndex (board : board) (row : int) (col : int) : int = *)
let checkPlayerWin (board : board) (lastCell : cell) : cell =
  let checkWin (a : cell) (b : cell) (c : cell) : bool =
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
  printCells [a; b; c; d; e; f; g; h; i];
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
  else if (List.find_opt (fun c -> c = E) board = None) then lastCell
  else E

let convertBoard (board : board) : board =
  let rec loop (b : board) (newBoard : board) first second third (iterator : int) : board =
    match b with
    | a :: b :: c ::
      d :: e :: f ::
      h :: g :: i :: tl->
      let newFirst = first @ [a; b; c] in
      let newSecond = second @ [d; e; f] in
      let newThird = third @ [h; g; i] in
      if (iterator mod 3) = 0 then
        loop tl (newBoard @ newFirst @ newSecond @ newThird) [] [] [] (iterator + 1)
      else
        loop tl newBoard newFirst newSecond newThird (iterator + 1);
    | _ -> newBoard;
  in
  loop board [] [] [] [] 1


let basicBoardTests () =
  let iniBoard = List.init boardSize (fun _ -> E) in
  let winBoardRow = (updateIndex
                       (updateIndex
                          (updateIndex iniBoard 0 0 O)
                          1 0 O)
                       2 0 O)
  in
  let winBoardCol = (updateIndex
                       (updateIndex
                          (updateIndex iniBoard 3 1 X)
                          4 1 X)
                       5 1 X)
  in
  print_endline "before";
  print_endline "before colPrint";
  (* printCells winBoardColPrint; *)
  print_endline "converted";
  printBoard (convertBoard winBoardCol);
  print_newline ();
  printBoard (convertBoard (updateGrid winBoardCol X));
  print_newline

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

let printWinner (winner : cell) : unit =
  if winner = WO then
    begin
      print_endline "
                   ooo OOO OOO ooo
               oOO                 OOo
           oOO                         OOo
        oOO                               OOo
      oOO                                   OOo
    oOO                                       OOo
   oOO                                         OOo
  oOO                                           OOo
 oOO                                             OOo
 oOO            O IS THE WINNER!!!               OOo
 oOO                                             OOo
 oOO                                             OOo
 oOO                                             OOo
  oOO                                           OOo
   oOO                                         OOo
    oOO                                       OOo
      oOO                                   OOo
        oO                                OOo
           oOO                         OOo
               oOO                 OOo
                   ooo OOO OOO ooo"

    end
  else
    begin
      print_endline "THE WINNER IS X";
      print_endline "
    (_ _)  (_ _)
      \\    //
       \\  //
        \\//
         ||
        //\\
       //  \\
     _//    \\_
    (___)  (___)
"
    end

let rec getInput board : int list =
  let xy = read_line ()
  in
  if checkError board xy then getInput board
  else
    [char_to_int(String.get xy 0) - 1; char_to_int(String.get xy 2) - 1]

let rec mainLoop (board : board) (player : cell) =
  let input = (getInput board) in
  let updatedBoard = updateGrid
      (updateIndex board (List.nth input 0) (List.nth input 1) player)
      player in
  printBoard (convertBoard updatedBoard);
  let winner  = checkPlayerWin updatedBoard player in
  if winner <> E then
    printWinner winner
  else
    mainLoop updatedBoard (if player = X then O else X)

let () =
  (* basicBoardTests (); *)
  mainLoop (List.init boardSize (fun _ -> E)) O
