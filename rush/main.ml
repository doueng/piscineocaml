type cell = O | X | E | WX | WO
type board = cell list
let boardSize = 81

let getCellString (cell : cell) (row : int) (col : int) : string =
  match cell with
  | O -> "O"
  | X -> "X"
  | E -> "-"
  (* fix  *)
  | WX -> List.nth ["\\"; " "; "/"; " "; "X"; " "; "/"; " "; "\\"] ((row * 3) + col)
  | WO -> List.nth ["/"; "-"; "\\"; "|"; " "; "|"; "\\"; "-"; "/"] ((row * 3) + col)

(* For debugging *)
let rec printCells (board : board) : unit =
  match board with
  | [] -> print_newline ();
  | hd :: tl -> print_string (getCellString hd 0 0);
    printCells tl

let updateIndexPrint (board : board) (row : int) (col : int) (newCell : cell) : board =
  List.mapi (fun i c -> if i = row + (col * 9) then newCell else c) board

(* let updatedCellIndex = row + (col * 9) in *)
(* 1 2 = 5 *)
(* 3 3 = 36 *)

let updateIndex (board : board) (row : int) (col : int) (newCell : cell) : board =
  let getRightBlock = ((row / 3) * 27) + ((col / 3) * 9) in
  let updatedCellIndex = getRightBlock + (row mod 3) + (col mod 3) in
  print_endline ("((" ^ (string_of_int updatedCellIndex) ^ "))");
  List.mapi (fun i c -> if i = updatedCellIndex then newCell else c) board

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
         else [a; b; c; d; e; f; g; h; i]));
    | _ -> newBoard;
  in
  loop board []


let convertBoard (board : board) : board =
  let rec loop (b : board) (newBoard : board) first second third (iterator : int) : board =
    match b with
    | a :: b :: c ::
      d :: e :: f ::
      h :: g :: i :: tl->
      let newFirst = first @ [a; b; c] in
      let newSecond = second @ [d; e; f] in
      let newThird = third @ [h; g; i] in
      (* printCells newFirst; *)
      (* printCells newSecond; *)
      printCells newThird;
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
  let winBoardRowPrint = (updateIndexPrint
                            (updateIndexPrint
                               (updateIndexPrint iniBoard 0 0 O)
                               1 0 O)
                            2 0 O)
  in
  let winBoardCol = (updateIndex
                       (updateIndex
                          (updateIndex iniBoard 3 1 X)
                          4 1 X)
                       5 1 X)
  in
  let winBoardColPrint = (updateIndexPrint
                            (updateIndexPrint
                               (updateIndexPrint iniBoard 0 0 X)
                               0 1 X)
                            0 2 X)
  in
  print_endline "before";
  printCells winBoardCol;
  print_endline "before colPrint";
  (* printCells winBoardColPrint; *)
  print_endline "converted";
  printCells (convertBoard winBoardCol);
  printBoard (convertBoard winBoardCol);
  print_newline ();
  printBoard (convertBoard (updateGrid winBoardCol X));
  print_newline ()

let getInput () : string list =
  let xy = read_line ()
  in
  if Str.string_match (Str.regexp "[0-8] [0-8]") xy 0 then
    Str.split (Str.regexp " ") xy
  else
    failwith "Usage: Input has to be of format <0-8> <0-8>"

let rec mainLoop (board : board) (player : cell) =
  let input = (getInput ()) in
  let updatedBoard = updateIndex board
      (int_of_string (List.nth input 0))
      (int_of_string (List.nth input 1))
      player in
  printCells updatedBoard;
  printBoard (convertBoard (updateGrid updatedBoard player));
  mainLoop updatedBoard (if player = X then O else X)

let () =
  (* basicBoardTests (); *)
  mainLoop (List.init boardSize (fun _ -> E)) O
