type cell = O | X | E | WX | WO
type board = cell list
let boardSize = 81

let getCellString (cell : cell) : string =
  match cell with
  | O -> "O"
  | X -> "X"
  | E -> "-"
  (* fix  *)
  | WX -> "WX"
  | WO -> "WO"

let updateIndexPrint (board : board) (row : int) (col : int) (newCell : cell) : board =
  let updatedCellIndex = (row * 9) + col in
  List.mapi (fun i c -> if i = updatedCellIndex then newCell else c) board

let updateIndex (board : board) (row : int) (col : int) (newCell : cell) : board =
  List.mapi (fun i c -> if i = (row + col) then newCell else c) board

let printBoardB (board : board) : unit =
  let rec loop (b : board) (i : int) (numRows : int) : unit =
    match b with
    | [] -> print_string "\n";
    | a :: b :: c :: tl
      when (i mod 3) = 0 && i <= 9 ->
      print_string ("(" ^ (string_of_int i) ^ ")");
      print_string ((getCellString a) ^ " ");
      print_string ((getCellString b) ^ " ");
      print_string (getCellString b);
      if (numRows mod 3) = 0 then
        print_newline ()
      else
        print_string " | ";
      loop tl (i + 1) (numRows + 1);
    | _ :: _ :: _ :: tl ->
      loop tl (i + 1) numRows;
    | _ -> print_string ""
  in
  loop board 0 1;
  loop board 2 1;
  loop board 1 1

let printBoard (board : board) : unit =
  let rec loop (b : board) (numRows : int) : unit =
    match b with
    | [] -> print_string "";
    | a :: b :: c :: d :: e :: f :: h :: i :: g :: tl ->
      print_string (getCellString a);
      print_string " ";
      print_string (getCellString b);
      print_string " ";
      print_string (getCellString c);
      print_string " | ";
      print_string (getCellString d);
      print_string " ";
      print_string (getCellString e);
      print_string " ";
      print_string (getCellString f);
      print_string " | ";
      print_string (getCellString h);
      print_string " ";
      print_string (getCellString i);
      print_string " ";
      print_string (getCellString g);
      if numRows = 2 || numRows = 5 then
        print_endline "\n---------------------"
      else
        print_newline();
      loop tl (numRows + 1)
    | _ -> print_newline ()
  in
  loop board 0

(* let checkWinRow (board : board) : cell =
 *   let rec loop (b : board) (c : cell) : cell =
 *     if c <> E then
 *       c
 *     else
 *       match b with
 *       | [] -> E;
 *       | a :: b :: c :: tl->
 *         loop tl (if a = b && b = c then a else E);
 *       | _ -> E;
 *   in
 *   loop board E *)

(* Very bad solution, very slow *)
(* let checkWinCol (board : board) : cell =
 *   let rec loop (i : int) : cell =
 *     let first = List.nth board i in
 *     let sec = List.nth board (i + 9) in
 *     let third = List.nth board (i + 18) in
 *     if first = sec && sec = third then
 *       first
 *     else if i < (81 - 18) then
 *       loop (i + 1)
 *     else
 *       E
 *   in
 *   loop 0 *)
(* ["\\"; " "; "/"; " "; "X"; " "; "/"; " "; "\\"] *)

let getWinGrid (winner: cell) : cell list =
  if winner = X then
    List.init 9 (fun _ -> WX)
  else
    List.init 9 (fun _ -> WO)

let updateGrid (board : board) (lastCell : cell) : cell list =
  let checkWin a b c =
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
         else if checkWin c f i then (getWinGrid b)
         else [a; b; c; d; e; f; g; h; i]));
    | _ -> newBoard;
  in
  loop board []


let checkWin (board : board) : cell =
  let rec loop (b : board) (c : cell) : cell =
    if c <> E then
      c
    else
      match b with
      | [] -> E;
      | a :: b :: c ::
        d :: e :: f ::
        g :: h :: i :: tl->
        loop tl (
          (* check row *)
          if a = b && b = c then a
          else if d = e && e = f then d
          else if g = h && h = i then g
          (* check col *)
          else if a = d && d = g then a
          else if d = e && e = f then d
          else if g = h && h = i then g
          else E
        );
      | _ -> E;
  in
  loop board E

(* iniBoard with E
 *   get xy readline
 *   check if xy is valid
 *   updateIndex xy
 *   check Win
 *      if WIN then
          updateIndexPrintWin xy
 *      else
 *        updateIndexPrint xy
 *   update player scores
 *   check if player won
 *   if no player won
 * *   printBoard and loop
 *   else
       print winner graphics
*)

let rec printInput (input : string list) =
  match input with
  | [] -> print_newline ();
  | hd :: tl -> print_string hd;
    printInput tl

let basicBoardTests () =
  let iniBoard = List.init boardSize (fun _ -> E) in
  let winBoardRow = (updateIndex
                       (updateIndex
                          (updateIndex iniBoard 0 0 O)
                          0 1 O)
                       0 2 O)
  in
  let winBoardRowPrint = (updateIndexPrint
                            (updateIndexPrint
                               (updateIndexPrint iniBoard 0 0 O)
                               0 1 O)
                            0 2 O)
  in
  let winBoardCol = (updateIndex
                       (updateIndex
                          (updateIndex iniBoard 0 0 X)
                          1 0 X)
                       2 0 X)
  in
  let winBoardColPrint = (updateIndexPrint
                            (updateIndexPrint
                               (updateIndexPrint iniBoard 0 0 X)
                               1 0 X)
                            2 0 X)
  in
  printBoard (updateGrid winBoardCol X);
  print_newline ();
  printBoard winBoardRowPrint;
  print_endline ((getCellString (checkWin winBoardRow)) ^ " is the winner!!!");
  printBoard winBoardColPrint;
  print_endline ((getCellString (checkWin winBoardCol)) ^ " is the winner!!!")

let getInput () : string list =
  let xy = read_line ()
  in
  if Str.string_match (Str.regexp "[0-8] [0-8]") xy 0 then
    Str.split (Str.regexp " ") xy
  else
    (* CHECK IF WE CAN USE THIS *)
    failwith "Usage: Input has to be of format <0-9> <0-9>"

let rec mainLoop (board : board) (player : cell) =
  let input = (getInput ()) in
  let newBoard = updateIndexPrint board
      (int_of_string (List.nth input 0))
      (int_of_string (List.nth input 1))
      player in
  printBoard newBoard;
  mainLoop newBoard (if player = X then O else X)

let () =
  (* basicBoardTests (); *)
  mainLoop (List.init boardSize (fun _ -> E)) O
