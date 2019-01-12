type cell = O | X | E
type board = cell list
let boardSize = 81

let getCellString (cell : cell) : string =
  match cell with
  | O -> "O"
  | X -> "X"
  | E -> "-"

let updateBoardPrint (board : board) (row : int) (col : int) (newCell : cell) : board =
  let updatedCellIndex = (row * 9) + col in
  List.mapi (fun i c -> if i = updatedCellIndex then newCell else c) board

let updateBoard (board : board) (row : int) (col : int) (newCell : cell) : board =
  List.mapi (fun i c -> if i = (row + col) then newCell else c) board

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
          if a = b && b == c then a
          else if d = e && e == f then d
          else if g = h && h == i then g
          (* check col *)
          else if a = d && d = g then a
          else if d = e && e = f then d
          else if g = h && h = i then g
          else E
        );
      | _ -> E;
  in
  loop board E

let () =
  let iniBoard = List.init boardSize (fun _ -> E) in
  let winBoardRow = (updateBoard
                       (updateBoard
                          (updateBoard iniBoard 0 0 O)
                          0 1 O)
                       0 2 O)
  in
  let winBoardRowPrint = (updateBoardPrint
                            (updateBoardPrint
                               (updateBoardPrint iniBoard 0 0 O)
                               0 1 O)
                            0 2 O)
  in
  let winBoardCol = (updateBoard
                       (updateBoard
                          (updateBoard iniBoard 0 0 X)
                          1 0 X)
                       2 0 X)
  in
  let winBoardColPrint = (updateBoardPrint
                            (updateBoardPrint
                               (updateBoardPrint iniBoard 0 0 X)
                               1 0 X)
                            2 0 X)
  in
  printBoard winBoardRowPrint;
  print_endline ((getCellString (checkWin winBoardRow)) ^ " is the winner!!!");
  printBoard winBoardColPrint;
  print_endline ((getCellString (checkWin winBoardCol)) ^ " is the winner!!!")
