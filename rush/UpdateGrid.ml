let getWinGrid (winner: Tictac.cell) (gridNum : int) : Tictac.cell list =
  print_endline ((Printing.getCellString winner 0 0) ^ " has won grid " ^ (string_of_int gridNum));
  List.init 9 (fun _ -> if winner = X then Tictac.WX else Tictac.WO)

let checkWin (a : Tictac.cell) (b : Tictac.cell) (c : Tictac.cell) : bool =
  a = b && b = c && (a = X || a = O)

let updateGrid (board : Tictac.board) (lastCell : Tictac.cell) : Tictac.board =
  let rec loop (b : Tictac.board) (newBoard : Tictac.board) (gridNum : int) : Tictac.cell list =
    match b with
    | [] -> newBoard;
    | a :: b :: c ::
      d :: e :: f ::
      g :: h :: i :: tl->
      loop tl (
        newBoard @
        (* check row *)
        (if checkWin a b c then (getWinGrid a gridNum)
         else if checkWin d e f then (getWinGrid d gridNum)
         else if checkWin g h i then (getWinGrid g gridNum)
         (* check col *)
         else if checkWin a d g then (getWinGrid a gridNum)
         else if checkWin b e h then (getWinGrid b gridNum)
         else if checkWin c f i then (getWinGrid c gridNum)
         (* check diagonal *)
         else if checkWin a e i then (getWinGrid e gridNum)
         else if checkWin g e c then (getWinGrid e gridNum)
         (* check if all cells have been filled *)
         else if (List.find_opt (fun c -> (c = Tictac.E) || (c = Tictac.WO) || (c = Tictac.WX))
                    [a; b; c; d; e; f; g; h; i])
                 = None then (getWinGrid lastCell gridNum)
         else [a; b; c; d; e; f; g; h; i]))
        (gridNum + 1);
    | _ -> newBoard;
  in
  loop board [] 1
