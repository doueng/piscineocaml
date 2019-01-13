let printWinner (winner : Tictac.cell) : unit =
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

let getCellString (cell : Tictac.cell) (row : int) (col : int) : string =
  match cell with
  | O -> "O"
  | X -> "X"
  | E -> "-"
  | WX -> List.nth ["\\"; " "; "/"; " "; "X"; " "; "/"; " "; "\\"] (((row mod 3) * 3) + col)
  | WO -> List.nth ["/"; "-"; "\\"; "|"; " "; "|"; "\\"; "-"; "/"] (((row mod 3) * 3) + col)

let printBoard (board : Tictac.board) : unit =
  let rec loop (b : Tictac.board) (numRows : int) : unit =
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
