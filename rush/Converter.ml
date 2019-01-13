let convertBoard (board : Tictac.board) : Tictac.board =
  let rec loop (b : Tictac.board) (newBoard : Tictac.board) first second third (iterator : int) : Tictac.board =
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
