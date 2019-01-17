(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ia.ml                                              :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: rmicolon <marvin@42.fr>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2019/01/13 17:44:28 by rmicolon          #+#    #+#             *)
(*   Updated: 2019/01/13 23:24:29 by rmicolon         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module IaGrid = struct

    let getLinedGrids (board: Tictac.cell list) i : Tictac.cell list list =
        let vgrids = (
            if i >= 54 then
                [List.nth board (i - 54); List.nth board (i - 27 )]
            else if i >= 27 then
                [List.nth board (i - 27); List.nth board (i + 27)]
            else
                [List.nth board (i + 27); List.nth board (i + 54)]
        ) in
        let hgrids = (
            if (i mod 27) >= 18 then
                [List.nth board (i - 18); List.nth board (i - 9 )]
            else if (i mod 27) >= 9 then
                [List.nth board (i - 9); List.nth board (i + 9)]
            else
                [List.nth board (i + 9); List.nth board (i + 18)]
        ) in
        let dlgrids = (
            if i >= 72 then
                [List.nth board (i - 36); List.nth board (i - 72)]
            else if i >= 36 && i < 45 then
                [List.nth board (i - 36); List.nth board (i + 36)]
            else if i < 9 then
                [List.nth board (i + 36); List.nth board (i + 72)]
            else 
                [Tictac.E; Tictac.E]
        ) in
        let drgrids = (
            if i >= 54 && i < 63 then
                [List.nth board (i - 18); List.nth board (i - 36)]
            else if i >= 36 && i < 45 then
                [List.nth board (i - 18); List.nth board (i + 18)]
            else if i >= 18 && i < 27 then
                [List.nth board (i + 18); List.nth board (i + 36)]
            else 
                [Tictac.E; Tictac.E]
        ) in
        [vgrids; hgrids; dlgrids; drgrids]

    let scoreGrid (board: Tictac.cell list) i : int =
        let linedgrids = getLinedGrids board i in
        let rec gridAllowWin (l: Tictac.cell list list) : int = match l with
            | [] -> 0
            | hd::tl -> (
                if (List.nth hd 0) = Tictac.WX && (List.nth hd 1) = Tictac.WX then 6 + (gridAllowWin tl)
                else if ((List.nth hd 0) = Tictac.WX && (List.nth hd 1) <> Tictac.WX)
                    || ((List.nth hd 1) = Tictac.WX && (List.nth hd 0) <> Tictac.WX)
                    then 1 + (gridAllowWin tl)
                else (gridAllowWin tl)
            ) in
        let rec gridAllowBlock l : int = match l with
            | [] -> 0
            | hd::tl -> (
                if (List.nth hd 0) = Tictac.WO && (List.nth hd 1) = Tictac.WO then 6 + (gridAllowBlock tl)
                else gridAllowBlock tl
            ) in
        let isGridCenter =
            let isVCenter =
                if (i mod 27) >= 9 && (i mod 27) < 18 then 1
                else 0
            in
            let isHCenter =
                if i >= 27 && i < 54 then 1
                else 0
            in
            isVCenter + isHCenter
        in 
        (gridAllowWin linedgrids) + (gridAllowBlock linedgrids) + isGridCenter
end

module IaCell = struct

     let getLinedCells (board: Tictac.cell list) i : Tictac.cell list list =
        let vcells = (
            if (i mod 9) >= 6 then
                [List.nth board (i - 6); List.nth board (i - 3)]
            else if i >= 3 then
                [List.nth board (i - 3); List.nth board (i + 3)]
            else
                [List.nth board (i + 3); List.nth board (i + 6)]
        ) in
        let hcells = (
            if (i mod 3) >= 2 then
                [List.nth board (i - 1); List.nth board (i - 2)]
            else if (i mod 3) >= 1 then
                [List.nth board (i - 1); List.nth board (i + 1)]
            else
                [List.nth board (i + 1); List.nth board (i + 2)]
        ) in
        let dlcells = (
            if (i mod 9) = 8 then
                [List.nth board (i - 4); List.nth board (i - 8)]
            else if (i mod 9) = 4 then
                [List.nth board (i - 4); List.nth board (i + 4)]
            else if (i mod 9) = 0 then
                [List.nth board (i + 4); List.nth board (i + 8)]
            else 
                [Tictac.E; Tictac.E] 
        ) in
        let drcells = (
            if (i mod 9) = 6 then
                [List.nth board (i - 2); List.nth board (i - 4)]
            else if (i mod 9) = 4 then
                [List.nth board (i - 2); List.nth board (i + 2)]
            else if (i mod 9) = 2 then
                [List.nth board (i + 2); List.nth board (i + 4)]
            else 
                [Tictac.E; Tictac.E] 
        ) in
        [vcells; hcells; dlcells; drcells]

    let scoreCell (board: Tictac.cell list) i : int =
        let linedcells = getLinedCells board i in
        let rec cellAllowWin (l: Tictac.cell list list) : int = match l with
            | [] -> 0
            | hd::tl -> (
                if (List.nth hd 0) = Tictac.X && (List.nth hd 1) = Tictac.X then 4 + (cellAllowWin tl)
                else if ((List.nth hd 0)  = Tictac.X && (List.nth hd 1) = Tictac.E)
                    || ( (List.nth hd 1) = Tictac.X && (List.nth hd 0) = Tictac.E)
                    then 1 + (cellAllowWin tl)
                else (cellAllowWin tl)
            ) in
        let rec cellAllowBlock l : int = match l with
            | [] -> 0
            | hd::tl -> (
                if (List.nth hd 0) = Tictac.O && (List.nth hd 1) = Tictac.O then 5 + (cellAllowBlock tl)
                else cellAllowBlock tl
            ) in
        let isCellCenter =
            let isVCenter =
                if (i mod 3) = 1 then 1
                else 0
            in
            let isHCenter =
                if (i mod 9) >= 3 && (i mod 9) < 6 then 1
                else 0
            in
            isVCenter + isHCenter
        in 
        (cellAllowWin linedcells) + (cellAllowBlock linedcells) + isCellCenter

end

let scoreBoard (board: Tictac.cell list) =
    let rec iter b i =
        match b with
        | [] -> []
        | hd::tl ->
                match hd with
                | Tictac.E -> ((IaGrid.scoreGrid board i) + (IaCell.scoreCell board i)) :: (iter tl (i+1))
                | _ -> (-1) :: (iter tl (i+1))
    in iter board 0

let rec printScores sb i =
    match sb with
    | [] -> ()
    | hd::tl -> (
        if (i mod 27) = 0 then (
            print_string ("\n | Grid " ^ string_of_int (i / 9) ^ " = ") ;
            print_string (string_of_int(hd)) ;
            printScores tl (i+1)
        )
        else if (i mod 9) = 0 then (
            print_string (" | Grid " ^ string_of_int (i / 9) ^ " = ") ;
            print_string (string_of_int(hd)) ;
            printScores tl (i+1)
        )
        else (
            print_string (" ") ;
            print_string (string_of_int(hd)) ;
            printScores tl (i+1)
        )
    )


let chooseCell (scores: int list) : int =
    let rec loop (s: int list) (i: int) (imax: int) (max: int) : int = 
        match s with
        | [] -> imax
        | hd::tl -> (
            if hd > max then loop tl (i+1) i hd
            else loop tl (i+1) imax max
    )
    in loop scores 0 (-1) (-1)

let iaPlays board =
    let scores = scoreBoard board in
    chooseCell scores

