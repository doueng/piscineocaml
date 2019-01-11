let rec print_ints cards =
  match cards with
  | [] -> print_newline ();
  | hd :: tl ->
    print_int (Card.Value.toInt hd);
    print_string ", ";
    print_ints tl

let get_hd lst =
  match lst with
  | hd :: _ -> hd;
  | _ -> invalid_arg "Empty lst"

let rec get_last lst =
  match lst with
  | [] -> invalid_arg "Empty lst"
  | hd :: sec :: tl -> get_last tl;
  | hd :: tl -> hd

let rec forall f cards =
  let rec loop cards =
    match cards with
    | [] -> print_newline ();
    | hd :: tl ->
      print_endline (f hd);
      loop tl;
  in
  loop cards

let rec bool_printer b =
  match b with
  | true -> print_endline "True";
  | false -> print_endline "False"

let () =
  print_endline "Print ints";
  print_ints Card.Value.all;
  print_newline ();
  print_endline "Print strings";
  forall Card.toString Card.all;
  print_newline ();
  print_endline "Print verbose strings";
  forall Card.toStringVerbose Card.all;
  print_string "Compare T3 Heart and T3 Spade = ";
  print_int (Card.compare (Card.newCard T3 Heart) (Card.newCard T3 Spade));
  print_newline ();
  print_string "Compare T4 Heart and T3 Spade = ";
  print_int (Card.compare (Card.newCard T4 Heart) (Card.newCard T3 Spade));
  print_newline ();
  print_string "Compare T3 Heart and T5 Spade = ";
  print_int (Card.compare (Card.newCard T3 Heart) (Card.newCard T5 Spade));
  print_newline ();
  print_string "Max (Jack, Queen) = ";
  print_endline (Card.toString (Card.max (Card.newCard Jack Spade) (Card.newCard Queen Heart)));
  print_string "Max (T5, T3) = ";
  print_endline (Card.toString (Card.max (Card.newCard T5 Spade) (Card.newCard T3 Heart)));
  print_string "Min (Jack, Queen) = ";
  print_endline (Card.toString (Card.min (Card.newCard Jack Spade) (Card.newCard Queen Heart)));
  print_string "Min (T5, T3) = ";
  print_endline (Card.toString (Card.min (Card.newCard T5 Spade) (Card.newCard T3 Heart)));
  let cards = [(Card.newCard T2 Spade);
               (Card.newCard T4 Heart);
               (Card.newCard Queen Heart);
               (Card.newCard T5 Spade)]
  in
  print_string "best [2S, 4H, QH, 5S] = ";
  print_endline (Card.toString (Card.best cards));
  let spade_card = Card.newCard T3 Spade
  in
  print_string "isOf 3S Spade = ";
  bool_printer (Card.isOf spade_card Spade);
  print_string "isOf 3S Hearts = ";
  bool_printer (Card.isOf spade_card Heart);

  print_string "isSpade 3S Spade = ";
  bool_printer (Card.isSpade spade_card);
  print_string "isSpade 3S Heart = ";
  bool_printer (Card.isSpade (Card.newCard T8 Heart));

  print_string "isHeart 3H Heart = ";
  bool_printer (Card.isHeart (Card.newCard T8 Heart));
  print_string "isHeart 3S Spade = ";
  bool_printer (Card.isHeart spade_card);

  print_string "isDiamond 3D Diamond = ";
  bool_printer (Card.isDiamond (Card.newCard T8 Diamond));
  print_string "isDiamond 3S Spade = ";
  bool_printer (Card.isDiamond spade_card);

  print_string "isClub 3C Club = ";
  bool_printer (Card.isClub (Card.newCard T8 Club));
  print_string "isClub 3S Spade = ";
  bool_printer (Card.isClub spade_card);

  print_string "best [] = ";
  print_endline (Card.toString (Card.best []))
