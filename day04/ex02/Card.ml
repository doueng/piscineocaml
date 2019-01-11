module Color = struct
  type t = Spade | Heart | Diamond | Club

  let all : t list = [Spade; Heart; Diamond; Club]

  let toString (card : t) : string =
    match card with
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"

  let toStringVerbose (card : t) : string =
    match card with
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"
end

module Value = struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  let all : t list = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

  let toInt (card : t) : int =
    match card with
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13

  let toString (card : t) : string =
    match card with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"

  let toStringVerbose (card : t) : string =
    match card with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King "
    | As -> "As"

  let next (card : t) : t =
    match card with
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg (toString As)

  let previous (card : t) : t =
    match card with
    | T2 -> invalid_arg (toString T2)
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King
end

type t = {value : Value.t;
          color : Color.t}

let newCard (newVal : Value.t) (newCol : Color.t) : t =
  {value = newVal;
   color = newCol}

let allSpades : t list =
  let rec loop values list color =
    match values with
    | [] -> list;
    | hd :: tl ->
      loop tl ({value = hd; color = color} :: list) color
  in
  loop Value.all [] Color.Spade

let allHearts : t list =
  let rec loop values list color =
    match values with
    | [] -> list;
    | hd :: tl ->
      loop tl ({value = hd; color = color} :: list) color
  in
  loop Value.all [] Color.Heart

let allDiamonds : t list =
  let rec loop values list color =
    match values with
    | [] -> list;
    | hd :: tl ->
      loop tl ({value = hd; color = color} :: list) color
  in
  loop Value.all [] Color.Diamond

let allClubs : t list =
  let rec loop values list color =
    match values with
    | [] -> list;
    | hd :: tl ->
      loop tl ({value = hd; color = color} :: list) color
  in
  loop Value.all [] Color.Club

let all : t list =
  allSpades @ allHearts @ allDiamonds @ allClubs

let getValue (card : t) : Value.t =
  card.value

let getColor (card : t) : Color.t =
  card.color

let toString (card : t) : string =
  (Value.toString card.value) ^
  (Color.toString card.color)

let toStringVerbose (card : t) : string =
  "Card(" ^
  (Value.toStringVerbose card.value) ^
  ", " ^
  (Color.toStringVerbose card.color) ^
  ")"

let compare (a : t) (b : t) : int =
  let first = Value.toInt (getValue a) in
  let second = Value.toInt (getValue b) in
  if first = second then
    0
  else if first < second then
    (-1)
  else
    1

let max (a : t) (b : t) : t =
  match a.value >= b.value with
  | true -> a;
  | false -> b

let min (a : t) (b : t) : t =
  match a.value <= b.value with
  | true -> a;
  | false -> b

let best (cards : t list) : t =
  match cards with
  | [] -> invalid_arg "Don't pass in an empty list to the best function!!!";
  | hd :: tl -> List.fold_left (fun best_card card -> max best_card card) hd tl

let isOf (card : t) (color : Color.t) : bool =
  card.color = color

let isSpade (card : t) : bool =
  card.color = Spade

let isHeart (card : t) : bool =
  card.color = Heart

let isDiamond (card : t) : bool =
  card.color = Diamond

let isClub (card : t) : bool =
  card.color = Club
