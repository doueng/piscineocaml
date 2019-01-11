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
