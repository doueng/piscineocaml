module type FIXED = sig
  type t
  val of_float
    : float -> t
  val of_int
    : int
    -> t
  val to_float
    : t
    -> float
  val to_int
    : t
    -> int
  val to_string : t
    -> string
  val zero : t
  val one
    : t
  val succ : t -> t
  val pred : t -> t
  val min
    : t -> t -> t
  val max
    : t -> t -> t
  val gth
    : t -> t -> bool
  val lth
    : t -> t -> bool
  val gte
    : t -> t -> bool
  val lte
    : t -> t -> bool
  val eqp
    : t -> t -> bool
  (** physical equality
  *)
  val eqs
    : t -> t -> bool
  (** structural equality *)
  val add
    : t -> t -> t
  val sub
    : t -> t -> t
  val mul
    : t -> t -> t
  val div
    : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end

module Pair : PAIR = struct let pair = ( 21, 42 ) end

module type MAKEFST =
  functor (Pair : PAIR) -> VAL

module MakeFst : MAKEFST =
  functor (Pair : PAIR) ->
  struct
    let x = Pervasives.fst Pair.pair
  end

module MakeSnd : MAKEFST =
  functor (Pair : PAIR) ->
  struct
    let x = Pervasives.snd Pair.pair
  end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

module type FRACTIONAL_BITS = sig val bits : int end

module type MAKE =
  functor (Bits : FRACTIONAL_BITS) -> FIXED

module Make : MAKE =
  functor (Bits : FRACTIONAL_BITS) ->
  struct

  end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)


let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
