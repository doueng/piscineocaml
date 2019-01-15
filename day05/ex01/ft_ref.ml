type 'a ft_ref = {
  mutable value : 'a;
}

let my_ref = {value = None}

let return (a : 'a) : 'a ft_ref =
  my_ref.value <- a;
  my_ref

let get (r : 'a ft_ref) : 'a =
  my_ref.value

let set (r : 'a ft_ref) (a : 'a) : unit =
  my_ref.value <- a

let bind (r : 'a ft_ref) (f : 'a -> 'b ft_ref) : 'b ft_ref =
  my_ref.value <- (f (r.value)).value;
  my_ref
