module type TRY =
sig
  type 'a t = Success of 'a | Failure of exn
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val recover : 'a t -> (exn -> 'a t) -> 'a t
  val filter : 'a t -> ('a -> bool) -> 'a t
  val flatten : 'a t t -> 'a t
end

module Try : TRY =
struct
  type 'a t = Success of 'a | Failure of exn

  let return (x : 'a) : 'a t = Success x

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    match x with
    | Success (a) -> f a;
    | Failure (exn) -> Failure exn

  let recover (x : 'a t) (f : exn -> 'a t) : 'a t =
    match x with
    | Success (a) -> Success a;
    | Failure (exn) -> f exn

  let filter (x : 'a t) (f : 'a -> bool) : 'a t =
    match x with
    | Success (a) when f a = true -> Success a;
    | Success (a) -> Failure (Invalid_argument "");
    | Failure (exn) -> Failure exn

  let flatten (x : 'a t t) : 'a t =
    match x with
    | Success (Success (a)) -> Success a;
    | _ -> Failure (Invalid_argument "")
end

let () =
  let print_ret x =
    match Try.return x with
    | Try.Success (ret) -> print_endline ret;
    | Try.Failure (ret) -> print_endline "Failure"
  in
  print_ret "return Success";
  print_endline "----------------------------------------";

  let print_bind x f =
    match Try.bind x f with
    | Success (ret) -> print_endline ret;
    | Failure (ret) -> print_endline "bind Failure"
  in
  print_bind (Success "rofl") (fun (a : 'a) -> Failure (Invalid_argument "hello"));
  print_bind (Success "rofl") (fun (a : 'a) -> Success "bind Success" );
  print_bind (Failure (Invalid_argument "lol")) (fun (a : 'a) -> Failure (Invalid_argument "hello"));
  print_bind (Failure (Invalid_argument "lol")) (fun (a : 'a) -> Success "hello" );
  print_endline "----------------------------------------";

  let print_recover  x f =
    match Try.recover x f with
    | Success (ret) -> print_endline ret;
    | Failure (ret) -> print_endline "Failure"
  in
  print_recover (Success "recover Success") (fun (e : exn) -> Try.Success "lol");
  print_recover (Failure (Invalid_argument "")) (fun (e : exn) -> Try.Success "recover Success");
  print_endline "----------------------------------------";

  let print_filter x f =
    match Try.filter x f with
    | Success (ret) -> print_endline ret;
    | Failure (ret) -> print_endline "filter Failure"
  in
  print_filter (Success "filter Success") (fun (e : 'a) -> true);
  print_filter (Success "filter Success") (fun (e : 'a) -> false);
  print_filter (Failure (Invalid_argument "")) (fun (e :'a) -> true);
  print_filter (Failure (Invalid_argument "")) (fun (e :'a) -> false);
  print_endline "----------------------------------------";

  let print_flatten x =
    match Try.flatten x with
    | Success (ret) -> print_endline ret;
    | Failure (ret) -> print_endline "flatten Failure"
  in
  print_flatten (Success (Success "flatten Sucess Sucesss"));
  print_flatten (Try.return (Try.bind (Success "random stuff") (fun a -> Failure (Invalid_argument ""))));
  print_flatten (Success (Try.recover (Failure (Invalid_argument "")) (fun _ -> Success "flatten Sucess Failure")));
  print_endline "----------------------------------------";
