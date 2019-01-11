let () =
  let rec loop cards stringify =
    match cards with
    | [] -> print_string "";
    | hd :: tl ->
      print_endline (stringify hd);
      loop tl stringify
  in
  loop Color.all Color.toString;
  loop Color.all Color.toStringVerbose
