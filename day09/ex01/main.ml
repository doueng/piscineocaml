let () =
  let print_proj p : unit =
    let (s, status, grade) = p in
    print_endline (s ^ " " ^ status ^ (string_of_int grade))
  in
  print_proj (App.fail ("hello", "fail", 100))
