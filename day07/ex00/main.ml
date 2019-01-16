let () =
  let person = new People.people "Douglas" in
  print_endline person#to_string;
  person#talk;
  person#die
