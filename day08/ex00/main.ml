let () =
  print_endline (new Atom.hydrogen)#to_string;
  print_endline (new Atom.carbon)#to_string;
  print_endline (new Atom.oxygen)#to_string;
  print_endline (string_of_bool ((new Atom.oxygen)#equals (new Atom.hydrogen)));
  print_endline (string_of_bool ((new Atom.oxygen)#equals (new Atom.oxygen)));
  let hydrogen = new Atom.hydrogen in
  print_endline hydrogen#name;
  print_endline hydrogen#symbol;
  Printf.printf "%d\n" hydrogen#atomic_number
