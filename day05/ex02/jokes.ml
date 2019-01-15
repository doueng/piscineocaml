let print_joke () =
  let jokes = Array.of_list [
      "How do you feel when there is no coffee?\nDepresso.";
      "Why don't oysters give to charity?\nBecause they are shellfish!";
      "Why don't dinosaurs talk?\nBecause they're dead.";
      "What do you call a wizard that has run out of spells?\nA was-ard!";
      "How do snails fight?\nThey slug it out!"
    ] in
  Random.self_init ();
  print_endline (jokes.(Random.int (Array.length jokes)))

let () =
  print_joke();
  print_joke();
  print_joke();
  print_joke();
  print_joke();
  print_joke();
  print_joke();
  print_joke();
  print_joke()
