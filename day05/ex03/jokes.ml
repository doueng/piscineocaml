let print_joke () =
  let channel = open_in (Sys.argv.(1)) in
  let joke_list = ref [] in
  try
    while true do
      joke_list := input_line channel :: !joke_list
    done
  with End_of_file ->
    close_in channel;
    let jokes = Array.of_list !joke_list in
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
