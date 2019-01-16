let () =
  let human = new People.people "Douglas" in
  let doctor = new Doctor.doctor "Doctor" in
  let dalek = new Dalek.dalek in
  print_endline human#to_string;
  print_endline doctor#to_string;
  print_endline dalek#to_string;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#exterminate human;
  print_endline dalek#to_string;
  doctor#use_sonic_screwdriver;
  dalek#die;
