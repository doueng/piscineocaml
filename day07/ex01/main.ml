let () =
  let doctor = new Doctor.doctor "Doctor" 10 (new People.people "sidekick") in
  print_endline doctor#to_string;
  doctor#talk;
  doctor#use_sonic_screwdriver;
  doctor#travel_in_time 10 20;
  print_endline doctor#to_string;
