let () =
  let doctor = new Doctor.doctor "Doctor" 10 (new People.people "sidekick") in
  print_endline doctor#to_string;
  doctor#talk;
  doctor#use_sonic_screwdriver;
  print_endline (doctor#travel_in_time 10 20)#to_string;
