let () =
  let rec printer lst =
    match lst with
    | [] -> print_newline ();
    | hd :: tl ->
      print_endline hd#to_string;
      printer tl
  in
  let peopleList = [new People.people "first person";
                    new People.people "second person";
                    new People.people "third person"] in
  let peopleArmy = new Army.army peopleList in
  print_endline "PRINT ARMY";
  printer peopleArmy#get;
  print_endline "DELETE";
  peopleArmy#delete;
  print_endline "DELETE";
  peopleArmy#delete;
  print_endline "PRINT ARMY";
  printer peopleArmy#get;
  print_endline "ADD";
  peopleArmy#add (new People.people "fourth");
  print_endline "PRINT ARMY";
  printer peopleArmy#get;
  print_endline "DELETE";
  peopleArmy#delete;
  print_endline "DELETE";
  peopleArmy#delete;
  print_endline "PRINT ARMY";
  printer peopleArmy#get;

  let doctorList = [new Doctor.doctor "first doctor" 10 (new People.people "sidekick");
                    new Doctor.doctor "second doctor" 10 (new People.people "sidekick");
                    new Doctor.doctor "third doctor" 10 (new People.people "sidekick")] in
  let doctorArmy = new Army.army doctorList in
  print_endline "Print Army";
  printer doctorArmy#get;
  print_endline "DELETE";
  doctorArmy#delete;
  print_endline "DELETE";
  doctorArmy#delete;
  print_endline "Print Army";
  printer doctorArmy#get;
  print_endline "Add person";
  doctorArmy#add (new Doctor.doctor "fourth doctor" 10 (new People.people "sidekick"));
  print_endline "Print Army";
  printer doctorArmy#get;
  print_endline "DELETE";
  doctorArmy#delete;
  print_endline "DELETE";
  doctorArmy#delete;
  print_endline "PRINT ARMY";
  printer doctorArmy#get;

  let dalekList = [new Dalek.dalek; new Dalek.dalek; new Dalek.dalek] in
  let dalekArmy = new Army.army dalekList in
  print_endline "PRINT ARMY";
  printer dalekArmy#get;
  print_endline "DELETE";
  dalekArmy#delete;
  print_endline "DELETE";
  dalekArmy#delete;
  print_endline "PRINT ARMY";
  printer dalekArmy#get;
  print_endline "ADD";
  dalekArmy#add (new Dalek.dalek);
  print_endline "PRINT ARMY";
  printer dalekArmy#get;
  print_endline "DELETE";
  dalekArmy#delete;
  print_endline "DELETE";
  dalekArmy#delete;
  print_endline "PRINT ARMY";
  printer dalekArmy#get;
