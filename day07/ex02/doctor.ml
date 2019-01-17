class doctor (name : string) (age : int) (sidekick : People.people) =
  object
    initializer print_endline "A doctor has been born!!!"

    val _name : string = name
    val mutable _hp : int = 100
    val mutable _age : int = age
    val _sidekick : People.people = sidekick
    method to_string = "Name: " ^ _name
                       ^ "\nHP: " ^ (string_of_int _hp)
                       ^ "\nAge: " ^ (string_of_int _age)
    method talk =  print_endline "Hi! I’m the Doctor!"
    method use_sonic_screwdriver  = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method travel_in_time (start : int) (arrival : int) = _age <- arrival - start + _age;
      print_endline "
_______(_@_)_______
| POLICE      BOX |
|_________________|
 | _____ | _____ |
 | |###| | |###| |
 | |###| | |###| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ |$_____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 |       |       |
 *****************"
    method private regenerate = _hp <- 100
  end
