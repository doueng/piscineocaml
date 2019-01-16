class people name =
  object
    initializer print_endline "A new person has been born!!!!!!"

    val _name : string = name
    val _hp : int = 100

    method to_string = "Name: " ^ _name ^ "\nHP: " ^ (string_of_int _hp)
    method talk = print_endline
        ("I'm " ^ _name ^ "! Do you know the Doctor?")

    method die = print_endline "Aaaarghh!"

  end
