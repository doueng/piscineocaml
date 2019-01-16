class dalek =
  object (self)

    val _name : string = "Dalek" ^ (fun _ ->
        Random.self_init();
        let s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
        let len = String.length s in
        String.init 3 (fun _ -> String.get s (Random.int len)))()
    val mutable _hp : int = 100
    val mutable _shield : bool = true
    method to_string = "Name: " ^ _name ^
                       "\nHP: " ^ (string_of_int _hp) ^
                       "\nShield: " ^ (string_of_bool _shield)
    method talk =
      let sayings = [
        "Explain! Explain!";
        "Exterminate! Exterminate!";
        "I obey!";
        "You are the Doctor! You are the enemy of the Daleks!"
      ] in
      let rec loop (lst : string list) =
        Random.self_init();
        match lst with
        | [] -> loop sayings
        | hd :: tl when 2 = (Random.int 4) -> print_endline hd
        | hd :: tl -> loop tl
      in loop sayings
    method exterminate (person : People.people) = person#die; _shield <- not _shield
    method die = print_endline "Emergency Temporal Shift!"
  end
