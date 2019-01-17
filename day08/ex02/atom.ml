class virtual atom (name : string) (symbol : string) (atomic_number : int) =
  object
    method name : string = name
    method symbol : string = symbol
    method atomic_number : int = atomic_number
    method to_string : string = name ^
                                " " ^ symbol ^
                                " " ^ (string_of_int atomic_number)
    method equals (a : atom) : bool = a#atomic_number = atomic_number
  end

class hydrogen =
  object
    inherit atom "hydrogen" "H" 1
  end

class carbon =
  object
    inherit atom "carbon" "C" 6
  end

class oxygen =
  object
    inherit atom "oxygen" "O" 8
  end

class nitrogen =
  object
    inherit atom "nitrogen" "N" 7
  end
