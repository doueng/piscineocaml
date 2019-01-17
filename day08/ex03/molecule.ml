let create (n : int) (a : Atom.atom) : Atom.atom list =
  List.init n (fun _ -> a)

let nitrogen = new Atom.nitrogen
let hydrogen = new Atom.hydrogen
let carbon = new Atom.carbon
let oxygen = new Atom.oxygen

class virtual molecule (name : string) (atoms : Atom.atom list) =
  object (self)
    method name = name
    method formula =
      let sortedAtoms = List.sort (fun a b -> Pervasives.compare a#symbol b#symbol) atoms in
      let rec loop (res : string) (n : int) (curr : string) (atoms : Atom.atom list)=
        let numStr n = if n > 1 then (string_of_int n) else "" in
        match atoms with
        | [] -> res ^ (numStr n);
        | hd :: tl ->
          if 0 <> Pervasives.compare hd#symbol curr then
            loop (res ^ (numStr n) ^ hd#symbol) 1 hd#symbol tl
          else
            loop res (n + 1) hd#symbol tl
      in
      loop (List.nth sortedAtoms 0)#symbol 0 (List.nth sortedAtoms 0)#symbol sortedAtoms
    method to_string = name ^ "\t\t" ^ self#formula;
    method equals (m : molecule) = Pervasives.compare m#formula self#formula;
  end


class trinitrotoluene =
  object
    inherit molecule "Trinitrotoluene"
        (create 3 nitrogen
         @ create 5 hydrogen
         @ create 6 oxygen
         @ create 7 carbon)
  end

class water =
  object
    inherit molecule "Water"
        (create 2 hydrogen
         @ create 1 oxygen)
  end

class carbonDioxyde =
  object
    inherit molecule "Carbon dioxyde"
        (create 1 carbon
         @ create 2 oxygen)
  end

class glucose =
  object
    inherit molecule "Glucose"
        (create 12 hydrogen
         @ create 6 carbon
         @ create 6 oxygen)
  end

class octane =
  object
    inherit molecule "Octane"
        (create 8 carbon
         @ create 18 hydrogen)
  end

class sucrose =
  object
    inherit molecule "Sucrose"
        (create 12 carbon
         @ create 22 hydrogen
         @ create 11 oxygen)
  end
