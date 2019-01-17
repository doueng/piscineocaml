let matchName (n : int) =
  match n with
  | 1 -> "Methane"
  | 2 -> "Ethane"
  | 3 -> "Propane"
  | 4 -> "Butane"
  | 5 -> "Pentane"
  | 6 -> "Hexane"
  | 7 -> "Heptane"
  | 8 -> "Octane"
  | 9 -> "Nonane"
  | 10 -> "Decane"
  | 11 -> "Undecane"
  | 12 -> "Dodecane"
  | _ -> invalid_arg "Input to Alkane has to be an int between 1 and 12 inclusive"

let createAtoms n =
  let create (n : int) (a : Atom.atom) : Atom.atom list =
    List.init n (fun _ -> a) in
  create n (new Atom.carbon) @ create ((2 * n) + 2) (new Atom.hydrogen)

class alkane (num : int) =
  object
    inherit Molecule.molecule (matchName(num)) (createAtoms(num))
  end
