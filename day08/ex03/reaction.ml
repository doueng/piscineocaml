class virtual reaction (m1 : Molecule.molecule list) (m2 : Molecule.molecule list) =
  object
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool
  end
