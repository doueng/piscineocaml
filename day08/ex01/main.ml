let () =
  let tnt = new Molecule.trinitrotoluene in
  let water = new Molecule.water in
  let carbonDioxyde = new Molecule.carbonDioxyde in
  let glucose = new Molecule.glucose in
  let octane = new Molecule.octane in
  let sucrose = new Molecule.sucrose in
  let printMolecule m =
    print_endline (m#name ^ "  " ^ m#formula) in
  printMolecule tnt;
  printMolecule water;
  printMolecule carbonDioxyde;
  printMolecule glucose;
  printMolecule octane;
  printMolecule sucrose;
