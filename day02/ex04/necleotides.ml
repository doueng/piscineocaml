type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
  phos : phosphate;
  deox: deoxyribose;
  base: nucleobase
}

let generate_nucleotide (c : char) : nucleotide =
  {
    phos = "phosphate";
    deox = "deoxyribose";
    base = match c with
      | 'A' -> A
      | 'T' -> T
      | 'C' -> C
      | 'G' -> G
      | _ -> None
  }

let () =
  let get_base_str (c : nucleobase) : string =
    match c with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"
  in
  let printer nucl =
    print_string (nucl.phos ^ " ");
    print_string (nucl.deox ^ " ");
    print_string (get_base_str nucl.base);
    print_newline ();
  in
  printer (generate_nucleotide 'A');
  printer (generate_nucleotide 'T');
  printer (generate_nucleotide 'C');
  printer (generate_nucleotide 'G');
  printer (generate_nucleotide 'z');
