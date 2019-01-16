class ['a] army (list : 'a list) =
  object
    val mutable _battalion = list
    method get = _battalion
    method add thing = _battalion <- thing :: _battalion
    method delete = _battalion <-
        match _battalion with
        | [] -> [];
        | hd :: tl -> tl
        (* method delete = _battalion <- List.mapi (fun i c -> *)
        (* if i < (List.length _battalion) - 1 then c) _battalion *)
  end
