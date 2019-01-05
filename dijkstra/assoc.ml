(* assoc : string -> (string * float) list -> float *)
let rec assoc ekimei0 lst = match lst with
    [] -> raise Not_found
  | (ekimei, kyori) :: rest ->
      if ekimei0 = ekimei then kyori else assoc ekimei0 rest

(* test *)
let test1 = assoc "後楽園" [] = Not_found 
let test2 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8
let test3 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = Not_found
