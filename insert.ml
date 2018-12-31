(* insert: int -> int list -> int list *)
let rec insert n lst = match lst with
    [] -> [n]
  | first :: rest -> if n < first then n :: lst
                                  else first :: ( insert n rest )

(* test *)
let test1 = insert 1 [2; 3; 4; 5] = [1; 2; 3; 4; 5]
let test2 = insert 3 [1; 2; 4; 5] = [1; 2; 3; 4; 5]
let test3 = insert 5 [1; 2; 3; 4] = [1; 2; 3; 4; 5]
