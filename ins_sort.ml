#use "insert.ml"

(* ins_sort: int list -> int list *)
let rec ins_sort lst = match lst with
  [] -> lst
  | first :: rest -> insert first ( ins_sort rest )

(* test *)
let test1 = ins_sort [1; 2; 3] = [1; 2; 3]
let test2 = ins_sort [1] = [1]
let test3 = ins_sort [3; 2; 1] = [1; 2; 3]
