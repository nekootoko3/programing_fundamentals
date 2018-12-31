(* add_to_each: prepend int to each lists *)
(* add_to_each: int -> int list -> int list *)
let add_to_each n lst = match lst with
  [] -> []
  | first :: rest -> ( n :: first ) :: add_to_each n rest

(* test *)
let test1 = add_to_each 1 [] = []
let test2 = add_to_each 1 [[2]] = [[1;2]]
let test3 = add_to_each 1 [[2;3];[2;3;4]] = [[1;2;3];[1;2;3;4]]
