(* minimum: int list -> int *)
let rec minimum lst = match lst with
  [] -> max_int
  | first :: rest -> let min = minimum rest in
                       if first < min then first
                       else min

let test1 = minimum [3] = 3
let test2 = minimum [1;2;3] = 1
let test3 = minimum [3;2;1] = 1
let test4 = minimum [3; 2; 6; 4; 1; 8] = 1
