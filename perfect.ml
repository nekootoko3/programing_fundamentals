(* returns list from n to 1 *)
(* enumerate : int -> int list *)
let rec enumerate n =
  if n = 0 then []
  else n :: enumerate (n - 1)
(* test *)
let test1 = enumerate 0 = []
let test2 = enumerate 1 = [1]
let test3 = enumerate 3 = [3; 2; 1]

(* returns divisors of n *)
(* divisor : int -> int list *)
let divisor n = List.filter (fun x -> n mod x = 0) (enumerate n)
(* test *)
let test4 = divisor 0 = []
let test5 = divisor 1 = [1]
let test6 = divisor 6 = [6; 3; 2; 1]

(* returns perfect number list under n *)
(* perfect : int -> int list *)
let perfect n = List.filter (fun x -> List.fold_right (+) (divisor x) 0 - x = x) (enumerate n)
(* test *)
let test7 = perfect 10000 = [8128; 496; 28; 6]
