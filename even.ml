(* is_even : int -> bool *)
let is_even n = n mod 2 = 0

(* test *)
let test1 = is_even 0 = true
let test2 = is_even 1 = false
let test3 = is_even 2 = true

(* even : int list -> int list *)
let even lst = List.filter is_even lst

(* test *)
let test4 = even [] = []
let test5 = even [0; 1; 2] = [0; 2]
