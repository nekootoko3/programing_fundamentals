#use "gakusei_list.ml"

(* is_A : gakusei_t -> bool *)
let is_A gakusei = match gakusei with
  {namae = n; tensuu = t; seiseki = s} -> s = "A"

(* test *)
let test1 = is_A gakusei1 = true
let test2 = is_A gakusei3 = false

(* count_A : gakusei_t list -> length *)
let count_A lst = List.length ( List.filter is_A lst )

(* test *)
let test3 = count_A lst1 = 1
let test4 = count_A lst2 = 0
let test5 = count_A lst4 = 2
