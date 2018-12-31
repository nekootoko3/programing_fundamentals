#use "gakusei_list.ml"

(* shukei: gakusei_t list -> int * int * int * int *)
let rec shukei lst = match lst with
  [] -> (0, 0, 0, 0)
  | {namae = n; tensuu = t; seiseki = s} :: rest ->
      let (a, b, c, d) = shukei rest in
      if s = "A" then (a+1, b, c, d)
      else if s = "B" then (a, b+1, c, d)
      else if s = "C" then (a, b, c+1, d)
      else (a, b, c, d+1)

(* test *)
let test1 = shukei lst1 = (1, 0, 0, 0)
let test2 = shukei lst2 = (0, 2, 0, 0)
let test3 = shukei lst3 = (0, 2, 0, 0)
let test4 = shukei lst4 = (2, 2, 2, 0)
