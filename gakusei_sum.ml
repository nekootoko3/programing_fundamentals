#use "gakusei_list.ml"

(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst =
  let add_tensuu first result_rest = match first with
    {namae = n; tensuu = t; seiseki = s} -> t + result_rest
  in List.fold_right add_tensuu lst 0

(* test *)
let test1 = gakusei_sum [] = 0
let test2 = gakusei_sum lst1 = 80
let test3 = gakusei_sum lst3 = 145
