#use "gakusei_list.ml"

(* count : gakusei_t list -> string -> int *)
let count lst seiseki0 =
  let is_seiseki gakusei = match gakusei with
    {namae = n; tensuu = t; seiseki = s} -> s = seiseki0
  in List.length ( List.filter is_seiseki lst )

(* test *)
let test1 = count lst1 "A" = 1
let test2 = count lst1 "B" = 0
let test3 = count lst2 "A" = 0
