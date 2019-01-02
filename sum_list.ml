(* int list を受け取り、それまでの数の合計を返す *)
(* sum_list : int list -> int list *)
let sum_list lst =
  (* total はこれまでの整数の合計 *)
  (* hojo : int list -> int -> int list *)
  let rec hojo lst total = match lst with
      [] -> []
    | first :: rest -> first + total :: hojo rest (first + total)
  in hojo lst 0

(* test *)
let test1 = sum_list [] = []
let test2 = sum_list [5] = [5]
let test3 = sum_list [3; 4; 7; 1] = [3; 7; 14; 15]
