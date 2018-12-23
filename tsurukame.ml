(* tsurukame *)
(* tsurukame : int -> int -> int *)
let tsurukame gokei ashi = ( gokei * 4 - ashi ) / 2

(* test *)
let test1 = tsurukame 3 6 = 3
let test2 = tsurukame 2 6 = 1
let test3 = tsurukame 3 12 = 0
