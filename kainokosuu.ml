(* kainokosuu : float -> float -> float -> int *)

let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c
let kainokosuu a b c =
  if hanbetsushiki a b c > 0. then 2
  else if hanbetsushiki a b c = 0. then 1
  else 0

(* test *)
let test2 = kainokosuu 1.0 3.0 1.0 = 2
let test1 = kainokosuu 1.0 2.0 1.0 = 1
let test3 = kainokosuu 2.0 1.0 2.0 = 0
