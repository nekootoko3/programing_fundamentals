(* kyosuukai : float -> float -> float -> bool *)

let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c
let kainokosuu a b c =
  if hanbetsushiki a b c >= 0. then true
  else false

(* test *)
let test2 = kainokosuu 1.0 3.0 1.0 = true
let test1 = kainokosuu 1.0 2.0 1.0 = true
let test3 = kainokosuu 2.0 1.0 2.0 = false
