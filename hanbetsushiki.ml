(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c

(* test *)
let test1 = hanbetsushiki 1.0 2.0 1.0 = 0.0
let test2 = hanbetsushiki 1.0 3.0 1.0 = 5.0
let test3 = hanbetsushiki 2.0 1.0 2.0 = -15.0
