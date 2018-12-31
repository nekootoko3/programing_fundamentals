(* twice : ('a -> 'a) -> 'a -> 'a *)
let twice f =
  let g x = f (f x)
  in g

let add3 x = x + 3

(* test *)
let test1 = twice add3 7 = 13

let times3 x = x * 3

(* test *)
let test2 = twice times3 3 = 27

let str_times3 x = x ^ x ^ x

(* test *)
let test3 = twice str_times3 "a" = "aaaaaaaaa"

(* twice2 : ('_weak2 -> '_weak2) -> '_weak2 -> '_weak2 *)
let twice2 = twice twice

(* test *)
let test4 = twice2 add3 10 = 22
