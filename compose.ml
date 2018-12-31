(* compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let compose f g =
  let h x = f (g x)
  in h

let add3 x = x + 3
let times2 x = x * 2

let test = (compose times2 add3) 4 = 14
