(* solve natural number's factorial *)
(* fac : int -> int *)
let rec fac n = if n = 0 then 1
                else n * fac (n -1)

