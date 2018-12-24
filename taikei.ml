(* bmi : float -> float -> string *)
let bmi shincho taiju =  taiju /. (shincho *. shincho) 
let taikei shincho taiju = 
  if bmi shincho taiju < 18.5 then "yase"
  else if bmi shincho taiju < 25. then "normal"
  else "debu"

(* test *)
let test1 = taikei 1.7 50. = "yase"
let test2 = taikei 1.7 70. = "normal"
let test3 = taikei 1.7 190. = "debu"
