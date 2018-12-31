#use "person_t.ml"

(* take_name : person_t -> string *)
let take_name person = match person with
  {name = n; shincho = s; taiju = tj; tsuki = tk; hi = h; ketsueki = k} -> n

(* test *)
let test1 = take_name person1 = "asai" 
let test2 = take_name person2 = "miyahara" 
let test3 = take_name person3 = "nakamura" 

(* person_namae : person_t list -> string list *)
let person_name lst = List.map take_name lst

(* test *)
let lst = [person1; person2; person3] 
let test4 = person_name [] = [] 
let test5 = person_name lst = ["asai"; "miyahara"; "nakamura"] 
