#use "person_t.ml"

(* first_A : person_t list -> person_t option *)
let rec first_A person_list = match person_list with
    [] -> None
  | ({ketsueki = k} as first) :: rest ->
      if k = "A" then Some (first)
      else first_A rest

(* test *) 
let lst1 = [person1; person2; person3] 
let lst2 = [person3; person2; person1] 
let test1 = first_A [] = None 
let test2 = first_A lst1 = Some (person1) 
let test3 = first_A lst2 = Some (person1) 
