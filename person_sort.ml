#use "person_t.ml"

(* person_t 型のリストの例 *) 
let lst1 = [] 
let lst2 = [person1] 
let lst3 = [person1; person2] 
let lst4 = [person2; person1] 
let lst5 = [person3; person1; person2] 

(* person_insert: person_t list -> person_t -> person_t list *)
let rec person_insert lst person = match lst with
  [] -> [person]
  | ({name = n} as first) :: rest ->
    match person with
      {name = n2} ->
        if n2 < n then person :: lst
        else first :: ( person_insert rest person )

(* テスト *) 
let test1 = person_insert [] person1 = [person1] 
let test2 = person_insert [person2] person1 = [person1; person2] 
let test3 = person_insert [person1] person2 = [person1; person2] 
let test4 = person_insert [person1; person2] person3 
	    = [person1; person2; person3] 

let rec person_ins_sort lst = match lst with
  [] -> []
  | first :: rest -> person_insert ( person_ins_sort rest ) first

(* テスト *) 
let test5 = person_ins_sort lst1 = [] 
let test6 = person_ins_sort lst2 = [person1] 
let test7 = person_ins_sort lst3 = [person1; person2] 
let test8 = person_ins_sort lst4 = [person1; person2] 
let test9 = person_ins_sort lst5 = [person1; person2; person3]
