(* merge ascend sorted list lst1 and lst2 *)
(* merge : 'a list -> 'a list -> 'a list *)
let rec merge lst1 lst2 = match (lst1, lst2) with
  ([], []) -> []
  | ([], first2 :: rest2) -> lst2
  | (first1 :: rest1, []) -> lst1
  | (first1 :: rest1, first2 :: rest2) ->
      if first1 < first2
      then first1 :: merge rest1 lst2
      else first2 :: merge lst1 rest2

      (* test *)
let test1 = merge [] [] = []
let test2 = merge [1; 3] [] = [1; 3]
let test3 = merge [] [2; 4] = [2; 4]
let test4 = merge [1; 3] [2; 4] = [1; 2; 3; 4]
let test4 = merge [2; 4] [1; 3] = [1; 2; 3; 4]
let test5 = merge [1; 3] [1; 3] = [1; 1; 3; 3]
