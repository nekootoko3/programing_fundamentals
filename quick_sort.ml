(* sort list asc by quick sort algorithm *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst =
  let take n lst p = List.filter (fun item -> p item n) lst
  in let take_less n lst = take n lst (<)
  in let take_more n lst = take n lst (>=)
  in match lst with
      [] -> []
    | first :: rest -> quick_sort (take_less first rest)
                       @ [first]
                       @ quick_sort (take_more first rest)

(* test *)
let test1 = quick_sort [] = []
let test2 = quick_sort [1] = [1]
let test3 = quick_sort [1; 2] = [1; 2]
let test4 = quick_sort [2; 1] = [1; 2]
let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]
