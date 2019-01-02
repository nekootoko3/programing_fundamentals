(* returns reversed list *)
(* reverse : 'a list -> 'a list *)
let reverse lst =
  (* rev : 'a list -> 'a list -> 'a list *)
  (* reversed_list is a reversed list until now *)
  let rec rev lst reversed_list = match lst with
      [] -> reversed_list
    | first :: rest -> rev rest (first :: reversed_list)
  in rev lst []


(* test *)
let test1 = reverse [] = []
let test2 = reverse [1] = [1]
let test3 = reverse [1; 2; 3] = [3; 2; 1]
let test4 = reverse ["a"; "b"; "c"] = ["c"; "b"; "a"]
