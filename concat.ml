(* fold_right : (a' -> 'b -> 'b) -> a list -> 'b -> 'b *)
let rec fold_right f lst init = match lst with
    [] -> init
  | first :: rest -> f first (fold_right f rest init)

(*
(* join_str : string -> string -> string *)
let join_str first rest_result = first ^ rest_result
(* concat : string list -> string *)
let concat lst = fold_right (fun first result_rest -> first ^ result_rest) lst ""
*)

let concat lst = fold_right (^) lst ""

(* test *)
let test1 = concat [] = ""
let test2 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

(*
let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest

let add_int first rest_result = first + rest_result
let sum lst = fold_right add_int lst 0

let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

let add_one first rest_result = 1 + rest_result
let length lst = fold_right add_one 0

let rec append lst1 lst2 = match lst1 with
    [] -> [lst2]
  | first :: rest -> first :: append rest lst2

let cons first rest_result = first :: rest_result
let append lst1 lst2 = fold_right cons lst1 lst2
*)
