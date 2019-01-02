(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
(* init がアキュムレータとしての役割を担っている *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest -> fold_left f (f init first) rest
(*
let fold_left f init lst = match lst with
    [] -> init
  | first :: rest ->
    let rec hojo f lst acm = match lst with
        [] -> acm
      | first :: rest -> hojo f rest (f acm first)
    in hojo f rest (f init first)
*)

(* test *)
let test1 = fold_left (-) 0 [] = 0 
let test2 = fold_left (-) 10 [4; 1; 3] = 2 
let test3 = fold_left (fun lst a -> a :: lst) [] [1; 2; 3; 4] = [4; 3; 2; 1] 
