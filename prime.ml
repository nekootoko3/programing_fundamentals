(* sieve : int list -> int list *)
let rec sieve lst = (
  print_string "lst is ";
  print_int (List.length lst);
  print_newline ();
  match lst with
    [] -> []
  | first :: rest -> first :: sieve (List.filter (fun item -> item mod first <> 0) rest))

(* test *)
let test1 = sieve [2] = [2]
let test2 = sieve [2; 3; 4; 5; 6; 7; 8] = [2; 3; 5; 7]

let two_to_n n =
  let rec loop i =
    if i <= n then i :: loop (i + 1) else []
  in loop 2


(* returns prime number list from 2 to n *)
(* prime : n -> int list *)
let prime n = sieve (two_to_n n)
(*
let prime n =
  let rec two_to_n m =
    if m = n then [n]
    else m :: two_to_n (m + 1)
  in sieve (two_to_n 2)
*)

let test3 = prime 2 = [2]
let test4 = prime 8 = [2; 3; 5; 7]
