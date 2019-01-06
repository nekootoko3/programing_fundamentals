let rec fib n =
  if n < 2 then n
  else fib (n - 1) + fib (n - 2)

let test1 = fib 1 = 1
let test2 = fib 2 = 1
let test3 = fib 3 = 2
let test4 = fib 4 = 3
let test5 = fib 5 = 5
let test6 = fib 8 = 21

let rec fib_array arr =
  let len = Array.length arr
  in let rec fib n =
    if n < len
    then (if n = 0 then arr.(n) <- 0
          else if n = 1 then arr.(n) <- 1
          else arr.(n) <- arr.(n - 1) + arr.(n - 2);
          fib (n + 1))
    else ()
  in (fib 0; arr)
      
let test = fib_array [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|] 
     = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|] 
