(* returns max divisor of m and n ( m >= n >= 0 ) *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  if n = 0
    then m
  else
    gcd n (m mod n)

(* test *)
let test1 = gcd 0 0  = 0
let test2 = gcd 10 0 = 10
let test3 = gcd 5 5  = 5
let test4 = gcd 10 5 = 5
let test5 = gcd 12 5 = 1
let test6 = gcd 12 8 = 4

(* 停止性 :
  1. 再帰呼び出しを行う際の部分問題が、元の問題よりも簡単になっている
    -> 部分問題では gcd n (m mod n) となっており、m >= n >= 0 のとき 元の問題 gcd m n と比較して 第一引数、第二引数ともに値が小さくなっている。( m >= n >= 0, n > (m mod n) >= 0)
  2. 再帰の度にnはちいさくなっているので、いずれ0になり止まる
*)
