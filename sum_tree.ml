#use "tree_t.ml"

(* tree_t を受け取り、全ての値を足し合わせて返す *)
(* sum_tree : tree_t -> int *)
let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (n) -> n
  | Node (left, n, right) -> (sum_tree left) + n + (sum_tree right)

(* test *)
let test1 = sum_tree tree1 = 0
let test2 = sum_tree tree2 = 3
let test3 = sum_tree tree3 = 7
let test4 = sum_tree tree4 = 15
