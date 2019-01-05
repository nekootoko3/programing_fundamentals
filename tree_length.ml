#use "tree_t.ml"

(* returns sum of Leafs and Nodes *)
(* tree_length : tree_t -> int *)
let rec tree_length tree = match tree with
    Empty -> 0
  | Leaf (n) -> 1
  | Node (left, n, right) -> (tree_length left) + 1 + (tree_length right)

let test1 = tree_length tree1 = 0
let test2 = tree_length tree2 = 1
let test3 = tree_length tree3 = 2
let test4 = tree_length tree4 = 4
