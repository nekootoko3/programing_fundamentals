#use "tree_t.ml"

let rec tree_double tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (n*2)
  | Node (left, n, right) -> Node ((tree_double left), (n*2), (tree_double right))

let tree1_2 = Empty
let tree2_2 = Leaf (6)
let tree3_2 = Node (tree1_2, 8, tree2_2)
let tree4_2 = Node (tree2_2, 10, tree3_2)


(* test *)
let test1 = tree_double tree1 = Empty
let test2 = tree_double tree2 = Leaf (6)
let test3 = tree_double tree3 = tree3_2
let test4 = tree_double tree4 = tree4_2
