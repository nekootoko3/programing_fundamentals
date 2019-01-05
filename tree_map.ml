#use "tree_t.ml"

(* map for tree_t *)
(* tree_map : (tree_t list -> tree_t list) -> tree_t list -> tree_t list *)
let rec tree_map f tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (f n)
  | Node (left, n, right) -> Node (tree_map f left, f n, tree_map f right)

let tree1_2 = Empty
let tree2_2 = Leaf (6)
let tree3_2 = Node (tree1_2, 8, tree2_2)
let tree4_2 = Node (tree2_2, 10, tree3_2)

(* test *)
let test1 = tree_map (fun n -> n + 1) tree2_2 = Leaf (7)
let test2 = tree_map (fun n -> n * 2) tree4   = tree4_2
