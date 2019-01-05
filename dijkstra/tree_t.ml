type 'a tree_t =
    Empty                               (* Empty Tree *)
  | Leaf of 'a                          (* Leaf *)
  | Node of 'a tree_t * int * 'a tree_t (* Node *)

let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (tree1, 4, tree2)
let tree4 = Node (tree2, 5, tree3)
