#use "tree_t.ml"

(* binary_tree_search : tree_t -> int -> bool *)
let rec binary_tree_search tree data = match tree with
    Empty -> false
  | Leaf (n) -> if n = data then true else false
  | Node (left, n, right) ->
      if data = n then true
      else if data < n then binary_tree_search left data
      else binary_tree_search right data
