#use "tree_t.ml"

(* binary_tree_search : tree_t -> int -> bool *)
let rec binary_tree_insert tree data = match tree with
    Empty -> Leaf (data)
  | Leaf (n) ->
      if n = data then Leaf (data)
      else if data < n then Node (Leaf (data), n, Empty)
      else Node (Empty, n, Leaf (data))
  | Node (left, n, right) ->
      if data = n then Node (left, data, right)
      else if data < n then Node (binary_tree_insert left data, n, right)
      else Node (left, n, binary_tree_insert right data)
