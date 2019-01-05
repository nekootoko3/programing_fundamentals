module RedBlack : sig
  type ('a, 'b) rb_tree_t
  val empty : ('a, 'b) rb_tree_t
  val insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t
  val search : ('a, 'b) rb_tree_t -> 'a -> 'b
end = struct
  type color_t =
      Red
    | Black
  type ('a, 'b) rb_tree_t =
      Empty
    | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

  let empty = Empty

  (* balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
  let balance rb_tree = match rb_tree with 
      Node (Node (Node (a, xa, xb, Red, b), ya, yb, Red, c), za, zb, Black, d) 
    | Node (Node (a, xa, xb, Red, Node (b, ya, yb, Red, c)), za, zb, Black, d) 
    | Node (a, xa, xb, Black, Node (Node (b, ya, yb, Red, c), za, zb, Red, d)) 
    | Node (a, xa, xb, Black, Node (b, ya, yb, Red, Node (c, za, zb, Red, d))) 
   -> Node (Node (a, xa, xb, Black, b), ya, yb, Red, Node (c, za, zb, Black, d)) 
    | _ -> rb_tree 

  (* insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
  let insert rb_tree k0 v0 =
    let rec ins rb_tree = match rb_tree with
        Empty -> Node (Empty, k0, v0, Red, Empty)
      | Node (left, k, v, color, right) ->
          if k0 = k then Node (left, k, v, color, right)
          else if k0 < k then balance (Node ((ins left), k, v, color, right))
          else balance (Node (left, k, v, color, (ins right)))
    in match ins rb_tree with
        Empty -> assert false
      | Node (left, k, v, color, right) ->
          Node (left, k, v, Black, right)

  (* search : ('a * 'b) rb_tree_t -> 'a -> 'b *)
  let rec search rb_tree k0 = match rb_tree with
      Empty -> raise Not_found
    | Node (left, k, v, color, right) ->
        if k0 = k then v
        else if k0 < k then search left k0
        else search right k0
end
