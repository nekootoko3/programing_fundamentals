type color_t =
    Red
  | Black

type ('a, 'b) rb_tree_t =
    Empty
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let balance rb_tree = match rb_tree with 
    Node (Node (Node (a, xa, xb, Red, b), ya, yb, Red, c), za, zb, Black, d) 
  | Node (Node (a, xa, xb, Red, Node (b, ya, yb, Red, c)), za, zb, Black, d) 
  | Node (a, xa, xb, Black, Node (Node (b, ya, yb, Red, c), za, zb, Red, d)) 
  | Node (a, xa, xb, Black, Node (b, ya, yb, Red, Node (c, za, zb, Red, d))) 
 -> Node (Node (a, xa, xb, Black, b), ya, yb, Red, Node (c, za, zb, Black, d)) 
  | _ -> rb_tree 

(* test *)
let rb_tree1 = 
  Node (Node (Node (Empty, 10, "x", Red, Empty), 13, "y", Red, Empty), 
  15, "z", Black, Empty) 
let rb_tree2 = 
  Node (Node (Empty, 10, "x", Red, Node (Empty, 13, "y", Red, Empty)), 
  15, "z", Black, Empty) 
let rb_tree3 = 
  Node (Empty, 10, "x", Black, 
  Node (Node (Empty, 13, "y", Red, Empty), 15, "z", Red, Empty)) 
let rb_tree4 = 
  Node (Empty, 10, "x", Black, 
  Node (Empty, 13, "y", Red, Node (Empty, 15, "z", Red, Empty))) 
let rb_tree5 = 
  Node (Node (Empty, 10, "x", Black, Empty), 13, "y", Red, 
  Node (Empty, 15, "z", Black, Empty)) 
let rb_tree6 = Empty 
let test1 = balance rb_tree1 = rb_tree5 
let test2 = balance rb_tree2 = rb_tree5 
let test3 = balance rb_tree3 = rb_tree5 
let test4 = balance rb_tree4 = rb_tree5 
let test5 = balance rb_tree6 = rb_tree6 

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

let rb_tree0 = Empty 
let rb_tree1 = insert rb_tree0 10 "x" 
let rb_tree2 = insert rb_tree1 13 "y" 
let rb_tree3 = insert rb_tree2 15 "z" 
 
let test1 = rb_tree1 = Node (Empty, 10, "x", Black, Empty) 
let test2 = rb_tree2 = Node (Empty, 10, "x", Black, 
         Node (Empty, 13, "y", Red, Empty)) 
let test3 = rb_tree3 = Node (Node (Empty, 10, "x", Black, Empty), 
         13, "y", Black, 
                   Node (Empty, 15, "z", Black, Empty)) 
(* search : ('a * 'b) rb_tree_t -> 'a -> 'b *)
let rec search rb_tree k0 = match rb_tree with
    Empty -> raise Not_found
  | Node (left, k, v, color, right) ->
      if k0 = k then v
      else if k0 < k then search left k0
      else search right k0

(* test *)
let rb_tree = 
  Node (Node (Empty, 10, "x", Black, Empty), 13, "y", Red, 
	Node (Empty, 15, "z", Black, Empty)) 
let test1 = search rb_tree 10 = "x" 
let test2 = search rb_tree 13 = "y" 
let test3 = search rb_tree 15 = "z" 
(* let test4 = search rb_tree 17 *) 
   (* Not_found を起こす *) 



