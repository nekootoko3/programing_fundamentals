module Tree : sig
  type ('a, 'b) t
  val empty : ('a, 'b) t
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  val search : ('a, 'b) t -> 'a -> 'b
end = struct
  type ('a, 'b) t =
      Empty
    | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t
  let empty = Empty
  let rec insert tree k0 v0 = match tree with
      Empty -> Node (Empty, k0, v0, Empty)
    | Node (left, k, v, right) ->
        if k0 = k then Node (left, k, v0, right)
        else if k0 < k then Node ((insert left k0 v0), k, v, right)
        else Node (left, k, v, (insert right k0 v0))
  let rec search tree k0 = match tree with
      Empty -> raise Not_found
    | Node (left, k, v, right) ->
        if k0 = k then v
        else if k0 < k then search left k0
        else search right k0
end
