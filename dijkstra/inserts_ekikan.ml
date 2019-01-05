#use "insert_ekikan.ml"

(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
(*
let inserts_ekikan ekikan_tree ekikan_list =
  List.fold_right (fun ekikan tree -> insert_ekikan tree ekikan) ekikan_list ekikan_tree
* List.fold_left を使った例
let insers_ekikan ekikan_tree ekikan_list =
  List.fold_left insert_ekikan ekikan_tree ekikan_list
*)
let rec inserts_ekikan ekikan_tree ekikan_list = match ekikan_list with
    [] -> ekikan_tree
  | first :: rest -> inserts_ekikan (insert_ekikan ekikan_tree first) rest

let test1 = inserts_ekikan Empty [ekikan1; ekikan2; ekikan3] = 
  Node (Node (Empty, "後楽園", [("茗荷谷", 1.8)], Empty), 
  "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
        Node (Empty, 
        "池袋", [("新大塚", 1.8)], 
        Node (Empty, 
        "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], 
        Empty))) 
 
