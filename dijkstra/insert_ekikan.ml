#use "global_ekikan_list.ml"
#use "ekikan_tree_t.ml"

let rec insert1 ekikan_tree kiten shuten kyori = match ekikan_tree with
    Empty -> Node (Empty, kiten, [(shuten, kyori)], Empty)
  | Node (left, ekimei, lst, right) ->
      if kiten = ekimei then Node (left, ekimei, (shuten, kyori) :: lst, right)
      else if kiten < ekimei
        then Node (insert1 left kiten shuten kyori, ekimei, lst, right)
      else Node (left, ekimei, lst, insert1 right kiten shuten kyori)

(* ekikan_tree_t と ekikan_t を受け取って、ekikan_t の情報を ekikan_tree_t に挿入して返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan ekikan_tree ekikan = match ekikan with
  {kiten = ktn; shuten = stn; keiyu = ky; kyori = kr; jikan = j} ->
    insert1 (insert1 ekikan_tree stn ktn kr) ktn stn kr


(* 駅間の例 *) 
let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 
 
(* テスト *) 
let tree1 = insert_ekikan Empty ekikan1 
let test1 = tree1 = 
  Node (Empty, "新大塚", [("池袋", 1.8)], 
  Node (Empty, "池袋", [("新大塚", 1.8)], Empty)) 
let tree2 = insert_ekikan tree1 ekikan2 
let test2 = tree2 = 
  Node (Empty, "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
  Node (Empty, "池袋", [("新大塚", 1.8)], 
    Node (Empty, "茗荷谷", [("新大塚", 1.2)], Empty))) 
let tree3 = insert_ekikan tree2 ekikan3 
let test3 = tree3 = 
  Node (Node (Empty, "後楽園", [("茗荷谷", 1.8)], Empty), 
    "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
      Node (Empty, "池袋", [("新大塚", 1.8)], Node (Empty, "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], Empty))) 
