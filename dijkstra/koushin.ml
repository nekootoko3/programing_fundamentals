#use "eki_t.ml"
#use "get_ekikan_kyori.ml"

(* let koushin p v = List.map (koushin1 p) v *)
let koushin p v = match p with
  {namae = np; saitan_kyori = skp; temae_list = tlp} ->
    List.map (fun q -> match q with
                {namae = nq; saitan_kyori = skq; temae_list = tlq} ->
                  let kyori = get_ekikan_kyori np nq global_ekikan_list
                  in if kyori = infinity then q
                     else if kyori +. skp < skq
                       then {namae = nq; saitan_kyori = kyori +. skp; temae_list = nq :: tlp}
                     else q) v

(*
let koushin p v =
  let koushin1 p q = match (p, q) with
    ({namae = np; saitan_kyori = skp; temae_list = tlp},
     {namae = nq; saitan_kyori = skq; temae_list = tlq}) ->
       let kyori = get_ekikan_kyori np nq global_ekikan_list
       in if kyori = infinity then q
          else if kyori +. skp < skq
            then {namae = nq; saitan_kyori = kyori +. skp; temae_list = nq :: tlp}
          else q
  in let f q = koushin1 p q
  in List.map f v
*)

(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* 駅リストの例 *) 
let lst = [eki1; eki2; eki3; eki4] 
 
(* テスト *) 
let test1 = koushin eki2 [] = [] 
let test2 = koushin eki2 lst = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 
