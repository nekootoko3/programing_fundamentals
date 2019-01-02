#use "saitan_wo_bunri.ml"

let koushin p v ekikan_list = match p with
  {namae = np; saitan_kyori = skp; temae_list = tlp} ->
    List.map (fun q -> match q with
                {namae = nq; saitan_kyori = skq; temae_list = tlq} ->
                  let kyori = get_ekikan_kyori np nq ekikan_list
                  in if kyori = infinity then q
                     else if kyori +. skp < skq
                       then {namae = nq; saitan_kyori = kyori +. skp; temae_list = nq :: tlp}
                     else q)
             v

let rec dijkstra_main eki_list ekikan_list = match eki_list with
    [] -> []
  | first :: rest ->
      let (saitan, nokori) = saitan_wo_bunri (first :: rest) in
      let eki_list2 = koushin saitan nokori ekikan_list in

      saitan :: dijkstra_main eki_list2 ekikan_list

(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* 駅リストの例 *) 
let lst = [eki1; eki2; eki3; eki4] 
 
(* テスト *) 
let test1 = dijkstra_main [] global_ekikan_list = [] 
let test2 = dijkstra_main lst global_ekikan_list = 
  [{namae = "茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}; 
   {namae = "新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]}; 
   {namae = "後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]}; 
   {namae = "池袋"; saitan_kyori = 3.; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}] 
