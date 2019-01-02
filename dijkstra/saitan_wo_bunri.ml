#use "eki_t.ml"

(* eki_t型のリストを受け取って、最短距離最小の駅と最短距離最小の駅以外からなるリストを返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri eki_list =
  List.fold_right (fun first (p, v) ->
                     match (first, p) with
                       ({namae = nf; saitan_kyori = kf; temae_list = lf},
                        {namae = np; saitan_kyori = kp; temae_list = lp}) ->
                        if np = "" then (first, v)
                        else if kf < kp then (first, p :: v)
                        else (p, first :: v))
                  eki_list
                  ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
                                            
(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* 駅リストの例 *) 
let lst = [eki1; eki2; eki3; eki4] 

let test1 = saitan_wo_bunri [] = ({namae = ""; saitan_kyori = infinity; temae_list = []}, [])
let test2 = saitan_wo_bunri lst = (eki3, [eki1; eki2; eki4]) 

(*
let saitan_wo_bunri lst =
  (* 2つの駅を受け取って距離の近い駅を返す *)
  (* kyori_no_chikai_eki : eki_t -> eki_t -> eki_t *)
  let take_near eki1 eki2 = match (eki1, eki2) with
    ({namae = n1; saitan_kyori = k1; temae_list = l1},
     {namae = n2; saitan_kyori = k2; temae_list = l2}) ->
       if k1 < k2 then eki1 else eki2
  (* 最短距離の駅を選択する *)
  (* saitan_kyori_no_eki : eki_t list -> eki_t *)
  in let saitan_kyori_no_eki =
       List.fold_right take_near lst {namae = ""; saitan_kyori = max_float; temae_list = []}
     in match saitan_kyori_no_eki with
       {namae = n0; saitan_kyori = k0; temae_list = l0} ->
         (saitan_kyori_no_eki,
          List.filter (fun item -> match item with
                         {namae = n; saitan_kyori = k; temae_list = l} ->
                           n <> n0) lst)
*)
 
