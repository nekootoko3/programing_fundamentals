#use "global_ekimei_list.ml"
#use "global_ekikan_list.ml"
#use "eki_t.ml"
#use "ekikan_tree_t.ml"
#use "tree.ml"

(* romaji_to_kanji : string -> eki_t list -> string *)
let rec romaji_to_kanji r0 lst = match lst with
    [] -> ""
  | {kanji = k; kana = a; romaji = r; shozoku = s} :: rest ->
      if r = r0 then k
      else romaji_to_kanji r0 rest

(* insert1 : ('a, ('b * 'c) list) Tree.t ->
               'a -> 'b -> 'c -> ('a, ('b * 'c) list) Tree.t *)
let insert1 ekikan_tree kiten shuten kyori =
  let lst = try
              Tree.search ekikan_tree kiten
            with Not_found -> []
  in Tree.insert ekikan_tree kiten ((shuten, kyori) :: lst)

(* ekikan_tree_t と ekikan_t を受け取って、ekikan_t の情報を ekikan_tree_t に挿入して返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan ekikan_tree ekikan = match ekikan with
  {kiten = ktn; shuten = stn; keiyu = ky; kyori = kr; jikan = j} ->
    insert1 (insert1 ekikan_tree stn ktn kr) ktn stn kr

(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan ekikan_tree ekikan_list =
  List.fold_left insert_ekikan ekikan_tree ekikan_list

(* 目的：ふたつの駅の間の距離を求める *) 
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *) 
let get_ekikan_kyori eki1 eki2 tree =
  List.assoc eki2 (Tree.search tree eki1)

(* saitan_wo_bunri : eki_t list -> (eki_t * eki_t list) *)
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

(* 目的：ekimei list から eki list を作る *) 
(* make_initial_eki_list : ekimei_t list -> string -> eki_t list *) 
let make_initial_eki_list ekimei_list kiten = 
  List.map (fun ekimei -> match ekimei with 
       {kanji = k; kana = a; romaji = r; shozoku = s} -> 
         if k = kiten 
         then {namae = k; saitan_kyori = 0.; temae_list = [k]} 
         else {namae = k; saitan_kyori = infinity; temae_list = []}) 
     ekimei_list 
 
(* 目的：未確定の駅のリスト v を必要に応じて更新したリストを返す *) 
(* koushin : eki_t -> eki_t list -> ekikan_tree_t -> eki_t list *) 
let koushin p v ekikan_tree = match p with 
  {namae = pn; saitan_kyori = ps; temae_list = pt} -> 
    List.map (fun q -> match q with 
         {namae = qn; saitan_kyori = qs; temae_list = qt} -> 
     let kyori = get_ekikan_kyori pn qn ekikan_tree in 
     if kyori = infinity 
     then q 
     else if ps +. kyori < qs 
     then {namae = qn; saitan_kyori = ps +. kyori; 
           temae_list = qn :: pt} 
     else q) 
       v 
 
(* 目的：未確定駅のリストと駅間リストから、各駅への最短路を求める *) 
(* dijkstra_main : eki_t list -> ekikan_tree_t -> eki_t list *) 
let rec dijkstra_main eki_list ekikan_tree = match eki_list with 
    [] -> [] 
  | first :: rest -> 
      let (saitan, nokori) = saitan_wo_bunri (first :: rest) in 
      let eki_list2 = koushin saitan nokori ekikan_tree in 
      saitan :: dijkstra_main eki_list2 ekikan_tree 
 
(* 目的：受け取った eki_list から shuten のレコードを探し出す *) 
(* find : string -> eki_t list -> eki_t *) 
let rec find shuten eki_list = match eki_list with 
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []} 
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if n = shuten then first else find shuten rest 
 
(* 目的：始点と終点を受け取ったら、最短路を求め、終点のレコードを返す *) 
(* dijkstra : string -> string -> eki_t *) 
let dijkstra romaji_kiten romaji_shuten = 
  let kiten = romaji_to_kanji romaji_kiten global_ekimei_list in 
  let shuten = romaji_to_kanji romaji_shuten global_ekimei_list in 
  let eki_list = make_initial_eki_list global_ekimei_list kiten in 
  let global_ekikan_tree = inserts_ekikan Tree.empty global_ekikan_list in 
  let eki_list2 = dijkstra_main eki_list global_ekikan_tree in 
  find shuten eki_list2 

(* テスト *) 
let test1 = dijkstra "shibuya" "gokokuji" = 
  {namae = "護国寺"; saitan_kyori = 9.8; 
   temae_list = 
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; 
      "青山一丁目"; "表参道"; "渋谷"]} 
let test2 = dijkstra "myogadani" "meguro" = 
  {namae = "目黒"; saitan_kyori = 12.7000000000000028; 
   temae_list = 
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王"; 
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]}
