#use "global_ekimei_list.ml"
#use "global_ekikan_list.ml"
#use "eki_t.ml"

let rec romaji_to_kanji r0 lst = match lst with
    [] -> ""
  | {kanji = k; kana = a; romaji = r; shozoku = s} :: rest ->
      if r = r0 then k
      else romaji_to_kanji r0 rest

let rec get_ekikan_kyori eki1 eki2 lst = match lst with
    [] -> infinity
  | {kiten = k; shuten = s; keiyu = ky; kyori = kr;  jikan = j} :: rest ->
      if ( k = eki1 && s = eki2 ) || ( k = eki2 && s = eki1 ) then kr
      else get_ekikan_kyori eki1 eki2 rest
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

let make_initial_eki_list ekimei_list kiten =
  List.map (fun ekimei -> match ekimei with
             {kanji = k; kana = a; romaji = r; shozoku = s} ->
               if k = kiten then {namae = k; saitan_kyori = 0.; temae_list = [kiten]}
               else {namae = k; saitan_kyori = infinity; temae_list = []})
           ekimei_list

let rec find shuten eki_list = match eki_list with 
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []} 
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if n = shuten then first else find shuten rest 

let dijkstra shiten_r shuten_r =
  let (shiten, shuten) = (romaji_to_kanji shiten_r global_ekimei_list,
                          romaji_to_kanji shuten_r global_ekimei_list) in
  let eki_list = make_initial_eki_list global_ekimei_list shiten in
  let saitan_eki_list = dijkstra_main eki_list global_ekikan_list in
  find shuten saitan_eki_list

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
