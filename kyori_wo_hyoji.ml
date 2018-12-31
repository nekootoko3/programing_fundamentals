#use "romaji_to_kanji.ml"
#use "get_ekikan_kyori.ml"

(* print km between eki1 and eki2 *)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji r1 r2 =
    let k1 = romaji_to_kanji r1 global_ekimei_list in
    if k1 = "" then r1 ^ " という駅は存在しません"
    else
      let k2 = romaji_to_kanji r2 global_ekimei_list in
      if k2 = "" then r2 ^ " という駅は存在しません"
      else
        let kyori = get_ekikan_kyori k1 k2 global_ekikan_list in
        if kyori = infinity then k1 ^ "と" ^ k2 ^ "はつながっていません"
        else k1 ^ "から" ^ k2 ^ "までは " ^ string_of_float kyori ^ " キロです"

(* test *)
let test1 = kyori_wo_hyoji "myougadani" "shinotsuka" 
	    = "myougadani という駅は存在しません" 
let test1 = kyori_wo_hyoji "myogadani" "shinotsuka" 
	    = "茗荷谷から新大塚までは 1.2 キロです" 
let test1 = kyori_wo_hyoji "myogadani" "ikebukuro" 
	    = "茗荷谷と池袋はつながっていません" 
let test1 = kyori_wo_hyoji "tokyo" "ootemachi" 
	    = "ootemachi という駅は存在しません" 
let test1 = kyori_wo_hyoji "tokyo" "otemachi" 
	    = "東京から大手町までは 0.6 キロです" 
