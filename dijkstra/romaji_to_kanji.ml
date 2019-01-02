#use "global_ekimei_list.ml"

(* translate romaji to kanji *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji r0 lst = match lst with
    [] -> ""
  | {kanji = k; kana = a; romaji = r; shozoku = s} :: rest ->
      if r = r0 then k
      else romaji_to_kanji r0 rest

(* test *)
let test1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷" 
let test2 = romaji_to_kanji "shibuya" global_ekimei_list = "渋谷" 
let test3 = romaji_to_kanji "otemachi" global_ekimei_list = "大手町" 
