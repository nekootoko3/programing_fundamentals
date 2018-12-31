#use "global_ekikan_list.ml"

(* return ekikan kyori *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> int *)
let rec get_ekikan_kyori eki1 eki2 lst = match lst with
    [] -> infinity
  | {kiten = k; shuten = s; keiyu = ky; kyori = kr;  jikan = j} :: rest ->
      if ( k = eki1 && s = eki2 ) || ( k = eki2 && s = eki1 ) then kr
      else get_ekikan_kyori eki1 eki2 rest

(* test *)
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2 
let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_list = infinity 
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_list = 0.6 
