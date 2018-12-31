#use "eki_t.ml"
(* shokika : eki_t list -> string -> eki_t list *)

let eki_list = [ 
{namae="池袋"; saitan_kyori = infinity; temae_list = []}; 
{namae="新大塚"; saitan_kyori = infinity; temae_list = []}; 
{namae="茗荷谷"; saitan_kyori = infinity; temae_list = []}; 
{namae="後楽園"; saitan_kyori = infinity; temae_list = []}; 
{namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []}; 
{namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []} 
] 

let rec shokika eki_list ekimei = match eki_list with
    [] -> []
  | ({namae = n; saitan_kyori = sk; temae_list = tl} as first) :: rest ->
      if n = ekimei then {namae = n; saitan_kyori = 0.; temae_list = [ekimei]} :: rest
      else first :: shokika rest ekimei

(* テスト *) 
let test1 = shokika [] "茗荷谷" = [] 
let test2 = shokika eki_list "茗荷谷" = [ 
{namae="池袋"; saitan_kyori = infinity; temae_list = []}; 
{namae="新大塚"; saitan_kyori = infinity; temae_list = []}; 
{namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}; 
{namae="後楽園"; saitan_kyori = infinity; temae_list = []}; 
{namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []}; 
{namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []} 
] 
