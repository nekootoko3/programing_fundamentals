

let rec find yasai0 lst = match lst with
    [] -> None
  | (namae, nedan) :: rest -> match yasai0 with
      (namae0, nedan0) ->
        if namae = namae0 then Some (1)
        else find yasai0 rest

(* yasai_list のうち、yaoya_list にない物の数を返す *)
(* count_urikire_yasai : (string * int) list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_list yaoya_list = match yasai_list with
    [] -> 0
  | first :: rest -> match find first yaoya_list with
      None -> 1 + count_urikire_yasai rest yaoya_list
    | Some (n) -> count_urikire_yasai rest yaoya_list

let yaoya_list = [("tomato", 300); ("tamanegi", 200); ("ninjin", 150); ("sashimi", 500)]
let yasai_list = [("tomato", 300); ("sashimi", 500); ("kintsuba", 1000)]

(* test *)
let test1 = count_urikire_yasai yasai_list yaoya_list = 1
