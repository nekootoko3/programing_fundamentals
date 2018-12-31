#use "gakusei_t.ml"

(* hyoka: gakusei_t -> gakusei_t *)
let hyoka gakusei = match gakusei with
  {namae = n; tensuu = t; seiseki = s} ->
    {namae   = n;
     tensuu  = t;
     seiseki = if t >= 80 then "A"
               else if t >= 70 then "B"
               else if t >= 60 then "C"
               else "D"}

(* test *)
let test1 = hyoka {namae="neko"; tensuu=80; seiseki=""} = {namae="neko"; tensuu=80; seiseki="A"}
let test2 = hyoka {namae="inu"; tensuu=70; seiseki=""}  = {namae="inu"; tensuu=70; seiseki="B"}
let test3 = hyoka {namae="kiji"; tensuu=60; seiseki=""} = {namae="kiji"; tensuu=60; seiseki="C"}
let test3 = hyoka {namae="saru"; tensuu=59; seiseki=""} = {namae="saru"; tensuu=59; seiseki="D"}
