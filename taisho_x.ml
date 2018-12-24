(* taisho_x : int * int -> int * int *)
let taisho_x zahyo = match zahyo with
  ( x, y ) -> ( -x, -y )

(* test *)
let test1 = taisho_x ( 0, 0 ) = ( 0, 0 )
let test2 = taisho_x ( 1, 1 ) = ( -1, -1 )
let test2 = taisho_x ( -1, -1 ) = ( 1, 1 )
