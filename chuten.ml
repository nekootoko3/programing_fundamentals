(* chuten : int * int => int * int *)
let chuten points = match points with
  ( ( x1, y1 ), ( x2, y2 ) ) -> ( ( ( x1 + x2 ) / 2 ), ( ( y1 + y2 ) / 2 ) )

(* test *)
let test1 = chuten ( ( 0, 0 ), ( 0, 0 ) ) = ( 0, 0 )
let test2 = chuten ( ( 1, 1 ), ( -1, -1 ) ) = ( 0, 0 )
let test3 = chuten ( ( 3, 3 ), ( 1, 1 ) ) = ( 2, 2 )
