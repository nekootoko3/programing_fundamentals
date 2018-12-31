type person_t = { 
  name : string;  (* 名前 *) 
  shincho : float;  (* 身長 *) 
  taiju : float;  (* 体重 *) 
  tsuki : int;    (* 誕生月 *) 
  hi : int;   (* 誕生日 *) 
  ketsueki : string;  (* 血液型 *) 
}

 
let person1 = 
  {name = "asai"; 
   shincho = 1.72; 
   taiju = 58.5; 
   tsuki = 9; 
   hi = 17; 
   ketsueki = "A" 
}
 
let person2 = { 
  name = "miyahara"; 
  shincho = 1.63; 
  taiju = 55.0; 
  tsuki = 6; 
  hi = 30; 
  ketsueki = "B" 
}
 
let person3 = { 
  name = "nakamura"; 
  shincho = 1.68; 
  taiju = 63.0; 
  tsuki = 6; 
  hi = 6; 
  ketsueki = "O" 
}
