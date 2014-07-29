let replicate lista num = 

(* we make list of "count" elements equal "x" *)
let rec make count x = 
if count = 0 then 
   []
else
   x :: make (count-1) x in

(* we build whole list from little lists from make function *)
let rec aux = function
| [] -> []
| x :: tl -> (make num x)@(aux tl)
in 
aux lista;;
