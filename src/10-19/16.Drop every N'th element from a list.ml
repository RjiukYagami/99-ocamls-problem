(* nothing to comment, it's easy one :) *)
let drop lista num = 
let rec aux count = function 
| [] -> [] 
| x::tl ->
   if count mod num = 0 then 
      aux (count+1) tl
   else
      x::(aux (count+1) tl)
in 
aux 1 lista;;
