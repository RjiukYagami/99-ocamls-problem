let split lista num = 
let rec aux count first second = function
| [] -> (List.rev first,List.rev second)
| x::tl ->
(* if we already put "num" elements to first list, put rest to second *)
   if count = 0 then
     aux 0 first (x::second) tl
   else
     aux (count - 1) (x::first) second tl
in
aux num [] [] lista;;

