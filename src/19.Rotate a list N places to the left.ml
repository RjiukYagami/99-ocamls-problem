let rotate lista num = 
let rec aux count first second = function
| [] -> 
   if num > 0 then
      List.append (List.rev second) (List.rev first)
   else
      List.append first second
| x::tl -> 
   if count = 0 then 
      aux 0 first (x::second) tl
   else
      aux (count-1) (x::first) second tl
in
if num < 0 then
   aux (-num) [] [] (List.rev lista)
else
   aux num [] [] lista;;
(* main idea is to take (-) rotate and change it so we can use same funtion to both cases like (+) and (-) rotate *)
