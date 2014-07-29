let rand_select lista num = 
(* function to extract something from list *)
let rec extract num = function
| [] -> raise Not_found
| x::tl ->
   if num = 0 then
      x
   else
      extract (num-1) tl
in
(* extract random number *)
let extract_rand list = 
   extract (Random.int (List.length list)) list
in
(* do random extract exactly "count" times *)
let rec aux count list = 
   if count = 0 then
      []
   else
      (extract_rand list)::(aux (count-1) list)
in
aux num lista;;

