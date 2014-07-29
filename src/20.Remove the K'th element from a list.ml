(* easy one, nothing to say *)
let rec remove_at num = function
| [] -> []
| x::tl ->
   if num = 0 then
      remove_at (num-1) tl
   else
      x::(remove_at (num-1) tl);;
