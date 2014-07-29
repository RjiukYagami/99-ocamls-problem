let rec insert_at what num = function
| [] -> [what] (* if we are still counting, put "what" *)
| x::tl as t ->
   if num > 0 then
      x::(insert_at what (num-1) tl)
   else
      what::t@[];;
