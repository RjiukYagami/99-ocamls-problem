let range a b = 
let rec aux ft sc = 
   if sc - ft >= 0 then
      ft::(aux (ft+1) sc)
   else
      []
in if a > b then 
   List.rev (aux b a)
else
   aux a b;;
