let phi n = 
let rec aux count acc = 
if count = 0 then 
 acc
else begin
   if coprime n count = true then 
      aux (count-1) (acc+1)
   else
      aux (count-1) acc
end in
aux (n-1) 0;;

