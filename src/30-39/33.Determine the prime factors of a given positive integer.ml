let factors n = 
let rec aux num act list = 
if is_prime act = true && num mod act = 0 then
   aux (num/act) act (act::list)
else begin
   if act = (n-1) || num = 1 then list
   else aux num (act+1) list
end in
List.rev (aux n 2 []);;
