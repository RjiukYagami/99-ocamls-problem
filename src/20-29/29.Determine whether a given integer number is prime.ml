let is_prime num = 
if num = 1 then false
else
   let rec aux act = 
      if act = 1 then 
         true
      else 
      begin
         if num mod act = 0 then
            false
         else
            aux (act-1)
      end
in
aux (num-1);;
