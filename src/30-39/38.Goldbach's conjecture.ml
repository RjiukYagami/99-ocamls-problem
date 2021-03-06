let is_prime n = 
   let rec check s = 
      if n mod s = 0 && n>2 then 
         false
      else if s = 2 || n = 2then true
      else check (s-1)
   in
   check (n-1);;

let goldbach n = 
   let rec check s = 
      if is_prime s = true && is_prime (n-s) = true then
         (s,n-s)
      else 
         check (s+1)
   in
   check 2;;
