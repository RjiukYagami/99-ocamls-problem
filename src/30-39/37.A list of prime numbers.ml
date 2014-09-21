(* is really dumb way to check if it's prime or even gain list of primes number form x to y, but it's the way they wanted me to write it, i supose *) 
let is_prime n = 
   let rec check s = 
      if n mod s = 0 && n>2 then 
         false
      else if s = 2 || n = 2then true
      else check (s-1)
   in
   check (n-1);;

let rec all_primes x y = 
   if x = y then []
   else if is_prime x = true then 
      x :: all_primes (x+1) y
   else
     all_primes (x+1) y;; 
