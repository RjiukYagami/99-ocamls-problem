let permutation list =
let rec extract num first = function
| [] -> raise Not_found
| x::tl ->
   if num = 0 then
      (x,first@tl)
   else
      extract (num-1) (x::first) tl
in 
let rand_ext list = 
   extract (Random.int (List.length list)) [] list
in
let rec aux acc list = 
if List.length list = 0 then
   acc
else
   let got, rest = rand_ext list in
   aux (got::acc) rest in
aux [] list;;

