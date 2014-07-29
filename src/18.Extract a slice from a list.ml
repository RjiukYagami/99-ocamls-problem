let slice lista a b =
(* 'c' is index of elemnt which we are considering *)
let rec aux a b c = function
| [] -> []
| x :: tl ->
   if c >= a && c <= b then
      x::(aux a b (c+1) tl)
   else
      aux a b (c+1) tl
in 
aux a b 0 lista;;

(* their solution, lol
let slice list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n-1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n-1) t
    in
    take (k - i + 1) (drop i list);;
it's kinda weird
*)
