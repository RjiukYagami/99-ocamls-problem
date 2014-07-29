 type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode2 lista = 
let to_tuple count who = 
   if count = 1 then
      One who
   else
      Many (count,who)
in

let rec aux count all = function
| [] -> []
| [x] -> (to_tuple (count+1) x)::all
| a :: ( b :: _ as t ) ->
   if a = b then
      aux (count+1) all t
   else
      aux 0 ((to_tuple (count+1) a)::all) t 
in 
List.rev (aux 0 [] lista);;

