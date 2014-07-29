type 'a rle =
    | One of 'a
    | Many of int * 'a

(* i feel like, I've already done this task, but w/e *)
let encode3 lista =
let make count what = 
   if count = 1 then
      One what
   else
      Many (count,what)
in
let rec aux count all = function 
| [] -> []
| [x] -> ((make (count+1) x)::all)
| a :: (b :: _ as tl) ->
   if a = b then
      aux (count+1) all tl
   else
      aux 0 ((make (count+1) a)::all) tl
in
List.rev (aux 0 [] lista);;

