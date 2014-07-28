let pack lista = 
let rec listuj ostatni wsio akt = function
   | [] -> wsio
   | x::tl ->
      if List.last ostatni = Some x then 
         listuj ostatni wsio (x::akt) tl
      else
         listuj (ostatni@[x]) (akt::wsio) [x] tl
in
List.drop (List.rev (listuj [] [] [] lista)) 1;;


(* ich rozwiązanie *)
let pack list =
    let rec aux current acc = function
      | [] -> []   
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;

(* nowa moja *)
let pack lista = 
let rec aux current all = function 
| [] -> []
| [x] -> (x :: current)::all
| a :: ( b :: _ as t) -> 
   if a = b then 
      aux (a::current) all t
   else
      aux [] ((a::current)::all) t
in
List.rev (aux [] [] lista);;

