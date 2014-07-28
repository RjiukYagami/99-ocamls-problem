let encode lista = 
let rec aux current all = function 
| [] -> []
| [x] -> (current+1,x)::all
| a :: (b :: _ as t) -> 
   if a = b then 
      aux (current + 1) all t
   else
      aux 0 ((current + 1,a)::all) t
in
List.rev (aux 0 [] lista);;

(*ich*)
let encode list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count+1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count+1,a) :: acc) t in
    List.rev (aux 0 [] list);;
