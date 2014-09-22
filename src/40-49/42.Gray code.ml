let non_gray n = 
   let rec build tab dl = 
      if dl = n  then 
         match List.reduce ~f:(fun x y -> x ^ y) tab with 
         | None -> []
         | Some x -> [String.rev x]
      else (build ("0"::tab) (dl+1)) @ (build ("1"::tab) (dl+1))
   in
   build [] 0
(* it's not acctualy gary function, i'll code it soon *)


(* real grey code function *)
let zmien lista = 
   let odwr = List.rev lista in
   List.concat [(List.map ~f:(fun x -> "0" ^ x) lista); (List.map ~f:(fun y -> "1" ^ y) odwr)]

let grey n =
   let rec szukaj tab = function 
      | 0 -> tab
      | x -> szukaj ( zmien tab ) (x - 1) 
   in szukaj ["0";"1"] (n-1);;
