type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let symetric tree = 
   let rec way lista right = 
      match lista with 
         | Node(ch, Empty, Empty) -> [(0,ch)]
         | Node(ch, x, Empty) -> 
            [(1,ch)] @ (way x true)
         | Node(ch, Empty, y) ->
            [(-1,ch)] @ (way y false)
         | Node(ch, x, y) ->
            if right = true then
               [(2,ch)] @ (way y true) @ (way x false)
            else
               [(2,ch)] @ (way x false) @ (way y true)
      in 
      match tree with
         | Node (ch , x , y) -> 
            if way x false = way y true then true
            else false
         | _ -> false
;;

(* second way *)
(* so much better way, shame i didn't come up with it myself </3 *)
let rec mirror t1 t2 = 
   match t1, t2 with
      | Empty,Empty -> true
      | Node(_,l1,r1), Node(_,l2,r2) ->
         mirror l1 r2 && mirror l2 r1
      | _ -> false

let symetric2 = function
   | Empty -> false
   | Node(_,r,l) -> mirror l r 
