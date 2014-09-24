type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left ~f:(fun a r -> Node("x", l, r) :: a) ~init:all right in
    List.fold_left ~f:add_right_tree ~init:all left (* it's equal to ~f:(fun x y -> add_right_tree x y) *)


let css n = 
   let rec stworz ile = 
      if ile = 0 then 
         [Node("x",Empty,Empty)]
      else if ile = 1 then
            [Node("x",Empty,Node("x",Empty,Empty));Node("x",Node("x",Empty,Empty),Empty)]
      else if ile mod 2 = 0 then
            add_trees_with (stworz (ile/2-1)) (stworz (ile/2-1)) []
      else
            (add_trees_with  (stworz (ile/2-1)) (stworz ((ile-ile/2)-1)) []) @ (add_trees_with  (stworz ((ile-ile/2)-1)) (stworz (ile/2-1)) [])
      in
      stworz (n-1);;

let rec mirror t1 t2 = 
   match t1, t2 with
      | Empty,Empty -> true
      | Node(_,l1,r1), Node(_,l2,r2) ->
         mirror l1 r2 && mirror l2 r1
      | _ -> false

let symetric2 = function
   | Empty -> false
   | Node(_,r,l) -> mirror l r 

(* only thing i had to write down is this :D *)
let sym_cbal_tree n = 
   List.filter ~f:symetric2 (css n);;
