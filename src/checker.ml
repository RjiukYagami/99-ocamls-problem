type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

(*let polacz a b = 
   let rec tworz = function 
      | ([],_) -> []
      | (hd::tl,[]) -> [] @ tworz (tl,b)
      | ((x::y) as t,hd::tl) -> Node("x",x,hd) :: tworz (t,tl)
   in 
   tworz (a,b)*)
   
let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left ~f:(fun a r -> Node("x", l, r) :: a) ~init:all right in
    List.fold_left ~f:add_right_tree ~init:all left


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

print_int (List.length (css 40))
