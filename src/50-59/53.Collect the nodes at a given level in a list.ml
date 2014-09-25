type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec at_level tree n = 
   match tree with
      | Empty -> []
      | Node (j, x, y) -> 
         if n = 1 then [j]
         else (at_level x (n-1)) @ (at_level y (n-1))
;;
