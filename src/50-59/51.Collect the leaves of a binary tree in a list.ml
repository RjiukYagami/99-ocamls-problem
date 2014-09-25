type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec leaves tree = 
   match tree with
      | Node (j,Empty,Empty) -> [j]
      | Node (j,x,y) -> (leaves x) @ (leaves y)
      | Empty -> []

