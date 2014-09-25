type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec count_leaves tree =
   match tree with
      | Node (j,Empty,Empty) -> 1
      | Node (j,x,y) -> (count_leaves x) + (count_leaves y)
      | Empty -> 0

