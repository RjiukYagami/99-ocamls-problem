type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec internals tree = 
   match tree with
      | Node(_, Empty, Empty) | Empty -> []
      | Node(j, x, y) ->
         [j] @ (internals x) @ (internals y)

(* TIP: idk why, but with Core.Std it fails, their solution either *)
