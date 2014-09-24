type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec adding all add =
   match all with
      | Empty -> Node(add,Empty,Empty)
      | Node (x, l, r) ->
         if x > add then Node(x, (adding l add), r)
         else Node (x, l, (adding r add))
   

let construct lista = 
    List.fold_left ~f:adding ~init:Empty lista;;

