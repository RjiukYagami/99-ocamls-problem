type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

type 'a pos_binary_tree =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree

let example_layout_tree =
    let leaf x = Node (x,Empty,Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('e', leaf 'd', leaf 'g')),
                   leaf 'm'),
         Node('u', Node('p', Empty, leaf 'q'), Empty))

let together left right father = 
   let get_one all value = 
      List.fold_left ~f:(fun a value2 -> ( (value, value2), father ) :: ( (value2, value), father ) :: a) ~init:[] left in
   List.fold_left ~f:(fun a value -> (get_one a value)@a) ~init:[] right

let rec build_anc_list = function 
   | Empty -> ([],[])
   | Node(j, x, y) ->
      let left = build_anc_list x in
      let right = build_anc_list y in
      let (left_list, left_nodes) = left in
      let (right_list, right_nodes) = right in
      let new_list = together left_nodes right_nodes j in
      ( (new_list @ left_list @ right_list) , (left_nodes @ [j] @ right_nodes) )  
