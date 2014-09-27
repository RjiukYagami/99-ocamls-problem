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

let layout_binary_tree_2 tree = 
   let rec max_level level = function
      | Empty -> (level-1)
      | Node(j, x, y) ->
         max (max_level (level+1) x) (max_level (level+1) y)
   in
   let rec make_new_tree max_level level akt = function
      | Empty -> E
      | Node(j, x, y) ->
         let adding = make_new_tree max_level (level+1) akt x in
         let new_x_layer = (2 lsl (max_level - level -1)) + akt in
	 if adding = E && akt = 0 then
	    N(j, 1 , level, adding, (make_new_tree max_level (level+1) 1 y) )
	 else 
            N(j, new_x_layer , level, adding, (make_new_tree max_level (level+1) new_x_layer y) )
   in
   let akt_max = max_level 1 tree in
   make_new_tree akt_max 1 (-1) tree;;
         
layout_binary_tree_2 example_layout_tree;;
