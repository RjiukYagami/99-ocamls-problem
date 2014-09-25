type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

type 'a pos_binary_tree =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree

let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('h', Node('g', leaf 'e',Empty), Empty)),
                   leaf 'm'),
         Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty))

let layout_binary_tree_1 tree =
   let all = [] in 
   let rec build_nr tree1 nr = 
      match tree1 with 
         | Empty -> 0
         | Node(j, x, y) -> 
            let akt = build_nr x nr in
            List.append all [(j,x)];
            build_nr y (nr+akt+1);
             j
   in 
   let rec build_all tree1 level=
     match tree1 with
        | Empty -> E
        | Node(j, x, y) ->
           N(j,(List.assoc j all),level, (build_all x (level+1)), (build_all y (level+1)))
   in 
   build_nr tree 1;
   build_all tree 1;;
       
         
        
         
