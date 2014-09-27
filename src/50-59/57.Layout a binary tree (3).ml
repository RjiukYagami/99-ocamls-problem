(* the main idea is that we're build tree without looking at position like left son number is father's - 1 and right father's + 1 
   then we check if there are any colision, if there is, we're taking their lowest common ancestor and \stretch\ it's subtree *)


(* the way we represent typical tree *)
type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

(* it's position tree representation *)
type 'a pos_binary_tree =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree

(* our tree from example *)
let example_layout_tree =
    let leaf x = Node (x,Empty,Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('e', leaf 'd', leaf 'g')),
                   leaf 'm'),
         Node('u', Node('p', Empty, leaf 'q'), Empty))

(* it's function to link every item form 'left' to every element form 'right' the way it is in 'get_one' *)
let together left right father = 
   let get_one all value = 
      List.fold_left ~f:(fun a value2 -> ( (value, value2), father ) :: ( (value2, value), father ) :: a) ~init:all left in
   List.fold_left ~f:get_one ~init:[] right

(* we build full list with ancestors like (node1, node2) , lca) *)
let rec build_anc_list = function 
   | Empty -> ([],[])
   | Node(j, x, y) ->
      let left = build_anc_list x in
      let right = build_anc_list y in
      let (left_list, left_nodes) = left in
      let (right_list, right_nodes) = right in
      let new_list = together left_nodes right_nodes j in
      ( (new_list @ left_list @ right_list) , (left_nodes @ [j] @ right_nodes) )  

let (list_of_anc, list_of_nodes) = build_anc_list example_layout_tree

(* a function to take dates from list_of_anc, where a,b are nodes and answer is their lca *)
let find_anc a b = 
   List.Assoc.find_exn list_of_anc (a,b)

(* function to check if there are any duplicates in list *)
let rec check_duplicate = function
   | [] | [_] -> None
   | hd1::(hd2::tl as t) ->
       let (a1,b1) = hd1 in
       let (a2,b2) = hd2 in
       if a1 = a2 then Some ((b1,b2))
       else check_duplicate t

(* we build our first tree, where we dont care about colision *)
let rec build_tree_as_list position level = function
   | Empty -> [((0,0),'z')]
   | Node(j, x ,y) ->
      if position = 0 then
	 let rest = build_tree_as_list position (level+1) x in
         let ((akt,_),_) = List.hd_exn rest in
         let right = build_tree_as_list (akt+1) (level+1) y in
         [((akt+1,level),j)] @ rest @ right
      else 
         let rest = build_tree_as_list (position-1) (level+1) x in
         let right = build_tree_as_list (position+1) (level+1) y in
         [((position+1,level),j)] @ rest @ right

(* we out filter Empty nodes *)      
let tree_in_list = List.filter ~f:(fun ((x,y),j) -> if x <> 0 && y <> 0 then true else false) (build_tree_as_list 0 1 example_layout_tree)

(* functon to compare to nodes where they are shown as ((position x, position y),node id) *)
let compare_list a b =
   let ((x1,y1),_) = a in
   let ((x2,y2),_) = b in
   if x1 < x2 then 1 
   else if x1 = x2 && y1 < y2 then 1
   else if x1 = x2 && y1 > y2 then -1
   else if x1 = x2 && y1 = y2 then 0
   else -1

let tree_in_list_sorted = List.sort ~cmp:compare_list tree_in_list

(* function to release date form Option type *)
let get = function
   | Some x -> x

(* function to check if there is need to change tree or it's good already *)
let must_change lista = 
   let akt = List.find_a_dup ~compare:compare_list lista in
   if akt <> None then
      let ((x1,y1),j1) = get(akt) in
      let j2 = get(List.Assoc.find (List.rev lista) (x1,y1)) in
      Some (j1,j2)
   else
      None

let get_anc who = 
   List.Assoc.find list_of_anc who 

(* it builds list of nodes that are in subtrees of x *)
let rec build_list_of_node left son x = function
   | Empty -> [('0',0)]
   | Node(j, x1, y1) ->
      if son = true && left = true then 
         [(j,(-1))] @ (build_list_of_node true true x x1) @ (build_list_of_node true true x y1)
      else if son = true && left = false then 
         [(j,1)] @ (build_list_of_node false true x x1) @ (build_list_of_node false true x y1)
      else if x = j then
         (build_list_of_node true true x x1) @ (build_list_of_node false true x y1)
      else
         (build_list_of_node false false x x1) @ (build_list_of_node false false x y1)

(* if we must change lca node we do it with this function *)
let rec change nodes lista = 
   match lista with
   | [] -> []
   | ((x,y),j)::tl ->
      let dod = List.Assoc.find nodes j in
      if dod <> None then  
         ((x+get(dod),y),j)::(change nodes tl)
      else 
         ((x,y),j)::(change nodes tl)
(* we change tree into list *)
let rec change_tree_in_list lista =
   let akt1 = must_change lista in
   if akt1 <> None then
      let father = get(get_anc(get(akt1))) in
      change_tree_in_list (change (build_list_of_node false false father example_layout_tree) lista)
   else 
      lista;;

let rec rev_tree = function
   | [] -> []
   | ((x,y),j)::tl ->
      (j,(x,y))::(rev_tree tl)

let done_tree = change_tree_in_list tree_in_list_sorted
let rev_done_tree = rev_tree done_tree

let smallest lista = 
   let add_to_all = ref 0 in
   let rec szukaj = function
      | ((x,y),j)::tl ->
         if (1 - x) > !add_to_all then
            add_to_all := 1 - x;
         szukaj tl
      | [] -> !add_to_all
    in szukaj lista

let add_to_all = smallest done_tree

(* we build new tree from out dates we made, becuase it's done in list not in variant representation *)
let rec build_answer = function
   | Empty -> E
   | Node(j, x, y) ->
      let (val1, val2) = List.Assoc.find_exn rev_done_tree j in
      N(j, (val1+add_to_all), val2 , (build_answer x), (build_answer y));;

build_answer example_layout_tree;;
(* biggest program i've ever written in OCaml gg *)
