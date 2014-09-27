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
      List.fold_left ~f:(fun a value2 -> ( (value, value2), father ) :: ( (value2, value), father ) :: a) ~init:all left in
   List.fold_left ~f:get_one ~init:[] right

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

let find_anc a b = 
   List.Assoc.find_exn list_of_anc (a,b)

let rec check_duplicate = function
   | [] | [_] -> None
   | hd1::(hd2::tl as t) ->
       let (a1,b1) = hd1 in
       let (a2,b2) = hd2 in
       if a1 = a2 then Some ((b1,b2))
       else check_duplicate t

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
      
let tree_in_list = List.filter ~f:(fun ((x,y),j) -> if x <> 0 && y <> 0 then true else false) (build_tree_as_list 0 1 example_layout_tree)

let compare_list a b =
   let ((x1,y1),_) = a in
   let ((x2,y2),_) = b in
   if x1 < x2 then 1 
   else if x1 = x2 && y1 < y2 then 1
   else if x1 = x2 && y1 > y2 then -1
   else if x1 = x2 && y1 = y2 then 0
   else -1

let tree_in_list_sorted = List.sort ~cmp:compare_list tree_in_list

let get = function
   | Some x -> x

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

let rec change nodes lista = 
   match lista with
   | [] -> []
   | ((x,y),j)::tl ->
      let dod = List.Assoc.find nodes j in
      if dod <> None then  
         ((x+get(dod),y),j)::(change nodes tl)
      else 
         ((x,y),j)::(change nodes tl)
   
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

let rec build_answer = function
   | Empty -> E
   | Node(j, x, y) ->
      let (val1, val2) = List.Assoc.find_exn rev_done_tree j in
      N(j, (val1+add_to_all), val2 , (build_answer x), (build_answer y));;

build_answer example_layout_tree;;
(* biggest program i've ever written in OCaml gg *)
