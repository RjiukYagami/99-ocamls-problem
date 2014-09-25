type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let polacz left right all =
    let add_right_tree all l =
       List.fold_left ~f:(fun a r -> Node("x", l, r) :: a) ~init:all right in
    List.fold_left ~f:add_right_tree ~init:all left

let rec hbal_tree n =
   if n = 0 then 
      [Empty]
   else if n = 1 then
      [Node("x",Empty,Empty)]
   else 
      (* (n-1,n-1) *)
      let n1 = hbal_tree (n-1) in
      let n2 = hbal_tree (n-2) in
      (polacz n1 n1 []) @ (polacz n2 n1 []) @ (polacz n1 n2 [])
     
   
