type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

let rec min_nodes h =
   if h = 1 then 1
   else if h <= 0 then 0
   else (min_nodes (h-1)) + (min_nodes (h-2)) + 1

let max_height n = 
   let rec szukaj akt = 
      if min_nodes akt > n then (akt-1)
      else szukaj (akt+1) 
   in 
   szukaj 1


