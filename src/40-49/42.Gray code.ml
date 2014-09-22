let gray n = 
   let rec build tab dl = 
      if dl = n  then 
         match List.reduce ~f:(fun x y -> x ^ y) tab with 
         | None -> []
         | Some x -> [String.rev x]
      else (build ("0"::tab) (dl+1)) @ (build ("1"::tab) (dl+1))
   in
   build [] 0
(*it's not acctualy gary function, i'll code it soon *)
