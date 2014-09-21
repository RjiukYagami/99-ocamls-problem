let factors2 num = 
let rec aux dive many num list = (* dive - is number which is diving our number, many means how many times "dive" can dive num, num is out number , and list is ofc list with tuples *)
if num = 1 then
  ((dive,many)::list)(* if we can't dive num anymore, return list *)
else
   begin
   if num mod dive = 0 then (* if it dive, many+1 and num/dive *)
      aux dive (many+1) (num/dive) list
   else
      begin
         if many > 0 then (* if we have anything to add, do it *)
            aux (dive+1) 0 num ((dive,many)::list)
         else
            aux (dive+1) 0 num list
       end
    end
in
List.rev (aux 2 0 num []) ;;
(*only to use it in next problem*)
let rec power x k = 
   if k = 0 then 1
   else x * power x (k-1);;

let phi n = 
   let lista = factors2 n in
      let odp = ref 1 in
         List.iter ~f:(fun (x,y) -> odp := !odp * (x-1)*(power x (y-1)) ) lista;
   	 !odp;;

