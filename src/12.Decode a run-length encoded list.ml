(* elementry type*)
type 'a rle =
|  One of 'a
|  Many of int * 'a

(* decode function which we call *)
let decode lista = 

(* building list from Many *)
let rec build count what acc =
   if count = 0 then
      acc
   else
      build (count - 1) what (what::acc)
in 

(* main rec function to build whole list *)
let rec aux acc = function 
| [] -> acc
| x::tl -> 
   match x with 
   | One y -> aux (y::acc) tl
   | Many (c,y) -> aux ((build c y [])@acc) tl
in 
List.rev (aux [] lista);;
