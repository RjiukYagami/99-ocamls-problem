let compress lista = 
let rec aux nowa = function
| [] -> nowa
| x::tl ->
   if List.last nowa = Some x then
      aux nowa tl
   else
      aux (List.append nowa [x]) tl
in
aux [] lista;;
