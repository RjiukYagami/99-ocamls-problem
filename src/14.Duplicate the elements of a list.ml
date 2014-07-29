let duplicate lista =
let rec aux nowa = function
| [] -> nowa
| x::tl -> aux (x::x::nowa) tl
in
List.rev (aux [] lista);;

