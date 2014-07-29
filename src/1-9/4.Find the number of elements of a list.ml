let length lista = 
let rec szukaj n = function
| [] -> n
| x::tl -> szukaj (n+1) tl;
in 
szukaj 0 lista;;

