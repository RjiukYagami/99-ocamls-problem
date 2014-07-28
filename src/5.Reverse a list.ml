let rec rev lista = 
let rec odw nowa = function
| [] -> nowa 
| x::tl -> odw (x::nowa) tl
in 
odw [] lista;;
