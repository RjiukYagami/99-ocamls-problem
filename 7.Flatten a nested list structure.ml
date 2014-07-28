 type 'a node =
    | One of 'a 
    | Many of 'a node list

let flatten lista =
let rec aux tab = function
| [] -> tab
| One x :: tl -> aux (x::tab) tl
| Many x :: tl -> aux (aux tab x) tl 
in 
List.rev (aux [] lista);;

