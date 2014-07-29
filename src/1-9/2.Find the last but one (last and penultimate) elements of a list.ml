let rec last_two lista = 
match lista with 
| [] | [_] -> None
| [a;b]-> Some (a,b)
| _ :: tl -> last_two tl;;

