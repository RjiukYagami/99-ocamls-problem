let rec last lista = 
match lista with
| [] -> None
| [x] -> Some x
| _ :: tl -> last tl
;;

