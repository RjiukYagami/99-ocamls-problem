let rec at k lista =
match lista with
| [] -> None 
| x :: tl ->
if k = 1 then 
   Some x
else
   at (k-1) tl;;
