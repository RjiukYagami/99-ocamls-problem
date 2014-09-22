type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

let rec wartosc list_val expr = 
   match expr with
   | Var x -> 
      let odp = 
      match List.Assoc.find list_val x with
      | None -> failwith "nie ma"
      | Some x -> x
      in odp
   | Or (x,y) ->
      if wartosc list_val x = false  && wartosc list_val y = false then false
      else true
   | And (x,y) ->
      if wartosc list_val x = true && wartosc list_val y = true then true
      else false
   | Not x ->
      if wartosc list_val x = true then false
      else true

let sorti n = 
   let rec build tab dl = 
      if dl = n  then 
         [List.rev tab]
      else (build (true::tab) (dl+1)) @ (build (false::tab) (dl+1))
   in
   build [] 0

let table list_var expr = 
   let lista = sorti ( List.length list_var ) in
      let rec szukaj = function
         | [] -> []
         | hd :: tl -> ( ( List.zip_exn list_var hd ), ( wartosc ( List.zip_exn list_var hd) expr ) ) :: szukaj tl
      in
      szukaj lista;;
   
