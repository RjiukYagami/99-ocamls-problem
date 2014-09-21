type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

let rec wartosc a val_a b val_b expr = 
   match expr with
   | Var x -> 
      if x = a then val_a
      else if x = b then val_b
      else failwith "Nie takie wyrazenie"
   | Or (x,y) ->
      if wartosc a val_a b val_b x = false  && wartosc a val_a b val_b y = false then false
      else true
   | And (x,y) ->
      if wartosc a val_a b val_b x = true && wartosc a val_a b val_b y = true then true
      else false
   | Not x ->
      if wartosc a val_a b val_b x = true then false
      else true

let table2 a b expr =
    [(true,  true,  wartosc a true  b true  expr);
     (true,  false, wartosc a true  b false expr);
     (false, true,  wartosc a false b true  expr);
     (false, false, wartosc a false b false expr) ];;
