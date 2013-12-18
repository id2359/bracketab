type term  = 
    S | K | 
    Variable of string |
    Application of term * term

type expr = term list;;



let atom = function 
  | S | K | Variable _ -> true
  | _   -> false

let app x y = Application(x,y);;
let var s = Variable(s);;

let sa x = app x x;;

(* some terms *)

let x = Variable("x");;

let y = Variable("y");;

let z = Variable("z");;

let listapp = function
  | [] -> Variable "huh?"
  | h::t -> List.fold_left app h t;;


(* derived combinators *)
let i  = listapp [S;K;K];;
let b = listapp [S;app K S;K];;

let contractS = function
  | [] -> var "?"
  | S :: ( p :: ( q :: t ) -> p;;






