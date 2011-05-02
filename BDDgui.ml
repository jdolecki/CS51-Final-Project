open BDD
open Print

(* 
 * Jakub Dolecki, Michael Cioff, Sean Murphy
 * 
 * BDDgui.ml 
 *
 * This is the GUI for our BDD application. It prints an ASCII tree
 * in addition to some parameters about each tree. 
 *)

(* Prints an expression *)
let rec print_exp (e : BDD.expression) = 
  match e with
    | BDD.True -> "TRUE"
    | BDD.False -> "FALSE"
    | BDD.Var x -> "Var " ^ string_of_int x
    | BDD.And(e1, e2) -> "And(" ^ print_exp e1 ^ ", " ^ print_exp e2 ^ ")"
    | BDD.Or(e1, e2) -> "Or(" ^ print_exp e1 ^ ", " ^ print_exp e2 ^ ")"
    | BDD.BImp(e1, e2) -> "BImp(" ^ print_exp e1 ^ ", " ^ print_exp e2 ^ ")"
    | BDD.Imp(e1, e2) -> "Imp(" ^ print_exp e1 ^ ", " ^ print_exp e2 ^ ")"
    | BDD.Neg(e1) -> "Neg(" ^ print_exp e1 ^ ")"

(* Print all satisfying truth assignments *)
let rec print_sat lstlst n = 
  match lstlst with
    | [] -> ()
    | hd :: tail -> 
        let _ = Printf.printf "%s" ("----\nSolution " ^ string_of_int n ^ 
                                      ":\n" )in
        let _ =
          (let rec print_one lst = 
            match lst with
              | [] -> let _ = Printf.printf "%s" "\n" in ()
              | (i, b)::t -> 
                  let _ = Printf.printf "%s" 
                    (string_of_int i ^ " -> " ^ print_exp b ^ "\n") 
                  in print_one t
          in print_one hd)
        in print_sat tail (n + 1);;


(* processes command line input *)
let _ =
  try
    let _ = Printf.printf "Starting BDD!\n" in 
    let lexbuf = Lexing.from_channel stdin in
      while true do
        let result = Parser.main Lexer.token lexbuf in
        let b = BDD.build result in
        let count = BDD.sat_count b in
        let all = BDD.all_sat b in
        (Printf.printf ("%s") ("----\nExpression: " ^ (print_exp result))) 
          ; print_newline();
          Printf.printf ("%s") ("----\n") ; print_newline();
          Print.prettyPrint b ; print_newline();
          Printf.printf ("%s") ("----\nNumber of satisfying solutions: " ^ 
                                 (string_of_float count)) ; print_newline();
          print_sat all 1; print_newline();
          flush stdout
      done
  with 
      Lexer.Eof ->
        let _ = Printf.printf "We reached end of file" in exit 0
    | Failure _ | Parsing.Parse_error ->
        let _ = Printf.printf "You have entered an invalid expression.
    Example usage: (1 or 2 <-> 3 and (!4) -> 3) \n" in exit 0

