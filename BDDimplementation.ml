(*
 * Team BDD 
 * Jakub Dolecki, Sean Murphy, Michael Cioffi
 * Spring, 2011
 * 
 * Binary Decision Diagram implementation
 * 
 * Binary decision diagrams are data structures that are used to represent
 * boolean functions. This project aims to implement a module that can be
 * utilized in creating BDDs and manipulating them.
 *
 *)

(* An interface for BDD module *)
module BDD =
  struct
    (* variables are represented as indices *)
    type variable = int
    
    (* expressions are *)
    type expression =
      | False
      | True
      | Var of variable
      | And of expression * expression (* && *)
      | Or of expression * expression (* || *)
      | Imp of expression * expression (* ->, implication *)
      | BImp of expression * expression (* <->, bi-implication *)
      | Neg of expression (* -, negation *)

    (* 
     * BDD is represented as a binary tree with the left branch being the 
     * low branch, or the branch depicting 0 value for the variable, and
     * the right branch being the high branch
     *)
    type bdd = Zero | One | Node of variable * bdd * bdd
    
    (*type op = 
      | OpAnd
      | OpOr
      | OpImp
      | OpBImp
      | OpNeg *)

    (* t is a hash table that holds our bdd *)
    let t = Hashtbl.create 15;;

    (* initialize t to hold Zero and One, the terminal nodes *)
    let _ = Hashtbl.add t 0 Zero;;
    let _ = Hashtbl.add t 1 One;;

    (* h is a lookup table that will be checked to ensure the bdd constructed
     * is also reduced *)
    let h = Hashtbl.create 15;;
    let _ = Hashtbl.add h Zero 0;;
    let _ = Hashtbl.add h One 1;;
    
    (* increments an index given to each entry in the hash table *)
    let increment = let u = ref 1 in fun () -> incr u; !u;;

    let rec eval (exp: expression) : expression = 
      match exp with
        | True -> True
        | False -> False
        | Var x -> Var x
        | Neg x -> (
            match x with
              | True -> False
              | False -> True
              | e1 -> Neg x
          )
        | And(x,y) -> (
            match (x,y) with
              | (True,True) -> True
              | (True,False) -> False
              | (False, True) -> False
              | (False, False) -> False
              | (e1,e2) -> And(x,y)
          )
        | Or(x,y) ->  (
            match (x,y) with 
              | (True,True) -> True
              | (True, False) -> True
              | (False, True) -> True
              | (False, False) -> False
              | (e1,e2) -> Or(x,y)
          )
        | Imp(x,y) -> (
            match (x,y) with 
              | (True,True) -> True 
              | (True,False) -> False
              | (False, True) -> True
              | (False, False) -> True
              | (e1,e2) -> Imp(x,y)
          )
        | BImp(x,y) -> (
            match (x,y) with
              | (True,True) -> True
              | (True,False) -> False 
              | (False, True) -> False
              | (False, False) -> True
              | (e1,e2) -> BImp(x,y)
          )
    ;;
    
    (* 
     * shannon function will take an expression and return the expression
     * after taking a single variable and changing it to a given assignment
     * Example: shannon (Var x) (False) should return False
     *)
    let rec shannon (exp: expression) (a: expression) : expression =
      match exp with
    | True -> True
    | False -> False 
    | Var x -> a
    | Neg x -> eval (Neg (shannon x a))
    | And(x,y) -> 
      if (x = True || x = False) then eval (And(eval x, shannon y a))
      else eval (And(shannon x a, y))
    | Or(x,y) ->
      if (x = True || x = False) then eval (Or(eval x, shannon y a))
      else eval (Or(shannon x a, y))
    | Imp(x,y) -> 
      if (x = True || x = False) then eval (Imp(eval x, shannon y a))
      else eval (Imp(shannon x a, y))
    | BImp(x,y) ->
      if (x = True || x = False) then eval (BImp(eval x, shannon y a))
      else eval (BImp(shannon x a, y))
    ;;

    (*let make_var (var : variable) = *)
    let make (i :int) (low : bdd) (high : bdd) : bdd=
      if low == high then low
      else if (Hashtbl.mem h (Node(i, low, high))) 
      then Node(i, low, high)
      else 
        let u = increment () in 
        let _ = Hashtbl.add h (Node(i, low, high)) u in
        let _ = Hashtbl.add t u (Node(i, low, high)) in
          Node(i, low, high)
    ;;

    let build (exp : expression) : bdd = 
      let rec build' (e : expression) (i : int) : bdd =
        match e with 
          | True -> One
          | False -> Zero
          | _ -> 
              let low = build' (shannon e False) (i+1) in
              let high = build' (shannon e True) (i+1) in
                make (i) (low) (high)
      in build' exp 1
    ;;

    let a = And(Var(1), Var(2));;
    let a = And(Or(Var(1), Var(2)), Neg(a));;
    let b = build a;;
    let paper_example = And(BImp(Var(1), Var(2)), BImp(Var(3), Var(4)));;
    let paper = build paper_example;;
    let neg = Neg (Neg (Var 1));;
    let n = build neg;;
  end
