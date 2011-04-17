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
    
    (* 
     * BDD is represented as a binary tree with the left branch being the 
     * low branch, or the branch depicting 0 value for the variable, and
     * the right branch being the high branch
     *)
    type bdd = Zero | One | Node of variable * bdd * bdd

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
    
    type op = 
      | OpAnd
      | OpOr
      | OpImp
      | OpBImp
      | OpNeg

    let a = And(Var(1), Var(2));;

    
    (* Make pseudocode *) 
    (* let make (b:bdd) : bdd = 
         match bdd with 
           | Node (variable, low, high) -> 
               check if node is in hash(implement hastable using Hashtbl module)
                 true -> return that node
                 false -> enter that node into hash table and return the node
           | _ -> RaiseExceptionImpossible? *)

    (* Build pseudocode *)
    (* let build (exp:expression) : bdd =
         match exp with
           | False -> Zero
           | True -> One
           | And -> for each of the cases below, we must have a function that
               will take the expression and return a mini bdd
               e.g Node(idx0, (Node (idx1, Zero ,Zero)), for And
                                (Node (idx1, Zero, One))) except it has to be
                                                            reduced
           | Or
           | Imp
           | BImp
           | Neg
    *)

    let build (exp : expression) : bdd = 
      (* create the hashtable that holds the bdd *)
      let t = Hashtbl.create(15) in
      let _ = Hashtbl.add t Zero 0 in
      let _ = Hashtbl.add t One 1 in Zero
      
    ;;

    (* Apply pseudocode *)
    (* let apply (operation:op) (a:bdd) (b:bdd) : bdd =
         
    *)
  end
