(*
 * Team BDD 
 * Jakub Dolecki, Sean Murphy, Michael Cioffi
 * Spring, 2011
 * 
 * Binary Decision Diagram interface
 * 
 * Binary decision diagrams are data structures that are used to represent
 * boolean functions. This project aims to implement a module that can be
 * utilized in creating BDDs and manipulating them.
 *
 *)

(* An interface for BDD module *)
module type BDD =
  sig
    type variable (* abstract type for a variable *)
 
    type bdd (* abstract type for bdd *)
  
    type expression (* abstract type for boolean expression *)

    type op (* abstract type for boolean operators *)

    (*
     * make builds a node out of an expression and ensures the bdd is reduced
     * as outlined in ¡°An Introduction to Binary Decision Diagrams¡± 
     * by Andersen.
     *)
    val make: bdd -> bdd

    (* 
     * build takes an expression and returns its binary decision diagram. this
     * function actually builds a bdd from an expression
     *)
    val build: expression -> bdd
    
    (*
     * apply will take a boolean operator and two bdds and return a new bdd 
     * that is the result of the specified operation
     *)
    val apply: op -> bdd -> bdd -> bdd

   (*
    * sat_count will return the number of solutions to the Boolean function
    * given f(x) = 1
    *)
    val sat_count: bdd -> int

    (*
     * any_sat returns any truth assignment
     *)
    val any_sat: bdd -> int list

    (*
     * all_sat returns all satisfying truth assignment
     *)
    val all_sat: bdd -> int list list

    (* TO DO Cool things like print and other extra features 

    val print: bdd -> unit

    val restrict: bdd -> assignment -> bdd

    val random_sat: bdd -> int list
  
    val weight_sat

     *)

  end


