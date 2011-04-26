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
    type bdd (* abstract type for bdd *)
  
    type expression (* abstract type for boolean expression *)

    type operation (* abstract type for boolean operators *)

    (*
     * make builds a node out of an expression and ensures the bdd is reduced
     * as outlined in ��An Introduction to Binary Decision Diagrams�� 
     * by Andersen.
     *)
    val make: int -> bdd -> bdd -> bdd

    (* 
     * build takes an expression and returns its binary decision diagram. this
     * function actually builds a bdd from an expression
     *)
    val build: expression -> bdd
    
    (*
     * apply will take a boolean operator and two bdds and return a new bdd 
     * that is the result of the specified operation
     *)
    val apply: operation -> bdd -> bdd -> bdd

   (*
    * sat_count will return the number of solutions to the Boolean function
    * given f(x) = 1
    *)
    val sat_count: bdd -> float

    (*
     * any_sat returns any truth assignment
     *)
    val any_sat: bdd -> int * expression list

    (*
     * all_sat returns all satisfying truth assignment
     *)
    val all_sat: bdd -> (int * expression) list list

    (* 
     * restrict takes a variable assignment and a bdd and returns the 
     * new, simplified bdd
     *)
    val restrict: bdd -> (expression * expression) -> bdd

  end

