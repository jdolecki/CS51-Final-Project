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
    type bdd (** Abstract type for binary decision diagrams. *)
  
    type expression (** Abstract type for boolean expression .*)

    type operation (** Abstract type for boolean operators *)

    (**
     Make builds a node out of an expression and ensures the bdd is reduced
     as outlined in 'An Introduction to Binary Decision Diagrams' 
     by Andersen. It returns that node, as well. 
     *)
    val make: int -> bdd -> bdd -> bdd

    (**
     Build takes an expression and recursively constructs its binary decision 
     diagram. It primarily uses Make for constructing each Node. Given 
     expression should have variables given in order 1, 2, 3... if the user
     wants correct sat_count solutions.
     *)
    val build: expression -> bdd
    
    (**
     Apply takes a boolean operator of type operation and two bdds, and 
     returns a new bdd that is the result of the given operation.
     *)
    val apply: operation -> bdd -> bdd -> bdd

   (**
    Sat_count returns the number of solutions to the Boolean function
    given f(x) = 1. 
    *)
    val sat_count: bdd -> float

    (**
     Any_sat returns a list of variable assignments that result in f(x) = 1. 
     The path any_sat takes is greedy, and it may fail in the case of functions
     that aren't well behaved. In those cases, NoSolutionFound exception is 
     thrown and all_sat should be used to find the solution.
     *)
    val any_sat: bdd -> (int * expression) list

    (**
     All_sat returns all satisfying variable assignments that result in f(x)=1 
     where f(x) is the boolean expression for the given bdd. 
     *)
    val all_sat: bdd -> (int * expression) list list

    (** 
     restrict takes a variable assignment and a bdd and returns the new, 
     simplified bdd. Variable assignments should be passed as (var number, 
     True/False). 
     *)
    val restrict: bdd -> (expression * expression) -> bdd


