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


open BDDinterface ;;

(* An interface for BDD module *)
module BDD =
  struct
    (* expressions are *)
    type expression =
      | False
      | True
      | Var of int
      | And of expression * expression (* && *)
      | Or of expression * expression (* || *)
      | Imp of expression * expression (* ->, implication *)
      | BImp of expression * expression (* <->, bi-implication *)
      | Neg of expression (* -, negation *)

    type operation = 
      | OpAnd
      | OpOr
      | OpImp
      | OpBimp
    ;;

    (* 
     * BDD is represented as a binary tree with the left branch being the 
     * low branch, or the branch depicting 0 value for the variable, and
     * the right branch being the high branch
     *)
    type bdd = Zero | One | Node of int * bdd * bdd

    (* t is a hash table that holds our bdd *)
    let t = Hashtbl.create 15;;

    (* initialize t to hold Zero and One, the terminal nodes *)
    (*let _ = Hashtbl.add t 0 Zero;;
    let _ = Hashtbl.add t 1 One;;*)

    (* h is a lookup table that will be checked to ensure the bdd constructed
     * is also reduced *)
    let h = Hashtbl.create 15;;
    
    (* increments an index given to each entry in the hash table *)
    let increment = let u = ref 1 in fun () -> incr u; !u;;

    exception TerminalNode;;

    (* *)
    let var (u : int) : int = 
      match Hashtbl.find t u with
        | Node(i, _, _) -> i
        | Zero | One -> raise TerminalNode
    ;;

    let low (u : int) : bdd = 
      match Hashtbl.find t u with
        | Zero -> Zero
        | One -> One
        | Node(_, l, _) -> l
    ;;
            
    let high (u : int) : bdd = 
      match Hashtbl.find t u with
        | Zero -> Zero
        | One -> One
        | Node(_, _, h) -> h
    ;;

    let rec eval (exp: expression) : expression = 
      match exp with
        | True -> True
        | False -> False
        | Var i -> Var i
        | Neg x -> (
            match x with
              | True -> False
              | False -> True
              | _ -> Neg (eval x)
          )
        | And(x,y) -> (
            match (x,y) with
              | (True,True) -> True
              | (True,False) -> False
              | (False, True) -> False
              | (False, False) -> False
              | (_, _) -> And(eval x, eval y)
          )
        | Or(x,y) ->  (
            match (x,y) with 
              | (True,True) -> True
              | (True, False) -> True
              | (False, True) -> True
              | (False, False) -> False
              | (_, _) -> Or(eval x,eval y)
          )
        | Imp(x,y) -> (
            match (x,y) with 
              | (True,True) -> True 
              | (True,False) -> False
              | (False, True) -> True
              | (False, False) -> True
              | (_, _) -> Imp(eval x,eval y)
          )
        | BImp(x,y) -> (
            match (x,y) with
              | (True,True) -> True
              | (True,False) -> False 
              | (False, True) -> False
              | (False, False) -> True
              | (_, _) -> BImp(eval x,eval y)
          )
    ;;
    
    let rec var_bool (v : expression) (e : expression) : bool = 
      match e with
        | True -> false
        | False -> false
        | Var i -> let Var j = v in j == i
        | And(x, y) -> var_bool v x || var_bool v y
        | Or(x, y) -> var_bool v x || var_bool v y
        | BImp(x, y) -> var_bool v x || var_bool v y
        | Imp(x, y) -> var_bool v x || var_bool v y
        | Neg x -> var_bool v x;;

    let rec var_lookup (v : expression) (e : expression) (asgmt : expression) 
        : expression = 
      match e with
        | True -> True
        | False -> False
        | Var i -> let Var j = v in if j == i then asgmt else e
        | And(x, y) -> And (var_lookup v x asgmt, var_lookup v y asgmt)
        | Or(x, y) -> Or (var_lookup v x asgmt, var_lookup v y asgmt)
        | BImp(x, y) -> BImp (var_lookup v x asgmt, var_lookup v y asgmt)
        | Imp(x, y) -> Imp (var_lookup v x asgmt, var_lookup v y asgmt)
        | Neg x -> Neg (var_lookup v x asgmt);;
    
    (* 
     * shannon function will take an expression and return the expression
     * after taking a single variable and changing it to a given assignment
     * Example: shannon (Var x) (False) should return False
     *)
    let rec shannon (exp: expression) (a: expression) : expression =
      match exp with
        | True -> True
        | False -> False 
        | Var i -> a
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

    (* make creates each node
     * First, it checks whether hash table h already holds a node with the
     * attributes i, low, and high. If yes, then nothing is added to t, and 
     * the node is returned. If not, the node is added to h and t. t holds
     * the actual bdd while h is just a lookup table. 
     *)
    let make (i :int) (low : bdd) (high : bdd) : bdd=
      if low == high then low
      else if Hashtbl.mem h (Node(i, low, high))
      then Node(i, low, high)
      else 
        let u = increment () in 
        let _ = Hashtbl.add h (Node(i, low, high)) u in
        let _ = Hashtbl.add t u (Node(i, low, high)) in
            Node(i, low, high)
    ;;
    
    (*
     * build actually creates the bdd after it gets an expression
     * It does so recursively by calling itself and passing shannon either
     * True or False. 
     * Example: build (And (BImp (Var 1, Var 2), BImp (Var 3, Var 4)))
     *            =  Node (1,Node (2, Node (3, Node (4, One, Zero), 
     *                 Node (4, Zero, One)), Zero),
     *                   Node (2, Zero, Node (3, Node (4, One, Zero), 
     *                     Node (4, Zero, One))))
     *) 
    let build (exp : expression) : bdd = 
      let rec build' (e : expression) (i : int) : bdd =
        match e with 
          | True -> 
              if Hashtbl.mem h (One) then One
              else
                let _ = Hashtbl.add h (One) (1) in 
                let _ = Hashtbl.add t 1 (One) in One
          | False -> 
              if Hashtbl.mem h (Zero) then Zero
              else
                let _ = Hashtbl.add h (Zero) (0) in 
                let _ = Hashtbl.add t 0 (Zero) in Zero
          | _ -> 
              if var_bool (Var i) e then
                let low = 
                  build' (eval (var_lookup (Var i) e False)) (i+1) in
                let high = build' (eval (var_lookup (Var i) e True)) (i+1) in
                  make (i) (low) (high)
              else build' (eval e) (i + 1)
      in build' exp 1
    ;;

    (* 
     * sat_count returns a count of satisfying paths leading to a One
     * terminal. 
     *)
    let sat_count (b: bdd) : float = 
      try let u = Hashtbl.find h b in
      let rec count (u : int) : float = 
        if u == 0 then 0.
        else if u == 1 then 1.
        else 
          let low_u = Hashtbl.find h (low u) in
          let high_u = Hashtbl.find h (high u) in
          try let low_var = var low_u in
          let high_var = var high_u in 
            ((2. ** float_of_int(low_var - (var u) - 1)) *. (count(low_u))) +. 
              ((2. ** float_of_int(high_var - (var u) - 1)) *. (count(high_u)))
          with TerminalNode -> 
            ((2. ** float_of_int(((var u)+1)- (var u) - 1)) 
             *. (count(low_u))) +. ((2. ** 
            float_of_int(((var u) + 1) - (var u) - 1)) *. (count(high_u)))
      in (count u) *. (2.**(float_of_int(var u) -. 1.))
      with TerminalNode -> let u = Hashtbl.find h b in 
        if u == 0 then 0. else 1.
    ;;

    exception NoSolutionFound;;

    let any_sat (b : bdd) : (int * expression) list  = 
      let u = Hashtbl.find h b in
      let rec loop (u : int) : (int * expression) list = 
        let low_u = Hashtbl.find h (low u) in
        let high_u = Hashtbl.find h (high u) in
        if u == 0 then raise NoSolutionFound
        else if u == 1 then []
        else
          let var = var u in 
          if low_u == 0 then (var, True)::(loop (high_u))
          else (var, False)::(loop(low_u))
      in loop u
    ;;

    let all_sat (b : bdd) : (int * expression) list list =
      let u = Hashtbl.find h b in
      let rec all (u: int) : (int * expression) list list = 
	let low_u = Hashtbl.find h (low u) in
	let high_u = Hashtbl.find h (high u) in
        if u == 0 then []
	else if u == 1 then [[]]
	else  
          let var = var u in
	  (List.map (fun a-> (var, False)::a) (all(low_u)))@
	  (List.map (fun a-> (var, True)::a) (all(high_u)))
      in all u
    ;;

    exception InvalidInput;;

    let apply (op : operation) (b1 : bdd) (b2 : bdd) : bdd =
      let rec app (b1 : bdd) (b2 : bdd) : bdd =
        let u1 = Hashtbl.find h b1 in
        let u2 = Hashtbl.find h b2 in 
        if (u1 == 0 || u1 == 1) && (u2 == 0 || u2 == 1)
        then let (e1, e2) = 
          match (u1, u2) with
            | (0, 0) -> (False, False)
            | (1, 0) -> (True, False)
            | (0, 1) -> (False, True)
            | (1, 1) -> (True, True) 
        in match op with 
          | OpAnd -> 
              (match eval (And (e1, e2)) with 
                 | True -> One
                 | False -> Zero
                 | _ -> raise InvalidInput)
          | OpOr ->
              (match eval (Or (e1, e2)) with 
                 | True -> One
                 | False -> Zero
                 | _ -> raise InvalidInput)               
          | OpBimp ->
              (match eval (BImp (e1, e2)) with 
                 | True -> One
                 | False -> Zero
                 | _ -> raise InvalidInput)
          | OpImp ->
              (match eval (Imp (e1, e2)) with 
                 | True -> One
                 | False -> Zero
                 | _ -> raise InvalidInput) 
        else if (u1 == 0) || (u1 == 1)
        then make (var u2) (app b1 (low u2)) (app b1 (high u2))
        else if (u2 == 0) || (u2 == 1)
        then make (var u1) (app (low u1) b2) (app (high u1) b2)
        else
          if (var u1) == (var u2) 
          then make (var u1) (app (low u1) (low u2)) (app (high u1) (high u2))
          else if var u1 < var u2
          then make (var u1) (app (low u1) b2) (app (high u1) b2)
          else (* var u1 > var u2 *)
            make (var u2) (app b1 (low u2)) (app b1 (high u2))
      in app b1 b2;;



    let a = And(Var, Var);;
    let a = And(Or(Var, Var), Neg a);;
    let b = build a;;
    let paper_example = And(BImp(Var 1, Var 2), BImp(Var 1, Var 3));;
    let paper = build paper_example;;
    let neg = Neg (Var);;
    let n = build neg;;
    let f = And(False, False);;
    let im = build f;;
    let test_apply = Node(3, Node(4, Node()), Node(4))
end
