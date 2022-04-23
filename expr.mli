exception CompileError of string ;;

type unop =
| Negate
| NegateDot
| Not
;;
  
type binop =
| Plus
| Minus
| Times
| Divide
| Equals
| NotEqual
| LessThan
| GreaterThan
| LessEqual
| GreaterEqual
| PlusDot
| MinusDot
| TimesDot
| DivideDot
| Exponent
| And
| Or
| Concat
| Cons
| Seq
| ListConcat
;;

type varid = string ;;

type literaltype = 
| Num
| Bool
| Float
| String
| Unit
| EmptyList
| List of literaltype ;;

type literal =
| Num of int
| Bool of bool
| Float of float
| String of string
| Unit
| List of literaltype * literal list ;;

val get_tag : literal -> literaltype ;;

type pattern =
| Wildcard
| Var of varid
| Literal of literal
| List of pattern list
| Cons of pattern * pattern
| Options of pattern * pattern ;;

type expr =
| Var of varid                         (* variables *)
| Literal of literal
| Unop of unop * expr                  (* unary operators *)
| Binop of binop * expr * expr         (* binary operators *)
| Conditional of expr * expr * expr    (* if then else *)
| Match of expr * (pattern * expr) list
| Fun of varid * expr                  (* function definitions *)
| Let of varid * expr * expr           (* local naming *)
| Letrec of varid * expr * expr        (* recursive local naming *)
| Raise                                (* exceptions *)
| Unassigned                           (* (temporarily) unassigned *)
| App of expr * expr                   (* function applications *)
;;

(* How to match literals: make a new variable then check if it's equal. *)
    
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
type varidset ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
val same_vars : varidset -> varidset -> bool ;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
val vars_of_list : varid list -> varidset ;;

(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
val free_vars : expr -> varidset ;;

(* new_varname () -- Returns a freshly minted `varid` *)
val new_varname : unit -> varid ;;

(*......................................................................
  Substitution 

 *)
                            
(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
val subst : varid -> expr -> expr -> expr ;;
     
(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
val exp_to_concrete_string : expr -> string ;;

val print_pattern : pattern -> string ;;

val pattern_bindings : pattern -> varidset ;; 

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
val exp_to_abstract_string : expr -> string ;;

val abs : bool ref;;

