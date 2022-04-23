(*......................................................................
  Abstract syntax of MiniML expressions 
 *)
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

let get_tag : literal -> literaltype = function
  | Num _ -> Num
  | Bool _ -> Bool
  | Float _ -> Float
  | String _ -> String
  | Unit -> Unit 
  | List (ltype, _) -> List ltype ;;

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
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;

(*......................................................................
  String representations of expressions
 *)
 let rec print_list printf = function
 | [] -> "[]"
 | [h] -> "[" ^ printf h ^ "]"
 | h :: t -> let tail = print_list printf t in
             "[" ^ printf h ^ ";" ^ (String.sub tail 1 @@ String.length tail - 2) ^ "]" ;;

let rec print_literal = function
  | Num x -> string_of_int x
  | Bool x -> string_of_bool x
  | Float x -> string_of_float x
  | String x -> "\"" ^ x ^ "\""
  | Unit -> "()"
  | List (_, l) -> print_list print_literal l ;;

let rec print_pattern (p : pattern) = "(" ^ (match p with
 | Wildcard -> "_"
 | Var x -> x
 | Literal l -> print_literal l
 | List l -> print_list print_pattern l
 | Cons (p1, p2) -> print_pattern p1 ^ " :: " ^ print_pattern p2
 | Options (p1, p2) -> print_pattern p1 ^ " | " ^ print_pattern p2) ^ ")" ;;

let rec pattern_bindings (p : pattern) = 
  let union s1 p2 =
    let s2 = pattern_bindings p2 in
    if SS.disjoint s1 s2 then SS.union s1 s2
    else raise @@ CompileError (print_pattern p ^ " variable bound more than once in pattern") in
  match p with
  | Wildcard | Literal _ -> SS.empty
  | Var x -> SS.singleton x
  | List plist -> List.fold_left union SS.empty plist
  | Cons (h, t) -> t |> union @@ pattern_bindings h
  | Options (o1, o2) ->
      let bindings = pattern_bindings o1 in
      if bindings = pattern_bindings o2 then bindings 
      else raise @@ CompileError (print_pattern p ^ " bindings in two options must be the same") ;;
    
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
(* Note to self: @@ has HIGHER precedence than |>, evaluates first *)
let rec free_vars : expr -> varidset = function
  | Var x -> SS.add x SS.empty
  | Literal _ 
  | Raise
  | Unassigned -> SS.empty
(* 
  | List l -> (match l with
    | [] -> SS.empty
    | h :: t -> free_vars @@ List t
              |> SS.union @@ free_vars h) *)

  | Unop (_, e)-> free_vars e
  | Binop (_, e1, e2)
  | App (e1, e2) -> free_vars e2 
                    |> SS.union @@ free_vars e1
  | Conditional (cond, tr, fal) -> free_vars fal 
                                    |> SS.union @@ free_vars tr 
                                    |> SS.union @@ free_vars cond
  | Match (e, cases) -> List.fold_left (fun acc (p, result) -> pattern_bindings p 
                                                                |> SS.diff @@ free_vars result
                                                                |> SS.union acc) SS.empty cases
                        |> SS.union @@ free_vars e
  | Fun (param, body) -> SS.remove param @@ free_vars body
  | Let (x, def, body) -> free_vars body 
                          |> SS.remove x
                          |> SS.union @@ free_vars def
  | Letrec (x, def, body) -> free_vars body
                              |> SS.union @@ free_vars def
                              |> SS.remove x
  ;;
  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". *)

let new_varname : unit -> varid =
  let suffix = ref 0 in
  fun () -> let symbol = "var" ^ string_of_int !suffix in
             suffix := !suffix + 1;
             symbol ;;

(*......................................................................
  Substitution 
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr = 
  if SS.mem var_name @@ free_vars exp then 
    let resub = subst var_name repl in
    match exp with
      | Var x -> if x = var_name then repl else exp
      | Unop (u, e) -> Unop (u, resub e)
      | Binop (b, e1, e2) -> Binop (b, resub e1, resub e2)
      | Conditional (cond, tr, fal) -> Conditional (resub cond, resub tr, resub fal)
      | Match (e, cases) -> Match (resub e, 
          List.map (fun (p, result) -> p, if SS.mem var_name @@ pattern_bindings p then result else resub result) cases)
      | Fun (param, body) -> Fun (param, resub body)
      | Let (x, def, body) -> 
        if SS.mem x @@ free_vars repl then
          let z = new_varname () in
          Let (z, resub def, 
            body 
            |> subst x @@ Var z 
            |> resub)
        else Let (x, resub def, resub body)
      | Letrec (x, def, body) -> 
        if SS.mem x @@ free_vars repl then
          let z = new_varname () in
          Letrec (z,  
            def 
            |> subst x @@ Var z 
            |> resub, 
            body 
            |> subst x @@ Var z 
            |> resub)
        else
          Letrec (x, resub def, resub body)
      | App (f, x) -> App (resub f, resub x)
  (* Rest of the cases are impossible since they have no free variables but to avoid warning *)
      | e -> e
  else exp ;;


(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string = 
  "(" ^ (match exp with
  | Var x -> x
  | Literal l -> print_literal l
  (* | List l -> ( *)
  | Unop (u, e)-> 
    (match u with
      | Negate -> "~-"
      | NegateDot -> "~-."
      | Not -> "not ") 
    ^ exp_to_concrete_string e
  | Binop (b, e1, e2) -> exp_to_concrete_string e1 ^ " " ^ 
    (match b with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Divide -> "/"
      | PlusDot -> "+."
      | MinusDot -> "-."
      | TimesDot -> "*."
      | DivideDot -> "/."
      | Exponent -> "**"
      | NotEqual -> "<>"
      | Equals -> "="
      | LessThan -> "<"
      | GreaterThan -> ">"
      | LessEqual -> "<="
      | GreaterEqual -> ">="
      | And -> "&&"
      | Or -> "||"
      | Concat -> "^"
      | Cons -> "::"
      | Seq -> ";"
      | ListConcat -> "@") 
    ^ " " ^ exp_to_concrete_string e2
  | Match (e, cases) -> 
    "match " ^ exp_to_concrete_string e ^ " with" 
    ^ List.fold_left (fun acc (pattern, result) -> acc ^ "\n| " ^ print_pattern pattern ^ " -> " ^ exp_to_concrete_string result) "" cases
  | Conditional (cond, tr, fal) -> 
    "if " ^ exp_to_concrete_string cond ^ " then " 
    ^ exp_to_concrete_string tr ^ 
    " else " ^ exp_to_concrete_string fal
  | Fun (param, body) -> 
    "fun " ^ param ^ " -> " ^ exp_to_concrete_string body
  | Let (x, def, body) -> 
    "let " ^ x ^ " = " ^ exp_to_concrete_string def ^ " in " ^ exp_to_concrete_string body
  | Letrec (x, def, body) -> 
    "let rec " ^ x ^ " = " ^ exp_to_concrete_string def ^ " in " ^ exp_to_concrete_string body
  | Raise -> "raise EvalException"
  | Unassigned -> "Unassigned"
  | App (f, x) -> exp_to_concrete_string f ^ " " ^ exp_to_concrete_string x
  ) ^ ")" ;;

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)

let rec literaltype_to_string : literaltype -> string = function
  | Num -> "Num"
  | Bool -> "Bool"
  | Float -> "Float"
  | String -> "String"
  | Unit -> "Unit"
  | EmptyList -> "EmptyList"
  | List ltype -> "List (" ^ 
      (match String.split_on_char ',' @@ literaltype_to_string ltype with
        | h :: _ :: _ -> h ^ ")"
        | l -> List.hd l)
      ^ ", ";;

let rec exp_to_abstract_string (exp : expr) : string = (match exp with
  | Var x -> "Var (" ^ x
  | Literal l -> "Literal (" ^ literaltype_to_string (get_tag l) ^ " (" ^ exp_to_concrete_string exp ^ ")"
  | Unop (u, e) -> "Unop (" ^ 
    (match u with
      | Negate -> "Negate"
      | NegateDot -> "NegateDot"
      | Not -> "Not") 
    ^  ", " ^ exp_to_abstract_string e
  | Binop (b, e1, e2) -> "Binop (" ^
    (match b with
      | Plus -> "Plus"
      | Minus -> "Minus"
      | Times -> "Times"
      | Divide -> "Divide"
      | PlusDot -> "PlusDot"
      | MinusDot -> "MinusDot"
      | TimesDot -> "TimesDot"
      | DivideDot -> "DivideDot"
      | Exponent -> "Exponent"
      | NotEqual -> "NotEqual"
      | Equals -> "Equals"
      | LessThan -> "LessThan"
      | GreaterThan -> "GreaterThan"
      | LessEqual -> "LessEqual"
      | GreaterEqual -> "GreaterEqual"
      | And -> "And"
      | Or -> "Or"
      | Concat -> "Concat"
      | Cons -> "Cons"
      | Seq -> "Seq"
      | ListConcat -> "ListConcat") 
    ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2
  | Conditional (cond, tr, fal) -> 
    "Conditional (" ^ exp_to_abstract_string cond ^ ", " ^ exp_to_abstract_string tr ^ ", " ^ exp_to_abstract_string fal
  | Match (e, cases) ->
    "Match (" ^ exp_to_abstract_string e ^ ", ["
    ^ List.fold_left (fun acc (pattern, result) -> acc ^ "(" ^ print_pattern pattern ^ ", " ^ exp_to_abstract_string result ^ ");") "" cases 
    ^ "]"
  | Fun (param, body) -> 
    "Fun (" ^ param ^ ", " ^ exp_to_abstract_string body
  | Let (x, def, body) -> 
    "Let (" ^ x ^ ", " ^ exp_to_abstract_string def ^ ", " ^ exp_to_abstract_string body
  | Letrec (x, def, body) -> 
    "Letrec (" ^ x ^ ", " ^ exp_to_abstract_string def ^ ", " ^ exp_to_abstract_string body
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (f, x) -> 
    "App (" ^ exp_to_abstract_string f ^ ", " ^ exp_to_abstract_string x
  ) ^ ")";;

let abs = ref false;;