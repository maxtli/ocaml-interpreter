                
%{
  open Expr ;;
  let sugarfun = List.fold_left (fun acc cur -> Fun(cur, acc)) ;;
%}

%token EOF
%token WILDCARD
%token OPEN CLOSE
%token OPENB CLOSEB
%token <Expr.binop> SEQ
%token LET ARROW DOT IN REC MATCH WITH
%token NEG NEGDOT
%token NOT
%token <Expr.binop> PLUS MINUS PLUSDOT MINUSDOT
%token <Expr.binop> TIMES TIMESDOT DIVIDE DIVIDEDOT
%token <Expr.binop> EXPONENT
%token <Expr.binop> CONS
%token <Expr.binop> CONCAT
%token <Expr.binop> LISTCONCAT
%token APP
%token SEP REVAPP
%token <Expr.binop> LESSTHAN GREATERTHAN LESSEQUAL GREATEREQUAL NOTEQUAL EQUALS
%token <Expr.binop> AND OR
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token <float> FLOAT
%token <string> STRING
%token UNIT
%token TRUE FALSE

%nonassoc IF
%right OR
%right AND
%left LESSTHAN GREATERTHAN LESSEQUAL GREATEREQUAL NOTEQUAL EQUALS
%left REVAPP SEP
%right CONCAT LISTCONCAT APP
%right CONS
%left PLUS MINUS PLUSDOT MINUSDOT
%left TIMES TIMESDOT DIVIDE DIVIDEDOT
%right EXPONENT
%nonassoc NOT
%nonassoc NEG NEGDOT

%start input
%type <Expr.expr> input

%%
input:  exp EOF                 { $1 }

exp:    expnoseq SEQ exp                { Binop(Seq, $1, $3) }
        | expnoseq                      { $1 }

expnoseq: expnoseq expnoapp            { App($1, $2) }
        | expnoapp                      { $1 }

expnoapp: ID                                    { Var $1 }
        | binop                                 { match $1 with x, y, z -> Binop (x,y,z) }
        | constant                              { Literal($1) }
        | exp APP exp                           { App($1, $3) }
        | exp REVAPP exp                        { App($3, $1) }
        | NEG exp                               { Unop(Negate, $2) }
        | NEGDOT exp                            { Unop(NegateDot, $2) }
        | NOT exp                               { Unop(Not, $2) }
        | OPENB objs CLOSEB                     { $2 }
        | IF exp THEN exp ELSE exp              { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp              { Let($2, $4, $6) }
        | LET ID params EQUALS exp IN exp       { Let($2, sugarfun $5 $3, $7)}
        | LET pattern EQUALS exp IN exp         { Match($4, [($2, $6)])}
        | LET REC ID EQUALS exp IN exp          { Letrec($3, $5, $7) }
        | LET REC ID params EQUALS exp IN exp   { Letrec($3, sugarfun $6 $4, $8) }
        | FUNCTION params ARROW exp             { sugarfun $4 $2 } 
        | MATCH exp WITH c = matchcases         { Match ($2, c)}
        | MATCH exp WITH SEP c = matchcases     { Match ($2, c)}
        | RAISE                                 { Raise }
        | OPEN exp CLOSE                        { $2 }
;

params: ID                                      { [$1] }
        | params ID                             { $2 :: $1 }

binop: exp PLUS exp           
        | exp MINUS exp       
        | exp TIMES exp       
        | exp DIVIDE exp      
        | exp EQUALS exp      
        | exp NOTEQUAL exp    
        | exp LESSTHAN exp    
        | exp GREATERTHAN exp 
        | exp LESSEQUAL exp   
        | exp GREATEREQUAL exp
        | exp PLUSDOT exp     
        | exp MINUSDOT exp    
        | exp TIMESDOT exp    
        | exp DIVIDEDOT exp   
        | exp EXPONENT exp    
        | exp AND exp         
        | exp OR exp          
        | exp CONCAT exp
        | exp CONS exp
        | exp LISTCONCAT exp          { $2, $1, $3 }


constant: INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | STRING                { String $1 }
        | UNIT                  { Unit }

matchcases : matchcase          { [$1] }
        | matchcase SEP matchcases { $1 :: $3 }

matchcase : pattern ARROW exp   { $1, $3 }

pattern: constant               { Literal($1) }
        | WILDCARD              { Wildcard }
        | ID                    { Var $1 }
        | OPEN pattern CLOSE    { $2 }
        | pattern SEP pattern   { Options ($1, $3) }
        | pattern CONS pattern  { Cons ($1, $3) }
        | OPENB patternlist CLOSEB            
        | OPENB patternlist SEQ CLOSEB    { List $2 }

patternlist: |                    { [] }
        | pattern                 { [$1] }
        | pattern SEQ patternlist { $1 :: $3 }

objs:   |                       { Literal(List(EmptyList, [])) }
        | expnoseq                   { Binop(Cons, $1, Literal(List(EmptyList, []))) }
        | expnoseq SEQ objs          { Binop(Cons, $1, $3) }

%%
