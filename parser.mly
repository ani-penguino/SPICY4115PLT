(* === FIXED parser.mly === *)
%{
open Ast
%}

/* Tokens */
%token <int> INT
%token <string> STRING
%token <bool> BOOLEAN_LITERAL
%token <string> IDENTIFIER
%token IF ELSE ELSEIF
%token FOR WHILE DO
%token LET PRINT
%token FUN
%token RETURN
%token LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT GT LE GE
%token AND OR NOT
%token EOF
%token IN
%token DOTDOT
%token INT_TYPE BOOL_TYPE STRING_TYPE VOID_TYPE
%token NEWLINE



/* Precedence */
%left OR
%left AND
%nonassoc EQ NEQ LT GT LE GE
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%right UMINUS

/* Start symbol */
%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { List.rev $1 }

stmt_list:
    /* empty */ { [] }
  | stmt_list stmt NEWLINE { $2 :: $1 }
  | stmt_list NEWLINE { $1 }


stmt:
    LET IDENTIFIER ASSIGN expr { Let ($2, $4) }
  | PRINT expr { Print $2 }
  | expr { Expr $1 }
  | function_def { FunctionDef $1 }

expr:
    INT { IntLit $1 }
  | STRING { StringLit $1 }
  | BOOLEAN_LITERAL { BoolLit $1 }
  | IDENTIFIER { Id $1 }
  | expr PLUS expr { Binop ($1, Add, $3) }
  | expr MINUS expr { Binop ($1, Sub, $3) }
  | expr TIMES expr { Binop ($1, Mult, $3) }
  | expr DIVIDE expr { Binop ($1, Div, $3) }
  | expr EQ expr { Binop ($1, Eq, $3) }
  | expr NEQ expr { Binop ($1, Neq, $3) }
  | expr LT expr { Binop ($1, Lt, $3) }
  | expr GT expr { Binop ($1, Gt, $3) }
  | expr LE expr { Binop ($1, Le, $3) }
  | expr GE expr { Binop ($1, Ge, $3) }
  | expr AND expr { Binop ($1, And, $3) }
  | expr OR expr { Binop ($1, Or, $3) }
  | NOT expr { Unop (Not, $2) }
  | MINUS expr %prec UMINUS { Unop (Neg, $2) }
  | IDENTIFIER LPAREN arg_list_opt RPAREN { FunCall ($1, $3) }
  | if_expr { $1 }
  | loop_expr { $1 }
  | LPAREN expr RPAREN { $2 }

arg_list_opt:
    /* empty */ { [] }
  | arg_list { $1 }

arg_list:
    expr { [$1] }
  | arg_list COMMA expr { $3 :: $1 }

function_def:
  FUN IDENTIFIER LPAREN param_list_opt RPAREN LBRACE stmt_list RBRACE {
    { fname = $2; params = List.rev $4; body = List.rev $7 }
  }

param_list_opt:
    /* empty */ { [] }
  | param_list { $1 }

param_list:
    IDENTIFIER { [$1] }
  | param_list COMMA IDENTIFIER { $3 :: $1 }

if_expr:
  IF expr LBRACE stmt_list RBRACE {
    IfExpr ($2, List.rev $4, [], None)
  }
| IF expr LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE {
    IfExpr ($2, List.rev $4, [], Some (List.rev $8))
  }
| IF expr LBRACE stmt_list RBRACE elseif_blocks {
    IfExpr ($2, List.rev $4, List.rev $6, None)
  }
| IF expr LBRACE stmt_list RBRACE elseif_blocks ELSE LBRACE stmt_list RBRACE {
    IfExpr ($2, List.rev $4, List.rev $6, Some (List.rev $9))
  }

elseif_blocks:
    ELSEIF expr LBRACE stmt_list RBRACE { [($2, List.rev $4)] }
  | elseif_blocks ELSEIF expr LBRACE stmt_list RBRACE {
      ($3, List.rev $5) :: $1
    }

loop_expr:
    FOR IDENTIFIER IN expr DOTDOT expr LBRACE stmt_list RBRACE {
      LoopExpr (For ($2, $4, $6, List.rev $8))
    }
  | WHILE expr LBRACE stmt_list RBRACE {
      LoopExpr (While ($2, List.rev $4))
    }
  | DO LBRACE stmt_list RBRACE WHILE expr {
      LoopExpr (DoWhile (List.rev $3, $6))
    }

