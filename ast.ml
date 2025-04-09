type binop =
  | Add | Sub | Mult | Div
  | Eq | Neq
  | Lt | Gt | Le | Ge
  | And | Or

type unop =
  | Not
  | Neg

type expr =
  | IntLit of int
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | FunCall of string * expr list
  | IfExpr of expr * stmt list * (expr * stmt list) list * stmt list option
  | LoopExpr of loop_expr
  | Parens of expr

and loop_expr =
  | For of string * expr * expr * stmt list
  | While of expr * stmt list
  | DoWhile of stmt list * expr

and stmt =
  | Let of string * expr
  | Print of expr
  | FunctionDef of function_def
  | Expr of expr

and function_def = {
  fname : string;
  params : string list;
  body : stmt list;
}

type program = stmt list

