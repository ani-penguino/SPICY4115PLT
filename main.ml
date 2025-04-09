open Parser
open Lexer
open Ast

let parse_with_error lexbuf =
  try
    Parser.program Lexer.token lexbuf
  with
  | Parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "Syntax error at line %d, column %d\n"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
      exit (-1)

let () =
  let input_channel =
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin
  in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = parse_with_error lexbuf in

  let rec string_of_expr = function
    | IntLit n -> string_of_int n
    | BoolLit b -> string_of_bool b
    | StringLit s -> "\"" ^ s ^ "\""
    | Id name -> name
    | Binop (e1, op, e2) ->
        "(" ^ string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 ^ ")"
    | Unop (Not, e) -> "not " ^ string_of_expr e
    | Unop (Neg, e) -> "-" ^ string_of_expr e
    | FunCall (name, args) ->
        name ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
    | IfExpr (cond, then_block, elifs, else_block) ->
        let then_str = "if " ^ string_of_expr cond ^ " {...}" in
        let elif_strs = List.map (fun (e, _) -> "elseif " ^ string_of_expr e ^ " {...}") elifs in
        let else_str = match else_block with
          | Some stmts when stmts <> [] -> "else {...}"
          | _ -> ""
        in
        String.concat " " (then_str :: elif_strs @ [else_str])
    | LoopExpr (For (id, start, stop, _)) ->
        "for " ^ id ^ " in " ^ string_of_expr start ^ ".." ^ string_of_expr stop ^ " {...}"
    | LoopExpr (While (cond, _)) ->
        "while " ^ string_of_expr cond ^ " {...}"
    | LoopExpr (DoWhile (_, cond)) ->
        "do {...} while " ^ string_of_expr cond
    | Parens e -> "(" ^ string_of_expr e ^ ")"

  and string_of_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="
    | And -> "and"
    | Or -> "or"

  and string_of_stmt = function
    | Let (id, expr) -> "let " ^ id ^ " = " ^ string_of_expr expr
    | Print expr -> "print " ^ string_of_expr expr
    | Expr expr -> string_of_expr expr
    | FunctionDef { fname; params; body } ->
        "fun " ^ fname ^ "(" ^ String.concat ", " params ^ ") { ... }"
  in

  List.iter (fun stmt -> print_endline (string_of_stmt stmt)) ast

