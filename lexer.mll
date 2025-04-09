(* Scanner for the SPICY Language *)
{
open Parser

let keyword_table = Hashtbl.create 16
let () =
  List.iter (fun (k, v) -> Hashtbl.add keyword_table k v)
    [
      ("true", BOOLEAN_LITERAL(true));
      ("false", BOOLEAN_LITERAL(false));
      ("not", NOT);
      ("and", AND);
      ("or", OR);
      ("let", LET);
      ("print", PRINT);
      ("fun", FUN);
      ("if", IF);
      ("elseif", ELSEIF);
      ("else", ELSE);
      ("for", FOR);
      ("in", IN);
      ("while", WHILE);
      ("do", DO);
      ("return", RETURN);
      ("Int", INT_TYPE);
      ("Bool", BOOL_TYPE);
      ("String", STRING_TYPE);
      ("Void", VOID_TYPE)
    ]

let reserved id =
  try Hashtbl.find keyword_table id
  with Not_found -> IDENTIFIER(id)

let strip_quotes s = String.sub s 1 (String.length s - 2)
}

rule tokenize = parse
  | [' ' '\t' '\r'] { tokenize lexbuf }
  | '\n'             { NEWLINE }
  | '#'[^'\n']*      { tokenize lexbuf }

  (* Literals *)
  | ['0'-'9']+ as num         { INT(int_of_string num) }
  | '"' [^'"']* '"' as str   { STRING(strip_quotes str) }


  (* Operators *)
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { TIMES }
  | "/"     { DIVIDE }
  | "=="     { EQ }
  | "!="    { NEQ }
  | "<="    { LE }
  | ">="    { GE }
  | "<"     { LT }
  | ">"     { GT }

  (* Punctuation *)
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "="     { ASSIGN }
  | "{"     { LBRACE }
  | "}"     { RBRACE }
  | ","     { COMMA }
  | ".."    { DOTDOT }

  (* Identifiers and Keywords *)
  | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as id { reserved id }

  | eof { EOF }
  | _ as c { raise (Failure("Unexpected character: " ^ Char.escaped c)) }

{
let token = tokenize
}

