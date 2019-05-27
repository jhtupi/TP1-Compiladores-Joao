
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TYPE
  | TO
  | TIMES
  | THEN
  | STRING of (string)
  | SEMI_COLON
  | R_PAREN
  | R_BRACKET
  | R_BRACE
  | PLUS
  | OR
  | OF
  | NIL
  | NEG
  | MINUS
  | L_PAREN
  | L_BRACKET
  | L_BRACE
  | LT
  | LET
  | LE
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GT
  | GE
  | FUNCTION
  | FOR
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DO
  | DIVIDE
  | COMMENT
  | COMMA
  | COLON
  | BREAK
  | ATRIB
  | ARRAY
  | AND
