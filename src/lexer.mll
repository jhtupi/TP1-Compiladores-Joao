{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }
}

let spaces = [' ' '\t'] +
let litint = [ '0' - '9'] +

rule token = parse
  | spaces        { token lexbuf }
  | '\n'          { L.new_line lexbuf; token lexbuf }
  | litint as lxm { INT (int_of_string lxm) }
  | eof           { EOF }
  | "/*"      { comment 0 lexbuf }
  | '"'       { string lexbuf.Lexing.lex_start_p "" lexbuf }
  | "array"     { L.ARRAY }
  | "break"     { L.BREAK }
  | "do"      { L.DO }
  | "else"      { L.ELSE}
  | "end"     { L.END }
  | "for"     { L.FOR }
  | "function"    { L.FUNCTION }
  | "if"      { L.IF }
  | "in"      { L.IN }
  | "let"     { L.LET }
  | "nil"     { L.NIL }
  | "of"      { L.OF }
  | "then"      { L.THEN }
  | "to"      { L.TO }
  | "type"      { L.TYPE }
  | "var"     { L.VAR }
  | "while"     { L.WHILE }
  | ":= "     { L.ASSIGN }
  | '|'       { L.OR }
  | '&'       { L.AND }
  | "->"      { L.ARROW }
  | "=>"      { L.FAT_ARROW }
  | '='       { L.EQ }
  | "<>"      { L.NEQ }
  | '<'       { L.LT }
  | "<="      { L.LE }
  | '>'       { L.GT }
  | ">="      { L.GE }
  | '+'       { L.PLUS }
  | '-'       { L.MINUS }
  | '*'       { L.TIMES }
  | '/'       { L.DIVIDE }
  | '('       { L.LPAREN }
  | ')'       { L.RPAREN }
  | '['       { L.LBRACK }
  | ']'       { L.RBRACK }
  | '{'       { L.LBRACE }
  | '}'       { L.RBRACE }
  | '.'       { L.DOT }
  | ':'       { L.COLON }
  | ','       { L.COMMA }
  | ';'       { L.SEMI }
  | _             { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
  
{
  let string_of_token = function
    L.INT x -> INT(" ^ string_of_int x ^ ")"
    | L.STRING x -> "STRING(" ^ x ^ ")"
    | L.ID x -> "ID(" ^ x ^ ")"
    | L.FOR -> "FOR"
    | L.WHILE -> "WHILE"
    | L.BREAK -> "BREAK"
    | L.LET -> "LET"
    | L.IN -> "IN"
    | L.NIL -> "NIL"
    | L.TO -> "TO"
    | L.END -> "END"
    | L.FUNCTION -> "FUNCTION"
    | L.VAR -> "VAR"
    | L.TYPE -> "TYPE"
    | L.ARRAY -> "ARRAY"
    | L.IF -> "IF"
    | L.THEN -> "THEN"
    | L.ELSE -> "ELSE
    | L.DO -> "DO"
    | L.OF -> "OF"
    | L.LPAREN -> "LPAREN"
    | L.RPAREN -> "RPAREN"
    | L.LBRACK -> "LBRACK"
    | L.RBRACK -> "RBRACK"
    | L.LBRACE -> "LBRACE"
    | L.RBRACE -> "RBRACE"
    | L.ASSIGN -> "ASSIGN"
    | L.DOT -> "DOT"
    | L.COLON -> "COLON"
    | L.COMMA -> "COMMA"
    | L.SEMI -> "SEMI"
    | L.OR -> "OR"
    | L.AND -> "AND"
    | L.EQ -> "EQ"
    | L.NEQ -> "NEQ"
    | L.LT -> "LT"
    | L.LE -> "LE"
    | L.GT -> "GT"
    | L.GE -> "GE"
    | L.ARROW -> "ARROW"
    | L.FAT_ARROW -> "FAT_ARROW"
    | L.PLUS -> "PLUS"
    | L.MINUS -> "MINUS"
    | L.TIMES -> "TIMES"
    | L.DIVIDE -> "DIVIDE"
    | L.EOF -> "EOF"
}