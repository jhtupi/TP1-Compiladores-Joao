{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let unterminated_string loc =
    Error.error loc "unterminated string"

  let unterminated_comment loc=
    Error.error loc "unterminated comment"

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }
}

let spaces = [' ' '\t'] +
let litint = [ '0' - '9'] +

let alpha = ['A'-'Z' 'a'-'z'] 
let digit = ['0' - '9']
let id = (alpha ['_']?)+ ((alpha ['_']?)digit)*
let asciichar = ['0'-'9']['0'-'9']?['0'-'9']?

rule token = parse
  | spaces          { token lexbuf }
  | '\n'            { L.new_line lexbuf; token lexbuf }
  | "/*"            { lex_comment 0 lexbuf }
  | "while"         { WHILE }
  | "for"           { FOR }
  | "to"            { TO }
  | "break"         { BREAK }
  | "let"           { LET }
  | "in"            { IN }
  | "end"           { END }
  | "function"      { FUNCTION }
  | "var"           { VAR }
  | "type"          { TYPE }
  | "array"         { ARRAY }
  | "if"            { IF }
  | "else"          { ELSE }
  | "then"          { THEN }
  | "do"            { DO }
  | "of"            { OF }
  | "nil"           { NIL }
  | ","             { COMMA }
  | ":"             { COLON }
  | ";"             { SEMI_COLON }
  | "("             { L_PAREN }
  | ")"             { R_PAREN }
  | "["             { L_BRACKET }
  | "]"             { R_BRACKET }
  | "{"             { L_BRACE }
  | "}"             { R_BRACE }
  | "."             { DOT }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { TIMES }
  | "/"             { DIVIDE }
  | "="              { EQ }
  | "<>"             { NEG }
  | "<"              { LT }
  | "<="             { LE }
  | ">"              { GT }
  | ">="             { GE }
  | "&"              { AND }
  | "|"              { OR }
  | ":="             { ATRIB }
  | litint as lxm    { INT (int_of_string lxm) }
  | '"'              { lex_string (Buffer.create 64) lexbuf}
  | id as lxm        { ID lxm }
  | eof              { EOF }
  | _                { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }

and lex_string buf =
  parse
  | '"'                       { STRING (Buffer.contents buf) }
  | '\\' 'n'                  { Buffer.add_char buf '\n'; lex_string buf lexbuf }
  | '\\' 't'                  { Buffer.add_char buf '\t'; lex_string buf lexbuf }
  | '\\' (asciichar as lxm)   { Buffer.add_char buf (char_of_int (int_of_string lxm)); lex_string buf lexbuf }
  | '\\' '\\'                 { Buffer.add_char buf '\\'; lex_string buf lexbuf }
  | '\\' '^' (alpha as lxm)   { Buffer.add_string buf ("Control Character: " ^ String.make 1 lxm); lex_string buf lexbuf }
  | [^ '"' '\\']+
    { 
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      lex_string buf lexbuf
    }
  | _ { 
        illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) 
      }
  | eof { unterminated_string (Location.curr_loc lexbuf) }


and lex_comment depth = parse
    | "/*" { lex_comment (depth + 1) lexbuf }
    | "*/"
        { if depth = 0 then
            token lexbuf
          else
            lex_comment (depth - 1) lexbuf
        }
    | eof
        { 
          unterminated_comment (Location.curr_loc lexbuf)
        }
    | _ { lex_comment depth lexbuf }