// parser.mly

%token <int> INT
%token <string> STRING
%token <string> ID
%token FOR WHILE BREAK LET IN NIL TO END
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token DOT COLON COMMA SEMI ARROW
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LE GT GE
%token FAT_ARROW
%token AND OR
%token ASSIGN
%token EOF

%start program

%type <unit> program

%%

program:
| EOF { }
;

%%