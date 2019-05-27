// parser.mly

%token <int> 	INT
%token <string> ID
%token <string> STRING
%token			COMMENT
%token       	EOF
%token		 	WHILE
%token       	FOR
%token		 	TO
%token       	BREAK
%token		 	LET
%token		 	IN
%token		 	END
%token		 	FUNCTION
%token		 	VAR
%token		 	TYPE
%token		 	ARRAY
%token		 	IF
%token		 	THEN
%token		 	ELSE
%token		 	DO
%token		 	OF
%token		 	NIL
%token		 	COMMA
%token		 	COLON
%token		 	SEMI_COLON
%token		 	L_PAREN
%token		 	R_PAREN
%token		 	L_BRACKET
%token		 	R_BRACKET
%token		 	L_BRACE
%token		 	R_BRACE
%token		 	DOT
%token		 	PLUS
%token		 	MINUS
%token		 	TIMES
%token		 	DIVIDE
%token		 	EQ
%token		 	NEG
%token		 	LT
%token		 	LE
%token		 	GT
%token		 	GE
%token		 	AND
%token		 	OR
%token		 	ATRIB

%%