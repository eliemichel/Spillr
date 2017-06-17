%{
	open Ast
%}

%token CLASS ELSE FALSE FOR IF INT NEW NULL
%token PUBLIC RETURN THIS TRUE VIRTUAL VOID WHILE
%token LBRACE RBRACE LPAR RPAR EQ OR AND DBLEQ NEQ LT
%token LEQ GT GEQ PLUS MINUS STAR DIV MOD NOT INCR DECR AMP
%token ARROW DOT SEMCOL COL COMMA IOSTREAM COUT FLOW
%token EOF
%token <string> IDENT TIDENT INTEGER STRING

%right EQ
%left OR
%left AND
%left NEQ DBLEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left STAR DIV MOD
%right NOT INCR DECR AMP
%left DOT ARROW LPAR

%nonassoc IFX
%nonassoc ELSE

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start <Ast.ast> main


%%

main:
	declarations = declaration*
	EOF
	{
		{declarations = declarations}
	}


declaration:
	| tident = TIDENT ident = IDENT LPAR RPAR LBRACE statements = statement* RBRACE
	{
		Ident ident
	}
	| VOID ident = IDENT LPAR RPAR LBRACE statements = statement* RBRACE
	{
		Ident ident
	}


/* expressions end with a semicolon, not with a newline character */
statement:
| e = expr SEMCOL { e }

expr:
| i = INTEGER
    { int_of_string i }
| LPAR e = expr RPAR
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr STAR e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
