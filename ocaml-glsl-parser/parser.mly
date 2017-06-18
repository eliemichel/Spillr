%{
	open Ast
%}

%token CLASS STRUCT ELSE FALSE FOR IF INT NEW NULL
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

%start <Ast.ast> main


%%

main:
	declarations = declaration*
	EOF
	{
		{declarations = declarations}
	}


declaration:
	| sd = structure_declaration       { Structure_declaration sd       }
	| gd = global_variable_declaration { Global_variable_declaration gd }
	| fd = function_declaration        { Function_declaration fd        }


structure_declaration:
	| STRUCT id = IDENT SEMCOL { Ident id }

global_variable_declaration:
	| t = type_ id = IDENT SEMCOL { Global_variable id }

function_declaration:
	| t = type_ name = IDENT LPAR RPAR LBRACE statements = separated_list(SEMCOL, statement) RBRACE
		{ {name = name; type_ = t; statements = statements} }

type_:
	|      VOID   { Void      }
	|      INT    { Int       }
	| id = TIDENT { TIdent id }

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
