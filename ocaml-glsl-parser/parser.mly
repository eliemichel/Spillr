%{
    open Ast
%}

%token STRUCT ELSE FOR WHILE IF NEW NULL
%token PUBLIC RETURN DISCARD
%token LBRACE RBRACE LBRACKET RBRACKET LPAR RPAR
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN LFLOW_ASSIGN RFLOW_ASSIGN BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%token OR AND XOR DBLEQ NEQ LT LFLOW RFLOW
%token LEQ GT GEQ PLUS MINUS STAR DIV MOD NOT INCR DECR
%token BITNOT BITAND BITOR BITXOR
%token DOT SEMCOL COL COMMA

%token ATTRIBUTE CONST UNIFORM VARYING BUFFER SHARED COHERENT VOLATILE RESTRICT READONLY WRITEONLY
%token ATOMIC_UINT LAYOUT CENTROID FLAT SMOOTH NOPERSPECTIVE PATCH SAMPLE BREAK CONTINUE
%token DO SWITCH CASE DEFAULT SUBROUTINE IN OUT INOUT INVARIANT
%token PRECISE PRECISION LOWP MEDIUMP HIGHP

%token EOF
%token <string> IDENT TIDENT STRING_CONST
%token <string> UINT_CONST INT_CONST
%token <bool> BOOL_CONST

%left COMMA
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN LFLOW_ASSIGN RFLOW_ASSIGN BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%left OR
%left XOR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left NEQ DBLEQ
%left LT LEQ GT GEQ
%left LFLOW RFLOW
%left PLUS MINUS
%left STAR DIV MOD
%right NOT INCR DECR BITNOT
%left DOT
%left LBRACKET
%left LPAR

%nonassoc IFX
%nonassoc ELSE

%start <Ast.ast> main


%%

main:
    | declarations = declaration* EOF
        { {declarations = declarations} }


declaration:
    | sd = structure_declaration       { Structure_declaration sd       }
    | gd = global_variable_declaration { Global_variable_declaration gd }
    | fd = function_declaration        { Function_declaration fd        }


structure_declaration:
    | STRUCT name = IDENT LBRACE fields = separated_list(SEMCOL, struct_field) SEMCOL? RBRACE SEMCOL
        { { struct_name = name; fields = fields } }

struct_field:
    t = type_ id = IDENT { t, id }

global_variable_declaration:
    | t = type_ id = IDENT SEMCOL { Global_variable id }

function_declaration:
    | t = type_ name = IDENT
      LPAR args = separated_list(COMMA, function_argument) RPAR
      LBRACE statements = separated_list(SEMCOL, statement) SEMCOL? RBRACE
        { { name = name; type_ = t; arguments = args; statements = statements; is_prototype = false } }
    | t = type_ name = IDENT
      LPAR args = separated_list(COMMA, function_argument) RPAR
      SEMCOL
        { { name = name; type_ = t; arguments = args; statements = []; is_prototype = true } }

function_argument:
    | s = storage? a = auxiliary? m = memory p = precision
      t = type_ name = IDENT? size = array_size? default = option(default_argument)
        { s, a, t, name, size, default }

array_size:
    | LBRACKET s = UINT_CONST RBRACKET
        { s }

storage:
    | CONST     { Const     }
    | IN        { In        }
    | OUT       { Out       }
    | ATTRIBUTE { Attribute }
    | UNIFORM   { Uniform   }
    | VARYING   { Varying   }
    | BUFFER    { Buffer    }
    | SHARED    { Shared    }

auxiliary:
    | CENTROID { Centroid    }
    | SAMPLE   { Sample      }
    | PATCH    { Patch       }

memory:
    | { 0 }

precision:
    | { 0 }

default_argument:
    | ASSIGN e = expression { e }

type_:
    | id = TIDENT { TIdent id }


statement:
    | e = expression SEMCOL { e }

expression:
    | i = INT_CONST                         { int_of_string i }
    | i = UINT_CONST                        { int_of_string i }
    | LPAR e = expression RPAR              { e               }
    | e1 = expression PLUS e2 = expression  { e1 + e2         }
    | e1 = expression MINUS e2 = expression { e1 - e2         }
    | e1 = expression STAR e2 = expression  { e1 * e2         }
    | e1 = expression DIV e2 = expression   { e1 / e2         }
