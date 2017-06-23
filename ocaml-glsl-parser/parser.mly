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

%token TVOID TBOOL TINT TUINT TFLOAT TDOUBLE TVEC2 TVEC3 TVEC4 TDVEC2 TDVEC3 TDVEC4 TBVEC2 TBVEC3
%token TBVEC4 TIVEC2 TIVEC3 TIVEC4 TUVEC2 TUVEC3 TUVEC4 TMAT2 TMAT3 TMAT4 TMAT2X2 TMAT2X3 TMAT2X4
%token TMAT3X2 TMAT3X3 TMAT3X4 TMAT4X2 TMAT4X3 TMAT4X4 TDMAT2 TDMAT3 TDMAT4 TDMAT2X2 TDMAT2X3
%token TDMAT2X4 TDMAT3X2 TDMAT3X3 TDMAT3X4 TDMAT4X2 TDMAT4X3 TDMAT4X4 TSAMPLER1D TIMAGE1D TSAMPLER2D
%token TIMAGE2D TSAMPLER3D TIMAGE3D TSAMPLERCUBE TIMAGECUBE TSAMPLER2DRECT TIMAGE2DRECT
%token TSAMPLER1DARRAY TIMAGE1DARRAY TSAMPLER2DARRAY TIMAGE2DARRAY TSAMPLERBUFFER TIMAGEBUFFER
%token TSAMPLER2DMS TIMAGE2DMS TSAMPLER2DMSARRAY TIMAGE2DMSARRAY TSAMPLERCUBEARRAY TIMAGECUBEARRAY
%token TSAMPLER1DSHADOW TSAMPLER2DSHADOW TSAMPLER2DRECTSHADOW TSAMPLER1DARRAYSHADOW TSAMPLER2DARRAYSHADOW
%token TSAMPLERCUBESHADOW TSAMPLERCUBEARRAYSHADOW TISAMPLER1D TIIMAGE1D TISAMPLER2D TIIMAGE2D TISAMPLER3D
%token TIIMAGE3D TISAMPLERCUBE TIIMAGECUBE TISAMPLER2DRECT TIIMAGE2DRECT TISAMPLER1DARRAY TIIMAGE1DARRAY
%token TISAMPLER2DARRAY TIIMAGE2DARRAY TISAMPLERBUFFER TIIMAGEBUFFER TISAMPLER2DMS TIIMAGE2DMS
%token TISAMPLER2DMSARRAY TIIMAGE2DMSARRAY TISAMPLERCUBEARRAY TIIMAGECUBEARRAY TATOMIC_UINT TUSAMPLER1D
%token TUIMAGE1D TUSAMPLER2D TUIMAGE2D TUSAMPLER3D TUIMAGE3D TUSAMPLERCUBE TUIMAGECUBE TUSAMPLER2DRECT
%token TUIMAGE2DRECT TUSAMPLER1DARRAY TUIMAGE1DARRAY TUSAMPLER2DARRAY TUIMAGE2DARRAY TUSAMPLERBUFFER
%token TUIMAGEBUFFER TUSAMPLER2DMS TUIMAGE2DMS TUSAMPLER2DMSARRAY TUIMAGE2DMSARRAY TUSAMPLERCUBEARRAY
%token TUIMAGECUBEARRAY

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
    | t = type_ name = IDENT default = option(default_argument)
        { t, name, default }

default_argument:
    | ASSIGN e = expression { e }

type_:
    |      TVOID  { Void      }
    |      TINT   { Int       }
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
