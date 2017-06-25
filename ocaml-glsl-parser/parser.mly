%{
    open Ast
%}

%token STRUCT ELSE FOR WHILE IF
%token RETURN DISCARD
%token LBRACE RBRACE LBRACKET RBRACKET LPAR RPAR
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN DIV_ASSIGN MOD_ASSIGN LFLOW_ASSIGN RFLOW_ASSIGN BITAND_ASSIGN BITXOR_ASSIGN BITOR_ASSIGN
%token OR AND XOR DBLEQ NEQ LT LFLOW RFLOW
%token LEQ GT GEQ PLUS MINUS STAR DIV MOD NOT INCR DECR
%token BITNOT BITAND BITOR BITXOR
%token SEMCOL COL COMMA

%token ATTRIBUTE CONST UNIFORM VARYING BUFFER SHARED COHERENT VOLATILE RESTRICT READONLY WRITEONLY
%token ATOMIC_UINT LAYOUT CENTROID FLAT SMOOTH NOPERSPECTIVE PATCH SAMPLE BREAK CONTINUE
%token DO SWITCH CASE DEFAULT SUBROUTINE IN OUT INOUT INVARIANT
%token PRECISE PRECISION LOWP MEDIUMP HIGHP

%token EOF
%token <string> IDENT TIDENT
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
    | q = qualifiers t = type_ id = IDENT SEMCOL { q, t, id }

function_declaration:
    | p = prototype statements = statement_bloc
        { let (t, name, params) = p in { name = name; type_ = t; parameters = params; body = Bloc statements } }
    | p = prototype SEMCOL
        { let (t, name, params) = p in { name = name; type_ = t; parameters = params; body = Empty } }

prototype:
    | t = type_ name = IDENT LPAR params = separated_list(COMMA, parameter) RPAR
        { t, name, params }

parameter:
    | q = parameter_qualifiers t = type_ name = IDENT? size = array_size? default = option(default_argument)
        { q, t, name, size, default }

array_size:
    | LBRACKET s = UINT_CONST RBRACKET
        { s }

qualifiers:
    | s = storage? a = auxiliary? m = memory* p = precision?
        { s, a, m, p }

parameter_qualifiers:
    | CONST IN  { ConstInParam }
    | IN        { InParam      }
    | OUT       { OutParam     }
    | INOUT     { InOutParam   }

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
    | COHERENT  { Coherent  }
    | VOLATILE  { Volatile  }
    | RESTRICT  { Restrict  }
    | READONLY  { ReadOnly  }
    | WRITEONLY { WriteOnly }

precision:
    | LOWP    { Lowp    }
    | MEDIUMP { Mediump }
    | HIGHP   { Highp   }

default_argument:
    | ASSIGN e = expression { e }

type_:
    | id = TIDENT { TIdent id }

statement_bloc:
    | LBRACE statements = statement* RBRACE
        { statements }

statement:
    | SEMCOL                                                         { Empty                      }
    | e = expression SEMCOL                                          { Expression e               }
    | b = statement_bloc                                             { Bloc b                     }
    | RETURN e = expression? SEMCOL                                  { Return e                   }
    | IF LPAR e = expression RPAR s = statement %prec IFX            { If_else (e, s, Empty)      }
    | IF LPAR e = expression RPAR s1 = statement ELSE s2 = statement { If_else (e, s1, s2)        }
    | WHILE LPAR e = expression RPAR s = statement { For ([], e, [], s) }
    | FOR
        LPAR
            init = separated_list(COMMA, expression) SEMCOL
            e = expression? SEMCOL
            loop = separated_list(COMMA, expression)
        RPAR
        s = statement
        { match e with
            | None -> For (init, Bool true, loop, s)
            | Some c -> For (init, c, loop, s)
        }
    | t = type_  l = separated_nonempty_list(COMMA, typed_declaration) SEMCOL
        { Group (List.map (fun (var, init) -> Declaration (t, var, init)) l) }
    | SWITCH LPAR e = expression RPAR sb = statement_bloc { Switch (e, Bloc sb) }
    | CASE e = expression COL { Case e      }
    | DEFAULT COL             { DefaultCase }
    | BREAK SEMCOL            { Break       }
    | CONTINUE SEMCOL         { Continue    }

typed_declaration:
    | var = var init = var_init? { var, init }

var:
    | id = IDENT { id }

var_init:
    | ASSIGN e = expression { e }

expression:
    | b = BOOL_CONST     { Bool b        }
    | i = INT_CONST      { Integer i     }
    | i = UINT_CONST     { Unsigned i    }
    | t = type_          { Constructor t }
    | id = IDENT         { Ident id }
    | f = expression LPAR args = separated_list(COMMA, expression) RPAR { Application (f, args) }
    | LPAR e = expression RPAR { e }
    | op = prefix_unary e = expression                         { PrefixUnop (op, e)      }
    | e = expression op = postfix_unary                        { PostfixUnop (op, e)     }
    | e1 = expression op = binary_operator e2 = expression     { Binop (op, e1, e2)      }
    | e1 = expression op = assignment_operator e2 = expression { Assignment (op, e1, e2) }


%inline binary_operator:
    | DBLEQ { Dbleq }
    | NEQ   { Neq   }
    | LT    { Lt    }
    | LEQ   { Leq   }
    | GT    { Gt    }
    | GEQ   { Geq   }
    | PLUS  { Add   }
    | MINUS { Sub   }
    | STAR  { Mult  }
    | DIV   { Div   }
    | MOD   { Mod   }
    | AND   { And   }
    | OR    { Or    }
    | XOR    { Xor }
    | BITAND { BitAnd }
    | BITOR  { BitOr }
    | BITXOR { BitXor }
    | BITNOT { BitNot }
    | LFLOW  { LFlow }
    | RFLOW  { RFlow }

prefix_unary:
    | NOT   { Not        }
    | INCR  { PrefixIncr }
    | DECR  { PrefixDecr }
    | PLUS  { Plus       }
    | MINUS { Minus      }

postfix_unary:
    | INCR  { PostfixIncr }
    | DECR  { PostfixDecr }

%inline assignment_operator:
    | ASSIGN        { Assign   }
    | PLUS_ASSIGN   { Plus_assign   }
    | MINUS_ASSIGN  { Minus_assign  }
    | STAR_ASSIGN   { Star_assign   }
    | DIV_ASSIGN    { Div_assign    }
    | MOD_ASSIGN    { Mod_assign    }
    | LFLOW_ASSIGN  { Lflow_assign  }
    | RFLOW_ASSIGN  { Rflow_assign  }
    | BITAND_ASSIGN { Bitand_assign }
    | BITXOR_ASSIGN { Bitxor_assign }
    | BITOR_ASSIGN  { Bitor_assign  }
