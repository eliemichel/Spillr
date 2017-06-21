{
    open Lexing
    open Parser
    
    exception Error of string

    let types_assoc = [
        "void", TVOID;
        "bool", TBOOL;
        "int", TINT;
        "uint", TUINT;
        "float", TFLOAT;
        "double", TDOUBLE;
        "vec2", TVEC2;
        "vec3", TVEC3;
        "vec4", TVEC4;
        "dvec2", TDVEC2;
        "dvec3", TDVEC3;
        "dvec4", TDVEC4;
        "bvec2", TBVEC2;
        "bvec3", TBVEC3;
        "bvec4", TBVEC4;
        "ivec2", TIVEC2;
        "ivec3", TIVEC3;
        "ivec4", TIVEC4;
        "uvec2", TUVEC2;
        "uvec3", TUVEC3;
        "uvec4", TUVEC4;
        "mat2", TMAT2;
        "mat3", TMAT3;
        "mat4", TMAT4;
        "mat2x2", TMAT2X2;
        "mat2x3", TMAT2X3;
        "mat2x4", TMAT2X4;
        "mat3x2", TMAT3X2;
        "mat3x3", TMAT3X3;
        "mat3x4", TMAT3X4;
        "mat4x2", TMAT4X2;
        "mat4x3", TMAT4X3;
        "mat4x4", TMAT4X4;
        "dmat2", TDMAT2;
        "dmat3", TDMAT3;
        "dmat4", TDMAT4;
        "dmat2x2", TDMAT2X2;
        "dmat2x3", TDMAT2X3;
        "dmat2x4", TDMAT2X4;
        "dmat3x2", TDMAT3X2;
        "dmat3x3", TDMAT3X3;
        "dmat3x4", TDMAT3X4;
        "dmat4x2", TDMAT4X2;
        "dmat4x3", TDMAT4X3;
        "dmat4x4", TDMAT4X4;
        "sampler1D", TSAMPLER1D;
        "image1D", TIMAGE1D;
        "sampler2D", TSAMPLER2D;
        "image2D", TIMAGE2D;
        "sampler3D", TSAMPLER3D;
        "image3D", TIMAGE3D;
        "samplerCube", TSAMPLERCUBE;
        "imageCube", TIMAGECUBE;
        "sampler2DRect", TSAMPLER2DRECT;
        "image2DRect", TIMAGE2DRECT;
        "sampler1DArray", TSAMPLER1DARRAY;
        "image1DArray", TIMAGE1DARRAY;
        "sampler2DArray", TSAMPLER2DARRAY;
        "image2DArray", TIMAGE2DARRAY;
        "samplerBuffer", TSAMPLERBUFFER;
        "imageBuffer", TIMAGEBUFFER;
        "sampler2DMS", TSAMPLER2DMS;
        "image2DMS", TIMAGE2DMS;
        "sampler2DMSArray", TSAMPLER2DMSARRAY;
        "image2DMSArray", TIMAGE2DMSARRAY;
        "samplerCubeArray", TSAMPLERCUBEARRAY;
        "imageCubeArray", TIMAGECUBEARRAY;
        "sampler1DShadow", TSAMPLER1DSHADOW;
        "sampler2DShadow", TSAMPLER2DSHADOW;
        "sampler2DRectShadow", TSAMPLER2DRECTSHADOW;
        "sampler1DArrayShadow", TSAMPLER1DARRAYSHADOW;
        "sampler2DArrayShadow", TSAMPLER2DARRAYSHADOW;
        "samplerCubeShadow", TSAMPLERCUBESHADOW;
        "samplerCubeArrayShadow", TSAMPLERCUBEARRAYSHADOW;
        "isampler1D", TISAMPLER1D;
        "iimage1D", TIIMAGE1D;
        "isampler2D", TISAMPLER2D;
        "iimage2D", TIIMAGE2D;
        "isampler3D", TISAMPLER3D;
        "iimage3D", TIIMAGE3D;
        "isamplerCube", TISAMPLERCUBE;
        "iimageCube", TIIMAGECUBE;
        "isampler2DRect", TISAMPLER2DRECT;
        "iimage2DRect", TIIMAGE2DRECT;
        "isampler1DArray", TISAMPLER1DARRAY;
        "iimage1DArray", TIIMAGE1DARRAY;
        "isampler2DArray", TISAMPLER2DARRAY;
        "iimage2DArray", TIIMAGE2DARRAY;
        "isamplerBuffer", TISAMPLERBUFFER;
        "iimageBuffer", TIIMAGEBUFFER;
        "isampler2DMS", TISAMPLER2DMS;
        "iimage2DMS", TIIMAGE2DMS;
        "isampler2DMSArray", TISAMPLER2DMSARRAY;
        "iimage2DMSArray", TIIMAGE2DMSARRAY;
        "isamplerCubeArray", TISAMPLERCUBEARRAY;
        "iimageCubeArray", TIIMAGECUBEARRAY;
        "atomic_uint", TATOMIC_UINT;
        "usampler1D", TUSAMPLER1D;
        "uimage1D", TUIMAGE1D;
        "usampler2D", TUSAMPLER2D;
        "uimage2D", TUIMAGE2D;
        "usampler3D", TUSAMPLER3D;
        "uimage3D", TUIMAGE3D;
        "usamplerCube", TUSAMPLERCUBE;
        "uimageCube", TUIMAGECUBE;
        "usampler2DRect", TUSAMPLER2DRECT;
        "uimage2DRect", TUIMAGE2DRECT;
        "usampler1DArray", TUSAMPLER1DARRAY;
        "uimage1DArray", TUIMAGE1DARRAY;
        "usampler2DArray", TUSAMPLER2DARRAY;
        "uimage2DArray", TUIMAGE2DARRAY;
        "usamplerBuffer", TUSAMPLERBUFFER;
        "uimageBuffer", TUIMAGEBUFFER;
        "usampler2DMS", TUSAMPLER2DMS;
        "uimage2DMS", TUIMAGE2DMS;
        "usampler2DMSArray", TUSAMPLER2DMSARRAY;
        "uimage2DMSArray", TUIMAGE2DMSARRAY;
        "usamplerCubeArray", TUSAMPLERCUBEARRAY;
        "uimageCubeArray", TUIMAGECUBEARRAY
    ]
    
    let keywords_assoc = [
        "struct",        STRUCT;
        "else",          ELSE;
        "for",           FOR;
        "if",            IF;
        "return",        RETURN;
        "discard",       DISCARD;
        "while",         WHILE;
        "attribute",     ATTRIBUTE;
        "const",         CONST;
        "uniform",       UNIFORM;
        "varying",       VARYING;
        "buffer",        BUFFER;
        "shared",        SHARED;
        "coherent",      COHERENT;
        "volatile",      VOLATILE;
        "restrict",      RESTRICT;
        "readonly",      READONLY;
        "writeonly",     WRITEONLY;
        "atomic_uint",   ATOMIC_UINT;
        "layout",        LAYOUT;
        "centroid",      CENTROID;
        "flat",          FLAT;
        "smooth",        SMOOTH;
        "noperspective", NOPERSPECTIVE;
        "patch",         PATCH;
        "sample",        SAMPLE;
        "break",         BREAK;
        "continue",      CONTINUE;
        "do",            DO;
        "switch",        SWITCH;
        "case",          CASE;
        "default",       DEFAULT;
        "subroutine",    SUBROUTINE;
        "in",            IN;
        "out",           OUT;
        "inout",         INOUT;
        "invariant",     INVARIANT;
        "precise",       PRECISE;
        "precision",     PRECISION;
        "lowp",          LOWP;
        "mediump",       MEDIUMP;
        "highp",         HIGHP;
    ]
    
    
    let id_or_keyword =
        let keywords_h = Hashtbl.create 97 in
        let types_h = Hashtbl.create 97 in
            List.iter (fun (s, t) -> Hashtbl.add keywords_h s t) keywords_assoc;
            List.iter (fun (s, t) -> Hashtbl.add types_h s t) types_assoc;
            fun s ->
                try
                    Hashtbl.find keywords_h s
                with Not_found -> (
                    try
                        Hashtbl.find types_h s
                    with Not_found -> (
                        if Hashtbl.mem Ast.tidentTbl s
                        then TIDENT s
                        else IDENT s
                    )
                )
    
    
    let convert_hexa c = match Char.lowercase c with
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
        | '9' -> 9
        | 'a' -> 10
        | 'b' -> 11
        | 'c' -> 12
        | 'd' -> 13
        | 'e' -> 14
        | 'f' -> 15
        |  c  -> raise (Invalid_argument ("convert_hexa : " ^ (String.make 1 c)))
    
    let fromAscii a b =
        let c = (convert_hexa a) lsl 4 + (convert_hexa b) in
            String.make 1 (Char.chr c)
    
    
    let newline lexbuf =
        let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- {
                    pos with
                    pos_lnum = pos.pos_lnum + 1;
                    pos_bol = pos.pos_cnum
                }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_')(alpha | digit | '_')*
let tident = ident

let octal_digit = ['0'-'7']
let hexa_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let uint_constant =
      '0'
    | ['1'-'9'] digit*
    | '0' octal_digit+
    | "0x" hexa_digit+

let int_constant =
      uint_constant
    | '-' uint_constant

let whitespace = [' ' '\t']+
let commentaire_simple = "//" [^'\n']* '\n'

rule token = parse
    | '\n'                { newline lexbuf ; token lexbuf}
    | whitespace          { token lexbuf }
    | uint_constant as i  { UINT_CONST i }
    | int_constant as i   { INT_CONST i }
    | "true"              { BOOL_CONST true }
    | "false"             { BOOL_CONST false }
    | ident as id         { id_or_keyword id }
    | '"'    { STRING_CONST (chaine "" lexbuf) }
    | '{'    { LBRACE }
    | '}'    { RBRACE }
    | '['    { LBRACKET }
    | ']'    { RBRACKET }
    | '('    { LPAR }
    | ')'    { RPAR }
    | '='    { ASSIGN }
    | "+="   { PLUS_ASSIGN }
    | "-="   { MINUS_ASSIGN }
    | "*="   { STAR_ASSIGN }
    | "/="   { DIV_ASSIGN }
    | "%="   { MOD_ASSIGN }
    | "<<="  { LFLOW_ASSIGN }
    | ">>="  { RFLOW_ASSIGN }
    | "&="   { BITAND_ASSIGN }
    | "^="   { BITXOR_ASSIGN }
    | "|="   { BITOR_ASSIGN }
    | "||"   { OR }
    | "^^"   { XOR }
    | "&&"   { AND }
    | "=="   { DBLEQ }
    | "!="   { NEQ }
    | '<'    { LT }
    | "<="   { LEQ }
    | '>'    { GT }
    | ">="   { GEQ }
    | '+'    { PLUS }
    | '-'    { MINUS }
    | '*'    { STAR }
    | '/'    { DIV }
    | '%'    { MOD }
    | '!'    { NOT }
    | "++"   { INCR }
    | "--"   { DECR }
    | '&'    { BITAND }
    | '|'    { BITOR }
    | '^'    { BITXOR }
    | '~'    { BITNOT }
    | '.'    { DOT }
    | ';'    { SEMCOL }
    | ':'    { COL }
    | ','    { COMMA }
    | "#include <iostream>" { IOSTREAM }
    | "std::cout"           { COUT }
    | "<<"   { LFLOW }
    | ">>"   { RFLOW }
    | "/*"   { comment lexbuf }
    | commentaire_simple { newline lexbuf ; token lexbuf }
    | eof    { EOF }
    | _ as c { raise (Error ("unexpected character : " ^ String.make 1 c)) }

and comment = parse
    | '\n'  { newline lexbuf ; comment lexbuf }
    | "*/"  { token lexbuf }
    | _     { comment lexbuf }
    | eof   { raise (Error "unexpected end of file (unterminated commentary)")}

and chaine s = parse
    | ([' '-'~'] # ['\\' '"']) as c
        { chaine (s ^ (String.make 1 c)) lexbuf }
    | "\\\\" { chaine (s ^ "\\") lexbuf }
    | "\\\"" { chaine (s ^ "\"") lexbuf }
    | "\\n"  { chaine (s ^ "\n") lexbuf }
    | "\\t"  { chaine (s ^ "\t") lexbuf }
    | "\\x" (hexa_digit as a) (hexa_digit as b)
        { chaine (s ^ (fromAscii a b)) lexbuf }
    | '"'    { s }
    | _ as c { raise (Error ("unexpected character in string : " ^ String.make 1 c)) }
    | eof    { raise (Error "unexpected end of file (unterminated string)") }

