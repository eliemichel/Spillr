{
    open Lexing
    open Parser
    
    exception Error of string

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

let line_separator =
      ['\n' '\r']
    | '\n'
let whitespace = [' ' '\t']+
let commentaire_simple = "//" [^'\n']* line_separator

rule token = parse
    | line_separator      { newline lexbuf ; token lexbuf}
    | whitespace          { token lexbuf }
    | uint_constant as i  { UINT_CONST i }
    | int_constant as i   { INT_CONST i }
    | "true"              { BOOL_CONST true }
    | "false"             { BOOL_CONST false }
    | ident as id         { Lexemes.id_or_keyword id }
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
