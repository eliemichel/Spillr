{
    open Lexing
    open Parser
    
    exception Error of string
    
    
    let keywords_assoc = [
        "class",   CLASS;
        "struct",   STRUCT;
        "else",    ELSE;
        "false",   FALSE;
        "for",     FOR;
        "if",      IF;
        "int",     INT;
        "new",     NEW;
        "NULL",    NULL;
        "public",  PUBLIC;
        "return",  RETURN;
        "this",    THIS;
        "true",    TRUE;
        "virtual", VIRTUAL;
        "void",    VOID;
        "while",   WHILE
    ]
    
    
    let id_or_keyword =
        let h = Hashtbl.create 97 in
            List.iter (fun (s, t) -> Hashtbl.add h s t) keywords_assoc;
            fun s ->
                try
                    Hashtbl.find h s
                with Not_found -> (
                    if Hashtbl.mem Ast.tidentTbl s
                    then TIDENT s
                    else IDENT s
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

let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_')(alpha | chiffre | '_')*
let tident = ident

let chiffre_octal = ['0'-'7']
let chiffre_hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let entier =
      '0'
    | ['1'-'9'] chiffre*
    | '0' chiffre_octal+
    | "0x" chiffre_hexa+

let whitespace = [' ' '\t']+
let commentaire_simple = "//" [^'\n']* '\n'

rule token = parse
    | '\n'                      { newline lexbuf ; token lexbuf}
    | whitespace                { token lexbuf }
    | ident as id               { id_or_keyword id }
    | entier as n               { INTEGER n }
    | '"'    { STRING (chaine "" lexbuf) }
    | '{'    { LBRACE }
    | '}'    { RBRACE }
    | '('    { LPAR }
    | ')'    { RPAR }
    | '='    { EQ }
    | "||"   { OR }
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
    | '&'    { AMP }
    | "->"   { ARROW }
    | '.'    { DOT }
    | ';'    { SEMCOL }
    | ':'    { COL }
    | ','    { COMMA }
    | "#include <iostream>" { IOSTREAM }
    | "std::cout"           { COUT }
    | "<<"   { FLOW }
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
    | "\\x" (chiffre_hexa as a) (chiffre_hexa as b)
        { chaine (s ^ (fromAscii a b)) lexbuf }
    | '"'    { s }
    | _ as c { raise (Error ("unexpected character in string : " ^ String.make 1 c)) }
    | eof    { raise (Error "unexpected end of file (unterminated string)") }

