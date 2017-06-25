
type todo = int

type ast = {
    declarations : declaration list;
}

and declaration =
    | Structure_declaration of structure_declaration
    | Global_variable_declaration of global_variable_declaration
    | Function_declaration of function_declaration

and structure_declaration = {
    struct_name: string;
    fields: (type_ * string) list;
}

and global_variable_declaration = qualifiers * type_ * string

and function_declaration = {
    name: string;
    type_: type_;
    parameters: function_parameter list;
    body: statement; (* either Empty -- for prototypes -- of Bloc *)
}

and function_parameter =
    parameter_qualifier * type_ * string option * string option * expression option

and parameter_qualifier =
    | ConstInParam
    | InParam
    | OutParam
    | InOutParam

and qualifiers = storage option * auxiliary option * memory list * precision option

and storage =
    | Const
    | In
    | Out
    | Attribute
    | Uniform
    | Varying
    | Buffer
    | Shared

and auxiliary =
    | Centroid
    | Sample
    | Patch

and memory =
    | Coherent
    | Volatile
    | Restrict
    | ReadOnly
    | WriteOnly

and precision =
    | Lowp
    | Mediump
    | Highp

and statement =
    | Empty
    | Expression of expression
    | Bloc of statement list
    | Declaration of var_declaration
    | Return of expression option
    | If_else of expression * statement * statement
    | For of for_init * expression * expression list * statement
    | Switch of expression * statement
    | Case of expression
    | DefaultCase
    | Break
    | Continue

and var_declaration = type_ * (string * expression option) list

and for_init = (* Variables can be declared inside for loops initializer *)
    | ForInitExpr of expression list
    | ForInitDecl of var_declaration
    | ForInitWhile  (* means that the for loop is an un-sugar-ed while*)

and expression =
    | Integer of string
    | Unsigned of string
    | Bool of bool
    | Ident of string
    | Constructor of type_
    | Application of expression * expression list
    | PrefixUnop of prefix_unary * expression
    | PostfixUnop of postfix_unary * expression
    | Binop of operator * expression * expression
    | Assignment of assignment_operator * expression * expression

and operator =
    | Dbleq
    | Neq
    | Lt
    | Leq
    | Gt
    | Geq
    | Add
    | Sub
    | Mult
    | Div
    | Mod
    | And
    | Or
    | Xor
    | BitAnd
    | BitOr
    | BitXor
    | BitNot
    | LFlow
    | RFlow

and prefix_unary =
    | Not
    | PrefixIncr
    | PrefixDecr
    | Minus
    | Plus

and postfix_unary =
    | PostfixIncr
    | PostfixDecr

and assignment_operator =
    | Assign
    | Plus_assign
    | Minus_assign
    | Star_assign
    | Div_assign
    | Mod_assign
    | Lflow_assign
    | Rflow_assign
    | Bitand_assign
    | Bitxor_assign
    | Bitor_assign

and type_ =
    | TIdent of string


let rec join sep = function
    | t :: [] -> t
    | t :: q  -> t ^ sep ^ (join sep q)
    | [] -> ""


let string_of_type = function
    | TIdent id -> id

let string_of_structure_declaration decl =
    let beginning = "struct " ^ decl.struct_name ^ "{\n" in
        (
            List.fold_left
            (fun acc (t, id) -> acc ^ (string_of_type t) ^ " " ^ id ^ ";\n")
            beginning
            decl.fields
        ) ^ "};"

let string_of_operator = function
    | Dbleq -> "=="
    | Neq   -> "!="
    | Lt    -> "<"
    | Leq   -> "<="
    | Gt    -> ">"
    | Geq   -> ">="
    | Add   -> "+"
    | Sub   -> "-"
    | Mult  -> "*"
    | Div   -> "/"
    | Mod   -> "%"
    | And   -> "&&"
    | Or    -> "||"
    | Xor    -> "^^"
    | BitAnd -> "&"
    | BitOr  -> "|"
    | BitXor -> "^"
    | BitNot -> "~"
    | LFlow  -> "<<"
    | RFlow  -> ">>"

let string_of_prefix_unary = function
    | Not        -> "!"
    | PrefixIncr -> "++"
    | PrefixDecr -> "--"
    | Minus      -> "-"
    | Plus       -> "+"

let string_of_postfix_unary = function
    | PostfixIncr -> "++"
    | PostfixDecr -> "--"

let string_of_assignment_operator = function
    | Assign        -> "="
    | Plus_assign   -> "+="
    | Minus_assign  -> "-="
    | Star_assign   -> "*="
    | Div_assign    -> "/="
    | Mod_assign    -> "%="
    | Lflow_assign  -> "<<="
    | Rflow_assign  -> ">>="
    | Bitand_assign -> "&="
    | Bitxor_assign -> "^="
    | Bitor_assign  -> "|="

let rec string_of_expression = function
    | Integer s -> s
    | Unsigned s -> s
    | Bool true -> "true"
    | Bool false -> "false"
    | Ident s -> s
    | Constructor t -> string_of_type t
    | Application (f, args) -> (string_of_expression f) ^ "(" ^ (join ", " (List.map string_of_expression args)) ^ ")"
    | PrefixUnop (op, e) -> (string_of_prefix_unary op) ^ (string_of_expression e)
    | PostfixUnop (op, e) -> (string_of_expression e) ^ (string_of_postfix_unary op)
    | Binop (op, e1, e2) -> (string_of_expression e1) ^ " " ^ (string_of_operator op) ^ " " ^ (string_of_expression e2)
    | Assignment (op, e1, e2) -> (string_of_expression e1) ^ " " ^ (string_of_assignment_operator op) ^ " " ^ (string_of_expression e2)

let string_of_var_declaration (t, l) =
    let s = join ", " (List.map (function
        | (var, None)      -> var
        | (var, Some expr) -> var ^ " = " ^ (string_of_expression expr)
    ) l) in
        (string_of_type t) ^ " " ^ s ^ ";"

let string_of_for_init = function
    | ForInitExpr l -> join ", " (List.map string_of_expression l)
    | ForInitDecl d -> string_of_var_declaration d
    | ForInitWhile -> ""

let string_of_statement =
    let rec aux indent =
        let indent_more = indent ^ "\t" in
        let new_line = "\n" ^ indent in
        let new_line_more = "\n" ^ indent_more in
        function
        | Empty -> ";"
        | Expression e -> (string_of_expression e) ^ ";"
        | Bloc l -> "{" ^ new_line_more ^ (join new_line_more (List.map (aux indent_more) l)) ^ new_line ^ "}"
        | Declaration d -> string_of_var_declaration d
        | Return (Some expr) -> "return " ^ (string_of_expression expr) ^ ";"
        | Return None -> "return;"
        | If_else (cond, s1, Empty) ->
            let c = string_of_expression cond in
            "if (" ^ c ^ ") " ^ (aux indent s1)
        | If_else (cond, s1, s2) ->
            let c = string_of_expression cond in
            "if (" ^ c ^ ") " ^ (aux indent s1) ^ " else " ^ (aux indent s2)
        | For (ForInitWhile, cond, [], body) ->
            let c = (string_of_expression cond) in
            "while (" ^ c ^ ") " ^ (aux indent body)
        | For (init, cond, loop, body) ->
            let i = string_of_for_init init in
            let c = (string_of_expression cond) in
            let l = join ", " (List.map string_of_expression loop) in
            "for (" ^ i ^ " ; " ^ c ^ " ; " ^ l ^ ") " ^ (aux indent body)
        | Switch (e, s) -> "switch (" ^ (string_of_expression e) ^ ") " ^ (aux indent s)
        | Case e -> "case " ^ (string_of_expression e) ^ ":"
        | DefaultCase -> "default:"
        | Break -> "break;" ^ new_line
        | Continue -> "continue;"
    in aux ""

let string_of_auxiliary_option = function
    | None -> ""
    | Some Centroid    -> "centroid "
    | Some Sample      -> "sample "
    | Some Patch       -> "patch "

let string_of_storage_option = function
    | None -> ""
    | Some Const ->     "const "
    | Some In ->        "in "
    | Some Out ->       "out "
    | Some Attribute -> "attribute "
    | Some Uniform ->   "uniform "
    | Some Varying ->   "varying "
    | Some Buffer ->    "buffer "
    | Some Shared ->    "shared "

let string_of_memory = function
    | Coherent -> "coherent "
    | Volatile -> "volatile "
    | Restrict -> "restrict "
    | ReadOnly -> "readonly "
    | WriteOnly -> "writeonly "

let string_of_precision_option = function
    | None -> ""
    | Some Lowp    -> "lowp "
    | Some Mediump -> "mediump "
    | Some Highp   -> "highp "

let rec string_of_memory_list = function
    | mem :: t -> (string_of_memory mem) ^ (string_of_memory_list t)
    | [] -> ""

let string_of_qualifiers (storage, aux, mem, prec) =
    (string_of_storage_option storage) ^
    (string_of_auxiliary_option aux) ^
    (string_of_memory_list mem) ^
    (string_of_precision_option prec)

let string_of_global_variable_declaration (qualif, t, name) =
    (string_of_qualifiers qualif) ^ " " ^ (string_of_type t) ^ " " ^ name

let string_of_parameter_qualifier = function
    | ConstInParam -> "const in"
    | InParam      -> "in"
    | OutParam     -> "out"
    | InOutParam   -> "inout"

let string_of_function_declaration decl =
    let s = (string_of_type decl.type_) ^ " " ^ decl.name ^ "(" in
    let s, _ = List.fold_left
        (fun (acc, is_first) (qualif, t, arg_name, array_size, default) ->
            acc ^
            (if is_first then "" else ", ") ^
            (string_of_parameter_qualifier qualif) ^
            (string_of_type t) ^
            (
                match arg_name with
                | None -> ""
                | Some name -> " " ^ name
            ) ^
            (
                match array_size with
                | None -> ""
                | Some size -> "[" ^ size ^ "]"
            ) ^
            (
                match default with
                | None -> ""
                | Some expr -> " = " ^ (string_of_expression expr)
            ), false
        )
        (s, true) decl.parameters
    in
        s ^ ")" ^ (string_of_statement decl.body)

let string_of_declaration = function
    | Structure_declaration sd       -> string_of_structure_declaration sd
    | Global_variable_declaration gd -> string_of_global_variable_declaration gd
    | Function_declaration fd        -> string_of_function_declaration fd

let string_of_ast ast =
    let s = List.fold_left (fun acc d -> acc ^ "\n" ^ (string_of_declaration d)) "" ast.declarations in
        s ^ "\n"

let print_ast ast = print_string (string_of_ast ast)



let tidentTbl : (string, unit) Hashtbl.t = Hashtbl.create 17
