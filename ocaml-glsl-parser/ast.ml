
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

and global_variable_declaration =
	| Global_variable of string

and function_declaration = {
	name: string;
	type_: type_;
	statements: statements list;
}

and statements = int

and type_ =
	| Void
	| Int
	| TIdent of string



let string_of_type = function
	| Void -> "void"
	| Int -> "int"
	| TIdent id -> id

let string_of_structure_declaration decl =
	let beginning = "struct " ^ decl.struct_name ^ "{\n" in
		(
			List.fold_left
			(fun acc (t, id) -> acc ^ (string_of_type t) ^ " " ^ id ^ ";\n")
			beginning
			decl.fields
		) ^ "};"

let string_of_global_variable_declaration = function
	| Global_variable s -> "Global_variable " ^ s

let string_of_statement = string_of_int

let string_of_function_declaration decl =
	let beginning = (string_of_type decl.type_) ^ " " ^ decl.name ^ "() {\n" in
		List.fold_left (fun acc s -> acc ^ ";\n" ^ (string_of_statement s)) beginning decl.statements

let string_of_declaration = function
	| Structure_declaration sd       -> string_of_structure_declaration sd
	| Global_variable_declaration gd -> string_of_global_variable_declaration gd
	| Function_declaration fd        -> string_of_function_declaration fd

let string_of_ast ast = List.fold_left (fun acc d -> acc ^ "\n" ^ (string_of_declaration d)) "" ast.declarations

let print_ast ast = print_string (string_of_ast ast)



let tidentTbl : (string, unit) Hashtbl.t = Hashtbl.create 17
