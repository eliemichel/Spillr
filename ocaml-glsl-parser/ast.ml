
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

and global_variable_declaration =
	| Global_variable of string

and function_declaration = {
	name: string;
	type_: type_;
	arguments: function_argument list;
	statements: statements list;
	is_prototype: bool;
}

and function_argument =
	storage option * auxiliary option * type_ * string option * string option * expression option

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

and statements = todo

and expression = todo

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

let string_of_expression = string_of_int

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

let string_of_function_declaration decl =
	let s = (string_of_type decl.type_) ^ " " ^ decl.name ^ "(" in
	let s, _ = List.fold_left
		(fun (acc, is_first) (storage, aux, t, arg_name, array_size, default) ->
			acc ^
			(if is_first then "" else ", ") ^
			(string_of_storage_option storage) ^
			(string_of_auxiliary_option aux) ^
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
		(s, true) decl.arguments
	in
	let s = s ^ ")" in
	if decl.is_prototype
	then
		s ^ ";"
	else
		let s = s ^ " {\n" in
		let s = List.fold_left (fun acc s -> acc ^ (string_of_statement s) ^ ";\n") s decl.statements
		in
			s ^ "}"

let string_of_declaration = function
	| Structure_declaration sd       -> string_of_structure_declaration sd
	| Global_variable_declaration gd -> string_of_global_variable_declaration gd
	| Function_declaration fd        -> string_of_function_declaration fd

let string_of_ast ast =
	let s = List.fold_left (fun acc d -> acc ^ "\n" ^ (string_of_declaration d)) "" ast.declarations in
		s ^ "\n"

let print_ast ast = print_string (string_of_ast ast)



let tidentTbl : (string, unit) Hashtbl.t = Hashtbl.create 17
