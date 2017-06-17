
type declaration =
	| Ident of string

type ast = {
	declarations : declaration list;
}

let string_of_declaration = function
	| Ident s -> "Ident " ^ s

let string_of_ast ast = List.fold_left (fun acc d -> acc ^ "\n" ^ (string_of_declaration d)) "" ast.declarations

let print_ast ast = print_string (string_of_ast ast)



let tidentTbl : (string, unit) Hashtbl.t = Hashtbl.create 17
