(*************************************************************)
(* A mutable data-structure to store formulas in Conjunctive *)
(*     Normal Form, to convert them to the DIMACS format,    *)
(*         and to run `minisat` on the exported file.        *)
(*************************************************************)

open Expr

type literal =
	| Pos of int
	| Neg of int

type clause = literal list
type formula = clause list
type t = formula ref

(* Creates a new formula in Conjunctive Normal Form. *)
let create () : t = 
	ref []

(* Appends a new clause to a given formula. *)
let append (f : t) c =
	f := c :: (!f)

(* Returns the integer representing a given literal. *)
let int_of_literal = function
	| Pos x -> x
	| Neg x -> (-1) * x

(* Returns the string representing a given literal. *)
let string_of_literal l =
	string_of_int @@ int_of_literal l

(* Returns the biggest variable used in a given formula. *)
let max_var (f : t) =
	let max_var_in_clause c =
		List.map int_of_literal c
		|> List.map abs
		|> List.fold_left max 0 in
	
	List.map max_var_in_clause (!f)
	|> List.fold_left max 0

(* Pretty-prints a given clause. *)
let print_clause (c : clause) =
	let rec aux = function
		| x :: [] -> print_string @@ string_of_literal x
		| x :: xs -> print_string @@ string_of_literal x; print_string " \\/ "; aux xs
		| [] -> () in
	print_string "(";
	aux c;
	print_string ")"

(* Pretty-prints a given formula. *)
let print (f : t) =
	let rec aux = function
		| x :: [] -> print_clause x
		| x :: xs -> print_clause x; print_string " /\\ "; aux xs 
		| [] -> () in
	aux (!f)

(* Converts a given formula to the DIMACS format. *)
let dimacs (f : t) =
	let string_of_clause c =
		let lits = List.map int_of_literal c 
				|> List.map string_of_int in
		(List.fold_right (fun x y -> x ^ " " ^ y)  lits "") ^ "0" in
	
	"p cnf " ^ (string_of_int @@ max_var f) ^ " " ^ (string_of_int @@ List.length !f) ^
	(List.map string_of_clause (!f) |> List.fold_left (fun x y -> x ^ "\n" ^ y) "")

(* Runs `minisat` on a given formula, and returns either None if the
   formula isn't satisfiable, or a Hashtable containing a possible
   assignation for it otherwise. *)
let minisat (f : t) =
	let in_file, ic = Filename.open_temp_file "minisat" "input" in
	let out_file, oc = Filename.open_temp_file "minisat" "output" in
	output_string ic (dimacs f);
	flush ic;

	let _ = Sys.command @@ "minisat " ^ in_file ^ " " ^ out_file ^ " > /dev/null" in
	let result = open_in out_file in

	if input_line result <> "SAT" then
		None
	else begin
		let assign = Hashtbl.create (max_var f) in
		
		input_line result
		|> Str.split (Str.regexp "[ \t]+")
		|> List.map int_of_string
		|> List.iter (function i ->
			if i > 0 then 
				Hashtbl.add assign i true 
			else if i < 0 then
				Hashtbl.add assign ((-1) * i) false);

		Some assign
	end
