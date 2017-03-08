(****************************************************************)
(* A mutable, append-only data-structure used to store formulas *)
(*  in Conjunctive Normal Form, to convert them to the DIMACS   *)
(*     format, and to run `minisat` on  the exported file.      *)
(****************************************************************)

open Expr
open Unix

let (|.) f g =
	fun x -> g @@ f x

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

(* Returns the biggest variable used in a given formula. *)
let max_var (f : t) =
	let max_var_in_clause c =
		List.map int_of_literal c
		|> List.map abs
		|> List.fold_left max 0 in
	
	List.map max_var_in_clause (!f)
	|> List.fold_left max 0

(* Converts a given formula to the DIMACS format. *)
let dimacs (f : t) =
	let string_of_clause c =
		List.map int_of_literal c 
		|> List.map string_of_int
		|> List.fold_left (fun x y -> x ^ " " ^ y) "" in
	
	"p cnf " ^ (string_of_int @@ max_var f) ^ " " ^ (string_of_int @@ List.length !f) ^
	(List.map string_of_clause (!f) |> List.fold_left (fun x y -> x ^ "\n" ^ y) "")

(* Runs `minisat` on a given formula. *)
let minisat (f : t) =
	let oc = open_process_out "minisat" in
	output_string oc (dimacs f);
	flush oc;
	close_process_out oc

