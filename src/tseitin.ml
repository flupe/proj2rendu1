(*********************************************************)
(* An implementation of the Tseitin transform which uses *)
(*               Hash-consing on every step.             *)
(*********************************************************)

open Expr
open Cnf

(* Applies the Tseitin transform on a given expression. *)
let transform e =
	let e', count = rename_vars e in
	
	(* The smallest unused variable name. *)
	let smallest = ref count in
	let cnf = create () in

	let next_var () =
		let next = !smallest in
		incr smallest;
		next in

	(* Applies the transform to all the sub-expressions of e,
	   and returns the variable name that was chosen for e. *)
	let rec aux = function
		| True | False ->
			failwith "True and False not supported."
		| Var x ->
			let c = next_var () in
			append cnf [Neg c; Pos x];
			append cnf [Pos c; Neg x]; c
		| Not p1 ->
			let c, i = next_var (), aux p1 in
			append cnf [Neg c; Neg i];
			append cnf [Pos c; Pos i]; c
		| And (p1, p2) ->
			let c, i, j = next_var (), aux p1, aux p2 in
			append cnf [Neg c; Pos i];
			append cnf [Neg c; Pos j];
			append cnf [Pos c; Neg i; Neg j]; c
		| Or (p1, p2) ->
			let c, i, j = next_var (), aux p1, aux p2 in
			append cnf [Neg c; Pos i; Pos j];
			append cnf [Pos c; Neg i];
			append cnf [Pos c; Neg j]; c
		| Xor (p1, p2) ->
			aux @@ And (Or (p1, p2), Not (And (p1, p2)))
		| Implies (p1, p2) ->
			aux @@ Or (Not p1, p2)
		| Equiv (p1, p2) ->
			aux @@ And (Implies (p1, p2), Implies (p2, p1)) in
	
	let _ = aux e in
	cnf