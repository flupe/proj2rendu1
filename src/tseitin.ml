(*********************************************************)
(* An implementation of the Tseitin transform which uses *)
(*               Hash-consing on every step.             *)
(*********************************************************)

open Expr
open Cnf

(* Applies the Tseitin transform on a given expression. *)
let transform e =
	let e', mapping, count = rename_vars @@ simplify e in

	(* The smallest unused variable name. *)
	let smallest = ref count in
	let cnf = Cnf.create () in
	let cache = Hashtbl.create 1000 in

	let next_var () =
		incr smallest;
		!smallest in

	(* Applies the transform to all the sub-expressions of e,
	   and returns the variable name that was chosen for e. *)
	let rec proxy e =
		(* Before calling aux for e, we check if we haven't already
		   encountered an equivalent expression before, in which case
		   we reuse its variable name instead of assigning a new one. *)
		if Hashtbl.mem cache e then
			Hashtbl.find cache e
		else begin
			let c = aux e in
			Hashtbl.add cache e c; c
		end
		
	and aux = function
		| True | False ->
			failwith "True and False not supported."
		| Var x -> x
		| Not p1 ->
			let c, i = next_var (), proxy p1 in
			append cnf [Neg c; Neg i];
			append cnf [Pos c; Pos i]; c
		| And (p1, p2) ->
			let c, i, j = next_var (), proxy p1, proxy p2 in
			append cnf [Neg c; Pos i];
			append cnf [Neg c; Pos j];
			append cnf [Pos c; Neg i; Neg j]; c
		| Or (p1, p2) ->
			let c, i, j = next_var (), proxy p1, proxy p2 in
			append cnf [Neg c; Pos i; Pos j];
			append cnf [Pos c; Neg i];
			append cnf [Pos c; Neg j]; c
		| Xor (p1, p2) ->
			proxy @@ And (Or (p1, p2), Not (And (p1, p2)))
		| Implies (p1, p2) ->
			proxy @@ Or (Not p1, p2)
		| Equiv (p1, p2) ->
			proxy @@ And (Implies (p1, p2), Implies (p2, p1)) in
	
	append cnf [Pos (proxy e')];
	cnf