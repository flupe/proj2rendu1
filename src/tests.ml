open Expr

let pair i j =
  (i + j) * (i + j + 1) / 2 + i

let range p =
  let rec aux acc p =
    if p = 0 then acc
    else aux (p :: acc) (p - 1)
  in aux [] p

let parity n = 
  Not(List.fold_left (fun a b -> Xor(a, Var(b))) (Var(1)) (List.tl @@ range n))

let drawers n =
  let range_n = range n in
  let range_np = (n + 1) :: range_n in
  (* could unfold this in for loops *)
  let left = List.fold_left (fun prev p ->
      And (prev, List.fold_left (fun prev' t ->
        Or (prev', Var(pair p t))
      ) False range_n)
    ) True range_np
  in
  let right = ref False in
  for t = 1 to n do
    for p = 1 to n + 1 do
      for q = p + 1 to n + 1 do
        right := Or(!right, And(Var(pair p t), Var(pair q t)))
      done
    done
  done;
  Implies(left, !right)

(* Generates a BDD from the expression, and compares it to the
   output of `minisat` on the same expression. *)
let compare expr =
  let bdd, _, _ = Bdd.from_expr expr in
  let cnf = Tseitin.transform expr in

  Bdd.display bdd;

  match Cnf.minisat cnf with
    (* The generated BDD should only have False leaves. *)
    | None -> Bdd.for_all_leaves false bdd

    (* The given assignation should lead to a True leave in the BDD. *)
    | Some assign -> Bdd.resolve (Hashtbl.find assign) bdd

let run () =
  for n = 1 to 5 do
    print_string @@ "Testing `drawers` for n = " ^ string_of_int n ^ "... ";
    flush_all ();
    if compare @@ drawers n then
      print_endline "Passed!"
    else
      print_endline "Failed."
  done;

  if compare @@ parity 10 then
    print_endline "parité check"
  else
    print_endline "nothing pair in my name"
