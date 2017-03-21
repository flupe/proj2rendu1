open Expr

let range ?start:(s = 0) p =
  let rec aux acc p =
    if p = s then acc
    else aux (p :: acc) (p - 1)
  in aux [] p

let pair i j =
  (i + j) * (i + j + 1) / 2 + i

let parity n = 
  Not(List.fold_left (fun a b -> Xor(a, Var(b))) (Var(1)) (List.tl @@ range n))

(* Variables 1 .. n are the input values, n+1 .. 2n are the output values. *)
let rotation n =
  List.fold_left (fun a b -> And(a, Equiv (Var b, Var (n + b + 1)))) (Equiv (Var n, Var (n + 1))) (range @@ n - 1)

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

(* Generates a random formula with a given maximum depth,
   maximum number of variables, and an integer in 0 .. 100
   which is the probability of having a variable "in the middle". *)
let random max_depth max_vars p_vars =
  Random.self_init ();

  let rec aux d =
    if d >= max_depth || Random.int 100 < p_vars then
      Var (Random.int max_vars + 1)
    else match Random.int 6 with
      | 0 -> Not (aux @@ d + 1)
      | 1 -> And (aux @@ d + 1, aux @@ d + 1)
      | 2 -> Or (aux @@ d + 1, aux @@ d + 1)
      | 3 -> Xor (aux @@ d + 1, aux @@ d + 1)
      | 4 -> Implies (aux @@ d + 1, aux @@ d + 1)
      | 5 -> Equiv (aux @@ d + 1, aux @@ d + 1)
      | _ -> failwith "Random number generator issue."

  in aux 0

(* Input:     a = x_n  .. x_1,
              b = x_2n .. x_(n+1),
   Output:    o = x_3n .. x_(2n+1)
   Remainder: r = x_4n .. x_(3n+1) *)
let adder n =
  let half_adder i =
    And (
      Equiv (Var (2 * n + i), Xor (Xor (
        Var i, 
        Var (n + i)), 
        Var (3 * n + i))),

      Equiv (Var (3 * n + i + 1), Or (
        And (Var i, Var (n + i)),
        And (Var (3 * n + i), Xor (Var i, Var (n + i))))))

  in And (And (
    Not (Var (3 * n + 1)),  (* No input remainder *)
    Not (Var (4 * n + 1))), (* No trailing remainder *)
    List.fold_left
      (fun x i -> And (x, half_adder i)) 
      True
      (range n))

(* Generates a BDD from the expression, and compares it to the
   output of `minisat` on the same expression. *)
let compare expr =
  let bdd, _, _ = Bdd.from_expr expr in
  let cnf = Tseitin.transform expr in

  match Cnf.minisat cnf with
    (* The generated BDD should only have False leaves. *)
    | None -> Bdd.for_all_leaves false bdd

    (* The given assignation should lead to a True leave in the BDD. *)
    | Some assign -> Bdd.resolve (Hashtbl.find assign) bdd

let run () =
  for n = 15 to 20 do
    print_string @@ "Testing `parity` for n = " ^ string_of_int n ^ "... ";
    flush_all ();
    if compare @@ parity n then
      print_endline "Passed!"
    else
      print_endline "Failed."
  done;

  for n = 3 to 7 do
    print_string @@ "Testing `adder` for n = " ^ string_of_int n ^ "... ";
    flush_all ();
    if compare @@ adder n then
      print_endline "Passed!"
    else
      print_endline "Failed."
  done;

  for n = 10 to 15 do
    print_string @@ "Testing `rotation` for n = " ^ string_of_int n ^ "... ";
    flush_all ();
    if compare @@ rotation n then
      print_endline "Passed!"
    else
      print_endline "Failed."
  done;

  for n = 1 to 5 do
    print_string @@ "Testing `drawers` for n = " ^ string_of_int n ^ "... ";
    flush_all ();
    if compare @@ drawers n then
      print_endline "Passed!"
    else
      print_endline "Failed."
  done;

  for n = 5 to 10 do
    print_string @@ "Testing `random` for n = " ^ string_of_int n ^ "... ";
    flush_all ();
    if compare @@ random n 30 20 then
      print_endline "Passed!"
    else
      print_endline "Failed."
  done;