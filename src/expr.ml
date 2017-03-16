type expr =
  | True | False
  | Var of int
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  | Not of expr
  | Implies of expr * expr
  | Equiv of expr * expr

let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | Var x -> string_of_int x
  | And (l, r) -> "(" ^ string_of_expr l ^ ") /\\ (" ^ string_of_expr r ^ ")"
  | Or (l, r) -> "(" ^ string_of_expr l ^ ") \\/ (" ^ string_of_expr r ^ ")"
  | Xor (l, r) -> "(" ^ string_of_expr l ^ ") X (" ^ string_of_expr r ^ ")"
  | Not e -> "~(" ^ string_of_expr e ^ ")"
  | Implies (l, r) -> "(" ^ string_of_expr l ^ ") => (" ^ string_of_expr r ^ ")"
  | Equiv (l, r) -> "(" ^ string_of_expr l ^ ") <=> (" ^ string_of_expr r ^ ")"

let rename_vars e =
  let count = ref 0 in
  let vars = Hashtbl.create 9 in
  let name id =
    try
      Hashtbl.find vars id
    with _ ->
      incr count;
      Hashtbl.add vars id !count;
      !count
  in
  let rec aux = function
    | Var x -> Var (name x)
    | Not e -> Not (aux e)
    | And (l, r) -> And (aux l, aux r)
    | Or (l, r) -> Or (aux l, aux r)
    | Xor (l, r) -> Xor (aux l, aux r)
    | Implies (l, r) -> Implies (aux l, aux r)
    | Equiv (l, r) -> Equiv (aux l, aux r)
    | x -> x
  in
  aux e, !count

let rec apply e i v = match e with
  | Var x when x = i -> v

  | Not e ->
      let e' = apply e i v in begin
        match e' with
        | True -> False
        | False -> True
        | _ -> Not e'
      end

  | And (l, r) ->
      let l' = apply l i v in
      let r' = apply r i v in begin
        match l', r' with
        | False, _ | _, False -> False
        | True, True -> True
        | _, _ -> And (l', r')
      end

  | Or (l, r) ->
      let l' = apply l i v in
      let r' = apply r i v in begin
        match l', r' with
        | True, _ | _, True -> True
        | False, False -> False
        | _, _ -> Or (l', r')
      end

  | Xor (l, r) ->
      let l' = apply l i v in
      let r' = apply r i v in begin
        match l', r' with
        | False, True
        | True, False -> True
        | True, True
        | False, False -> False
        | _, _ -> Xor (l', r')
      end

  | Implies (l, r) ->
      let l' = apply l i v in
      let r' = apply r i v in begin
        match l', r' with
        | True, False -> False
        | False, _
        | True, True -> True
        | _, _ -> Implies (l', r')
      end

  | Equiv (l, r) ->
      let l' = apply l i v in
      let r' = apply r i v in begin
        match l', r' with
        | False, False
        | True, True -> True
        | True, False
        | False, True -> False
        | _, _ -> Equiv (l', r')
      end

  | x -> x

(* Simplifies the given expression to remove unnecessary 
   True and False litterals. *)
let rec simplify = function
  | And (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    begin match e1', e2' with
      | False, _
      | _, False -> False
      | True, _ -> e2'
      | _, True -> e1'
      | _ -> And (e1', e2')
    end
  | Or (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    begin match e1', e2' with
      | True, _
      | _, True -> True
      | False, _ -> e2'
      | _, False -> e1'
      | _ -> Or (e1', e2')
    end
  | Xor (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    begin match e1', e2' with
      | True, True -> False
      | True, _
      | _, True -> True
      | False, _ -> e2'
      | _, False -> e1'
      | _ -> Xor (e1', e2')
    end
  | Not e ->
    let e' = simplify e in
    begin match e' with
      | True -> False
      | False -> True
      | _ -> Not e'
    end
  | Implies (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    begin match e1', e2' with
      | True, False -> False
      | False, True
      | False, False
      | True, True -> True
      | _, False -> Not (e1')
      | _, _ -> Implies (e1', e2')
    end
  | Equiv (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    begin match e1', e2' with
      | False, False
      | True, True -> True
      | True, False
      | False, True -> False
      | _, _ -> Equiv (e1', e2')
    end
  | e -> e