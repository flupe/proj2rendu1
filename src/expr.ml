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

let max_var e =
  let rec aux m = function
    | Var x -> max m x
    | Not e -> aux m e
    | True
    | False -> m
    | And (l, r)
    | Or (l, r)
    | Xor (l, r)
    | Implies (l, r)
    | Equiv (l, r) ->
        aux (aux m l) r
  in aux 0 e

let rename_vars e =
  let count = ref 0 in
  let vars = ref [] in
  let name id =
    try
      List.assoc id !vars
    with _ ->
      incr count;
      vars := (id, !count) :: !vars;
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
      let e' = apply e i v in
      match e' with
      | True -> False
      | False -> True
      | _ -> Not e'

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
        | False, _
        | True, True -> True
        | False, True -> False
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
        | _, _ -> Equiv(l, r)
      end

  | x -> x

