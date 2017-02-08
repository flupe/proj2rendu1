type expr =
  | Var of int
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  | Not of expr
  | Implies of expr * expr
  | Equiv of expr * expr

let rec string_of_expr = function
  | Var x -> string_of_int x
  | And (l, r) -> "(" ^ string_of_expr l ^ ") /\\ (" ^ string_of_expr r ^ ")"
  | Or (l, r) -> "(" ^ string_of_expr l ^ ") \\/ (" ^ string_of_expr r ^ ")"
  | Xor (l, r) -> "(" ^ string_of_expr l ^ ") X (" ^ string_of_expr r ^ ")"
  | Not e -> "~(" ^ string_of_expr e ^ ")" 
  | Implies (l, r) -> "(" ^ string_of_expr l ^ ") => (" ^ string_of_expr r ^ ")"
  | Equiv (l, r) -> "(" ^ string_of_expr l ^ ") <=> (" ^ string_of_expr r ^ ")"

