type expr =
  | Var of int
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  | Not of expr
  | Implies of expr * expr
  | Equiv of expr * expr
