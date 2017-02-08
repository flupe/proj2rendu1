type expr =
  | Const of bool
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  | Not expr
  | Implies expr * expr
  | Equiv expr * expr
