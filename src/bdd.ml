open Expr

type dtree =
  | Node of int * dtree * dtree
  | Leaf of bool

let pair i j =
  (i + j) * (i + j + 1) / 2 + i

module HTbl = Hashtbl.Make(struct
  type t =
    int * int * int

  let equal (i, v0, v1) (j, u0, u1) =
    i = j && v0 = u0 && v1 = u1

  let hash (i, v0, v1) =
    pair i (pair v0 v1)
end)

let from_expr e =
  let e', n = rename_vars e in
  let nodes = ref [0, (0, 0, 0); 1, (0, 0, 0)] in

  let mk i l h =
    if l = h then l
    else l
  in

  let rec build t i =
    if i > n then
      if t = False then 0 else 1
      else
        let v0 = build (apply t i False) (i + 1) in
        let v1 = build (apply t i True) (i + 1) in
        mk i v0 v1
  in
  ()
