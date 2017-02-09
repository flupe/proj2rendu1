open Expr

let pair i j =
  (i + j) * (i + j + 1) / 2 + i

module HTbl = Hashtbl.Make(struct
  type t =
    int * int * int

  let equal = (=)

  let hash (i, v0, v1) =
    pair i (pair v0 v1)
end)

let from_expr e =
  let e', n = rename_vars e in
  let nodes = ref [0, (0, 0, 0); 1, (0, 0, 0)] in
  let table = HTbl.create n in
  let k = ref 1 in

  let add i l h =
    incr k;
    nodes := (!k, (i, l, h)) :: !nodes;
    !k
  in

  let mk i l h =
    if l = h then l
    else if HTbl.mem table (i, l, h) then
      HTbl.find table (i, l, h)
    else let u = add i l h in begin
      HTbl.add table (i, h, l) u;
      u
    end
  in

  let rec build t i =
    if i > n then
      if t = False then 0 else 1
      else
        let v0 = build (apply t i False) (i + 1) in
        let v1 = build (apply t i True) (i + 1) in
        mk i v0 v1
  in

  begin
    ignore @@ build e 1;
    !nodes
  end
