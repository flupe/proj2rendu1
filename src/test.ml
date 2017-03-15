open Expr

let pair i j =
  (i + j) * (i + j + 1) / 2 + i

let range p =
  let rec aux acc p =
    if p = 0 then acc
    else aux (p :: acc) (p - 1)
  in aux [] p

let drawers n =
  let range_n = range n in
  let range_np = (n + 1) :: range_n in

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
