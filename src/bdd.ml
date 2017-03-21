open Hashcons

type robdd = bdd hash_consed
and bdd
  = True
  | False
  | Node of int * robdd * robdd

module HashedBdd = struct
  type t = bdd

  let equal x y = match x, y with
    | True, True
    | False, False -> true
    | Node (v1, l1, h1), Node (v2, l2, h2) ->
        v1 = v2 && l1 == l2 && h1 == h2
    | _ -> false

  let hash = function
    | False -> 0
    | True -> 1
    | Node (v, l, h) ->
        19 * (19 * l.tag + h.tag) + v
end

module OrderedRobdd = struct
  type t = robdd

  let compare a b =
    Pervasives.compare a.tag b.tag
end

module HC = Hashcons.Make(HashedBdd)
(* ideally we would implement optimized Sets for hash_consed items *)
(* as suggested by the paper (Patricia trees and such) but eh *)
module HS = Set.Make(OrderedRobdd)


(* from_expr : Expr.expr -> robdd * int * int *)
let from_expr e =
  let e', _, n = Expr.rename_vars e in
  let memory = HC.create 251 in
  let hashcons = HC.hashcons memory in

  let _true = hashcons True in
  let _false = hashcons False in

  let mk v l h =
    if l == h then l
    else hashcons (Node(v, l, h))
  in

  let rec build t i =
    if i > n then
      if t = Expr.True then _true else _false
    else
      let l = build (Expr.apply t i Expr.False) (i + 1) in
      let h = build (Expr.apply t i Expr.True) (i + 1) in
      mk i l h
  in
  build e' 1, n, HC.count memory


(* temporary impl *)
let display robdd =
  let low_edges = ref [] in
  let high_edges = ref [] in
  let nodes = ref [] in

  let add_node tag name =
    nodes := (string_of_int tag ^ " [label=\"" ^ name ^ "\"];\n") :: !nodes
  in

  let edge a b =
    string_of_int a.tag ^ " -> " ^ string_of_int b.tag ^ ";\n"
  in

  let rec explore node acc =
    if not (HS.mem node acc) then begin
      let set = HS.add node acc in
      match node.value with
      | True -> add_node node.tag "true"; set
      | False -> add_node node.tag "false"; set
      | Node (v, l, h) ->
        add_node node.tag (string_of_int v);
        low_edges := edge node l :: !low_edges;
        high_edges := edge node h :: !high_edges;
        set
        |> explore l
        |> explore h
    end
    else acc
  in
  ignore @@ explore robdd HS.empty;

  let out = open_out "/tmp/graph.dot" in begin
    output_string out "digraph G {\n";
    List.iter (output_string out) !nodes;
    List.iter (output_string out) !high_edges;
    output_string out "edge [style=dotted];\n";
    List.iter (output_string out) !low_edges;
    output_string out "}";
    close_out out;
  end;

  ignore @@ Sys.command "dot -Tpdf /tmp/graph.dot -o /tmp/graph.pdf";
  ignore @@ Sys.command "xdg-open /tmp/graph.pdf"

(* resolve : (int -> bool) -> robdd -> bool *)
(* Returns the value of the leaf for a given assignation. *)
let resolve f =
  let rec aux r = match r.value with
    | Node (i, r1, r2) ->
        if f i then aux r2 else aux r1
    | True -> true
    | False -> false in
  aux

exception NegativeTest

(* for_all_leaves : bool -> robdd -> bool *)
(* Checks that every leaf is either true or false. *)
let for_all_leaves b r =
  let rec aux r = match r.value with
    | Node (i, r1, r2) -> aux r1; aux r2 
    | True -> if not b then raise NegativeTest
    | False -> if b then raise NegativeTest in
  try 
    aux r;
    true
  with NegativeTest ->
    false