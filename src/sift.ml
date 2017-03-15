(* See: perso.ens-lyon.fr/daniel.hirschkoff/P2/docs/r1/BDD-variable-ordering-sifting.pdf *)

open Bdd
open Hashcons


let get_levels robdd n =
  (* create an array containing every instance of each variable of the DAG *)
  (* using sets instead of hashtables, we'll see how it goes *)
  let levels = Array.make n HS.empty in

  let rec explore (node : robdd) =
    match node.value with
    | Node (v, l, h) ->
       if not (HS.mem node levels.(v - 1)) then begin
         levels.(v - 1) <- HS.add node levels.(v - 1);
         explore l;
         explore h;
       end
    | _ -> ()

  in explore robdd;
  levels


(* does swap(x_i, x_{i + 1}) *)
let swap levels i =
  let swap_node node =
    ()
  in
  HS.iter swap_node levels.(i)


let reduce_robdd =
  ()


let pomme n =
  let tableau = Array.make n 0 in
  tableau.(1) <- 3;
