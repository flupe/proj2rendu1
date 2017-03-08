(* See: https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf *)

type +'a hash_consed =
  { value : 'a
  ; tag : int
  ; hash_key : int
  }


module type HashedType = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end


module type S = sig
  type t
  type data
  val fold : ('a -> data hash_consed -> 'a) -> 'a -> t -> 'a
  val create : int -> t
  val hashcons : t -> data -> data hash_consed
  val count  : t -> int
end


module Make (H : HashedType) : (S with type data = H.t) = struct
  type data = H.t


  type t =
    { mutable table : H.t hash_consed Weak.t array
    ; mutable size : int (* the amount of values we can store *)
    ; mutable next_tag : int
    }


  let next_tag table =
    let tag = table.next_tag in begin
      table.next_tag <- tag + 1;
      tag
    end


  let empty_bucket = Weak.create 0


  let create size =
    { table = Array.make size empty_bucket
    ; size = 0
    ; next_tag = 0
    }


  let iter f t =
    let rec fold_bucket i bucket =
      if i < Weak.length bucket then
        match Weak.get bucket i with
        | Some v -> f v; fold_bucket (i + 1) bucket
        | None -> fold_bucket (i + 1) bucket
    in
    Array.iter (fold_bucket 0) t.table


  (* left fold *)
  let fold f init t =
    let rec fold_bucket i acc bucket =
      if i >= Weak.length bucket then acc
      else
        match Weak.get bucket i with
        | Some v -> fold_bucket (i + 1) (f acc v) bucket
        | None -> fold_bucket (i + 1) acc bucket
    in
    Array.fold_left (fold_bucket 0) init t.table


  let next_size n =
    min (3 * n / 2 + 3) (Sys.max_array_length - 1)


  (* does not trigger resize *)
  let add t d =
    (* we do not recompute the hash key *)
    let index = d.hash_key mod (Array.length t.table) in
    let bucket = t.table.(index) in
    let bucket_size = Weak.length bucket in
    let rec loop i =
      if i < bucket_size then begin
        if Weak.check bucket i
        then loop (i + 1)
        else Weak.set bucket i (Some d)
      end
      else begin
        let new_size = min (bucket_size + 3) (Sys.max_array_length - 1) in
        if new_size <= bucket_size then
          failwith "Hashcons.Make: hash bucket cannot grow more";

        let new_bucket = Weak.create new_size in

        Weak.blit bucket 0 new_bucket 0 bucket_size;
        Weak.set new_bucket i (Some d);

        t.table.(index) <- new_bucket;
        t.size <- t.size + (new_size - bucket_size);
      end
    in
    loop 0


  let resize t =
    let old_length = Array.length t.table in
    let new_length = next_size old_length in

    if new_length > old_length then begin
      let new_table = create new_length in
      t.table <- new_table.table;
      iter (fun v -> add new_table v) t;
      t.size <- new_table.size;
    end


  let hashcons t v =
    let hash_key = H.hash v land max_int in
    let index = hash_key mod (Array.length t.table) in
    let bucket = t.table.(index) in
    let bucket_size = Weak.length bucket in

    let rec loop i =
      if i < bucket_size then begin
        (* used to not hijack garbage collection *)
        match Weak.get_copy bucket i with
        | Some d when d.value = v ->
            begin match Weak.get bucket i with
              | Some d -> d
              | _ -> loop (i + 1)
            end
        | _ -> loop (i + 1)
      end
      else begin
        let node =
          { hash_key = hash_key
          ; tag = next_tag t
          ; value = v
          }
        in
        add t node;

        (* check if we should resize the table *)
        (* if we have an average > 3 elements per bucket *)
        if t.size > 3 * (Array.length t.table) then
          resize t;

        node
      end
    in
    loop 0


  let count =
    fold (fun count _ -> count + 1) 0
end

