module Set : sig
  type 'a t
  val empty : 'a t
  val singleton : 'a -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val infer : 'a t -> 'a t -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val mem : 'a -> 'a t -> bool
end = struct
  type 'a t = 'a list
  let empty = []
  let singleton a = [a]
  (* 要素が集合に入っているか *)
  let rec mem ele set = match set with
      [] -> false
    | first :: rest -> if ele = first then true else mem ele rest
  (* 和集合 *)
  let union set1 set2 =
    List.fold_right (fun ele set ->
                       if mem ele set then set else ele :: set)
                    set1
                    set2
  (* 共通部分 *)
  let infer set1 set2 =
    List.fold_right (fun ele set ->
                       if mem ele set2 then ele :: set else set)
                    set1
                    []
  (* 差集合 (set1 - set2) *)
  let diff set1 set2 =
    List.fold_right (fun ele set ->
                       if mem ele set2 then set else ele :: set)
                     set1
                     []
end
