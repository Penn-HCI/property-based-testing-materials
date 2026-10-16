(*****************************************************************************)
(* Queue operation types and generators for property-based testing.          *)
(* Used by queueTest.ml; does not depend on any queue implementation.       *)
(*****************************************************************************)

open Assert

let rec fold (f : 'a -> 'b -> 'a) (acc : 'a) (l : 'b list) : 'a =
  match l with
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs

type 'a op =
  | Enq of 'a
  | Deq

type which =
  | Enq
  | Deq

let specs_for (ws : which list) (gen_elem : 'a gen_spec) : 'a op gen_spec 
  list =
  List.flatten
    (List.map
       (function
         | Enq -> [ gen_map gen_elem (fun x -> (Enq x : 'a op)) ]
         | Deq -> [ gen_pure (Deq : 'a op) ])
       ws)

let string_of_op (op : 'a op) : string =
  match (Obj.magic op : int op) with
  | Enq x -> "enq " ^ string_of_int x
  | Deq -> "deq"
