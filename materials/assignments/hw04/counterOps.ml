(*****************************************************************************)
(* Counter operation types and generators for property-based testing.        *)
(* Used for testing counter implementations; does not depend on any          *)
(* counter implementation.                                                   *)
(*****************************************************************************)

open Assert

(* Left fold: same semantics as List.fold_left *)
let rec fold (f : 'a -> 'b -> 'a) (acc : 'a) (l : 'b list) : 'a =
  match l with
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs

type 'a op =
  | Increment
  | Decrement

type which =
  | Increment
  | Decrement

let specs_for (which : which list) (_gen_elem : 'a gen_spec) 
  : 'a op gen_spec list =
  List.flatten
    (List.map
       (function
         | Increment -> [ gen_pure (Increment : 'a op) ]
         | Decrement -> [ gen_pure (Decrement : 'a op) ])
       which)

let string_of_op (op : 'a op) : string =
  match (Obj.magic op : unit op) with
  | Increment -> "increment"
  | Decrement -> "decrement"
