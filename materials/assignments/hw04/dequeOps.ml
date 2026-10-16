(*****************************************************************************)
(* Deque operation types and generators for property-based testing.          *)
(* Used by dequeTest.ml; does not depend on any deque implementation.        *)
(*****************************************************************************)

open Assert

let rec fold (f : 'a -> 'b -> 'a) (acc : 'a) (l : 'b list) : 'a =
  match l with
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs

type 'a op =
  | Insert_head of 'a
  | Insert_tail of 'a
  | Remove_head
  | Remove_tail

type which =
  | Insert_head
  | Insert_tail
  | Remove_head
  | Remove_tail

let specs_for (ws : which list) (gen_elem : 'a gen_spec) : 
  'a op gen_spec list =
  List.flatten
    (List.map
       (function
         | Insert_head -> [ gen_map gen_elem 
            (fun x -> (Insert_head x : 'a op)) ]
         | Insert_tail -> [ gen_map gen_elem 
            (fun x -> (Insert_tail x : 'a op)) ]
         | Remove_head -> [ gen_pure (Remove_head : 'a op) ]
         | Remove_tail -> [ gen_pure (Remove_tail : 'a op) ])
       ws)

let string_of_op (op : 'a op) : string =
  match (Obj.magic op : int op) with
  | Insert_head x -> "insert_head " ^ string_of_int x
  | Insert_tail x -> "insert_tail " ^ string_of_int x
  | Remove_head -> "remove_head"
  | Remove_tail -> "remove_tail"
