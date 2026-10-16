(*****************************************************************************)
(* PROBLEM 7: WRITING TEST CASES                                             *)
(*****************************************************************************)

;; open Assert
;; open Deque
;; open DequeOps

(* `DequeTest` is used to test the deque implementation from deque.ml.

   Read through the module, then write your test cases in the space
   provided below. This problem will be graded in the same way as Problem 4. *)

(* ------------------------------------------------------------------------ *)
(* PBT INFRASTRUCTURE (skip to "TESTING DEQUEUES")                          *)
(* ------------------------------------------------------------------------ *)

(* This section provides property-based testing infrastructure for deques.
   dequeOps.ml also contains infrastructure code for PBT. Don't modify
   or worry about the code in this section and dequeOps.ml. *)

module DequeOpGen = MakeOpGen (DequeOps)
type 'a op = 'a DequeOps.op
let gen_ops : int op list gen_spec =
  DequeOpGen.gen_ops
    [ Insert_head; Insert_head; Insert_tail; Insert_tail; 
      Remove_head; Remove_tail ]
    gen_int

let apply_op (op : int op) (d : int deque) : int option = match op with
  | Insert_head x -> insert_head x d; None
  | Insert_tail x -> insert_tail x d; None
  | Remove_head -> (try Some (remove_head d) with _ -> None)
  | Remove_tail -> (try Some (remove_tail d) with _ -> None)

let string_of_ops (ops : int op list) : string = 
  string_of_list string_of_op ops

(* `init_with ops` builds a random deque state by applying `ops` to a new
   deque `create ()`. *)
let init_with (ops : int op list) : int deque =
  fold (fun d op -> ignore (apply_op op d); d) (create ()) ops

(* We first redefine local versions of the `run_test` and `run_failing_test`
   functions so that they prepend the name of the implementation we're
   testing to the test description. *)
let run_test desc = Assert.run_test ("Deque: " ^ desc)
let run_failing_test desc = Assert.run_failing_test ("Deque: " ^ desc)
let run_property_test desc gen prop = Assert.run_property_test 
   ("Deque: " ^ desc) gen prop

;; print_endline ("\n--- Running tests for Deque ---")

(* ------------------------------------------------------------------------ *)
(* TESTING DEQUEUES                                                         *)
(* ------------------------------------------------------------------------ *)

(* Here is a test to help get you started. *)

let test () : bool =
  is_empty (create ())
;; run_test "is_empty: call on empty returns true" test

(* Again, here's a property-based test that uses `gen_ops` and `init_with`
   to check that a deque remains valid after applying a random sequences of
   operations. For deques, `gen_ops` creates random sequences of 
   `insert_head x`, `inser_tail x`, `remove_head`, and `remove_tail`.
   `init_with` applies these operations to an empty deque. *)

let property (ops : int op list) : bool =
  let d = init_with ops in
  valid d
;; run_property_test
   "randomly formed deque using operations is valid"
   gen_ops
   property

(* Now it's your turn...

   As in Problem 4, your goal is a robust set of tests, this time for deques.
   As before, you should include both unit and property-based tests. See our
   guidance on goal and grading in Problem 4.

   For this problem, a minimal (though likely insufficient) gradeable solution
   to this problem includes:
   - tests for `remove_head`, `remove_tail`, `delete_last`, `delete_first`,
     and `reverse`
   - with at least 1 property-based test using each
   - and at least 1 unit test using each

   Make sure to comprehensively test all the other functions you implemented in
   `deque.ml`. It will probably be helpful to have the files `deque.ml` and
   `deque.mli` open as you work. Your TAs will be manually grading your
   test case coverage for the 5 functions listed specified above. 

   *Should I write all of my tests before moving on to my deque implementation?*
   Probably no. See our reasoning in Problem 4.

   *Any tips on what to test?* Yes. First, review the tips in Problem 4. Then...
   - You may find it helpful to review the interface in `deque.mli` and the
     implementation in `deque.ml` for invariants, functions, and possible error 
     modes. 

   NOTE: Remember the difference between structural and reference
   equality; think about why you shouldn't be directly comparing
   deques with the '=' of structural equality. *)

(* ---------- Add your own test cases below. ---------- *)

(* INSERT_HEAD TESTS *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "insert_head into empty" test



(* Property: `insert_head` makes element the new head *)
let property ((ops, x) : int op list * int) : bool =
  let d = init_with ops in
  insert_head x d;
  valid d && peek_head d = x
;; run_property_test
     "insert_head makes x the new head"
     (gen_pair gen_ops gen_int)
     property

(* Property: `insert_head` preserves tail (when non-empty) *)
let property ((ops, x) : int op list * int) : bool =
  let d = init_with ops in
  is_empty d
  || (let old_tail = peek_tail d in
      insert_head x d;
      valid d && peek_tail d = old_tail)
;; run_property_test
     "insert_head preserves tail on non-empty"
     (gen_pair gen_ops gen_int)
     property

(* INSERT_TAIL TESTS *)



let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  valid d && peek_head d = 1 && peek_tail d = 3
;; run_test "insert_tail into non-empty, multi-element" test

(* Property: `insert_tail` makes element the new tail *)
let property ((ops, x) : int op list * int) : bool =
  let d = init_with ops in
  insert_tail x d;
  valid d && peek_tail d = x
;; run_property_test
     "insert_tail makes x the new tail"
     (gen_pair gen_ops gen_int)
     property

(* Property: `insert_tail` on empty makes head = tail *)
let property (x : int) : bool =
  let d = create () in
  insert_tail x d;
  valid d && peek_head d = x && peek_tail d = x
;; run_property_test
     "insert_tail on empty: head = tail"
     gen_int
     property

(* TO_LIST TESTS *)
let test () : bool =
  to_list (create ()) = []
;; run_test "to_list empty" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  to_list d = [1]
;; run_test "to_list singleton" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  to_list d = [3; 2; 1]
;; run_test "to_list multiple elements" test

(* Property: `insert_head` then `to_list` has element at front;
   `insert_tail` then `to_list` has element at back. *)
let rec last (lst : int list) =
  match lst with
  | [] -> assert false
  | [z] -> z
  | _ :: t -> last t

let property ((ops, x) : int op list * int) : bool =
  let d = init_with ops in
  insert_head x d;
  let starts_ok = match to_list d with [] -> false | hd :: _ -> hd = x in
  insert_tail x d;
  let lst = to_list d in
  let ends_ok = match lst with [] -> false | _ -> last lst = x in
  valid d && starts_ok && ends_ok
;; run_property_test
     "insert_head: to_list starts with x"
     (gen_pair gen_ops gen_int)
     property

(* REMOVE_HEAD *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  remove_head d = 1 && is_empty d
;; run_test "remove_head singleton" test


(* REMOVE_TAIL *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  remove_tail d = 1
;; run_test "remove_tail singleton" test


(* DELETE_LAST *)


(* DELETE_FIRST *)


(* REVERSE *)


(* ---------- Write your own test cases above. ---------- *)
