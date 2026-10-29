(*****************************************************************************)
(* PROBLEM 4: WRITING TEST CASES                                             *)
(*****************************************************************************)

(* `QueueTest` is a reusable module that we'll use to test other
modules that conform to the `QUEUE` interface. When `QueueTest` is
instantiated with a particular queue implementation, it will run all
of its test cases against the queue type defined in that
implementation.  This means that the _same_ tests can be used for
both the SimpleQueue and LinkedQueue implementations -- and that both
implementations should behave the same on these tests!

Read through the module, then write your own test cases, including both
property-based tests and unit tests. More guidance below. *)

module QueueTest (QImpl: QueueInterface.QUEUE) = struct

;; open QImpl
;; open Assert
;; open QueueOps

(* -------------------------------------------------------------------------- *)
(* PBT INFRASTRUCTURE (skip to "TESTS FOR QUEUES")                            *)
(* -------------------------------------------------------------------------- *)

(* This section provides property-based testing infrastructure for queues.
queueOps.ml also contains infrastructure code for PBT. Don't modify
or worry about the code in this section and queueOps.ml. *)

module QueueOpGen = MakeOpGen (QueueOps)
type 'a op = 'a QueueOps.op

let gen_ops : int op list gen_spec =
  QueueOpGen.gen_ops [ Enq; Enq; Enq; Deq ] gen_int
let gen_list (g : 'a gen_spec) : 'a list gen_spec = gen_list_max 100 g

let apply_op (op : int op) (q : int queue) : int option = match op with
  | Enq x -> enq x q; None
  | Deq -> deq q

let string_of_ops (ops : int op list) : string = 
  string_of_list string_of_op ops

(* `init_with ops` builds a random queue state by applying `ops` to a new 
    queue `create ()`. *)
let init_with (ops : int op list) : int queue =
  fold (fun q op -> ignore (apply_op op q); q) (create ()) ops

(* Local versions `run_test` and `run_failing_test` that prepend the name of
    the implementation you are testing to the test description. *)
let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
let run_failing_test desc =
  Assert.run_failing_test (debug_name ^ ": " ^ desc)
let run_property_test desc gen prop = Assert.run_property_test 
    (debug_name ^ ": " ^ desc) gen prop

;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

(* ------------------------------------------------------------------------ *)
(* TESTS FOR QUEUES                                                         *)
(* ------------------------------------------------------------------------ *)

(* Here are all the tests, both ours and yours, that will be used to test the
  `QUEUE` abstract type. *)

let test () : bool =
  is_empty (create ())
;; run_test "is_empty: call on empty returns true" test

(* Here is another test that will *not pass* for now, until you've
  implemented the relevant functions. In general, your tests and our tests
  are likely to test functions that you have not fully implemented yet. For
  now, just make sure the tests typecheck and compile. *)

let test () : bool =
  let q = from_list [1; 2; 3] in
  not (is_empty q)
;; run_test "is_empty: non-empty set returns false" test

(* Here is a property-based that test that demonstrates the basic structure
  that most of your property-based tests should follow. If you have not yet
  read through `counterTest.ml`, do so! *)

let property (ops : int op list) : bool =
  let q = init_with ops in
  valid q
;; run_property_test
    "enq keeps queue valid"
    gen_ops
    property

(* The parts to familiarize yourself with are `gen_ops` and `init_with ops`.
  `gen_ops` creates series of `enq x` and `deq` operations that can be used
  to set up a random queue. For the purposes of this homework, `x` is always
  and integer. `init_with ops` creates an empty queue, applies the
  operations, and hands you the queue so that you can test it.

  *You are not expected to need to change `gen_ops` at any point.* We expect
  you to use it in all or most of your property-based tests to initialize
  random queues. *)

(* Property: after `init_with ops`, `enq x` matches doing the analogous
  list append operation (a differential test). *)

let property ((ops, x) : int op list * int) : bool =
  let q = init_with ops in
  let lst = to_list q in
  enq x q;
  to_list q = lst @ [ x ]
;; run_property_test
  "enq matches list append"
  (gen_pair gen_ops gen_int)
  property

(* Now, it's your turn to write tests!

  Your goal here is to produce a _robust_ set of tests---including both unit
  tests and property-based tests---that you believe can deeply stress test
  the correctness of a queue implementation. Robustness will be our main
  criterion for grading your work on this problem.

  We have provided test cases for many functions below, but they may not be
  as comprehensive as you want. In addition, some of the functions are
  missing tests entirely---in particular, `truncate` and `delete`. 
  Your main task is to write thorough tests for these two functions.

  A minimal gradeable solution to this problem includes:
  - tests for `truncate` and `delete`
  - with at least 1 property-based test for each
  - and at least 1 unit test for each

  However, you should go beyond the minimum. An effective submission will
  provide a robust set of tests for `truncate`, `delete`. You also may find
  it helpful to add tests to the other functions, even though there are
  already some provided for you. This is particularly important for this
  homework, as students have historically found the implementation and
  testing of HW04 much more difficult than HW03. Writing strong tests can
  help you catch bugs earlier and save you time in the long run.

  *Any tips on what to test?*

  Yes! Here are some ideas...
  - Review the `QUEUE` interface in `queueInterface.ml`. What are the 
    invariants of the queue? What is each function supposed to do?
  - Review the categories of properties from HW03. Could any of these 
    apply to queues? As a reminder, these categories are:
    - Round-trip properties (e.g., check that `unzip`ing a `zip`ped list of
      pairs returns the same list of pairs)
    - Postcondition properties (e.g., checking that a list maintains the 
      same elements after `sort`ing)
    - Differential properties (e.g., checking that `my_append` behaves the
      same on test inputs as OCaml's `@` operator)
    - Validity properties (e.g., a set remains valid after `add`ing an
      element)
    - Metamorphic properties (e.g. a set remains the same after `delete`ing
      an element then `add`ing it back)
  - As in HW03, you may find it useful to write properties in English 
    before deciding which ones to implement in code and how.
  - You do not need to test the `print` function. *)

(* --------------- Complete the test suite below  --------------- *)

(* ENQ TESTS *)

let test () : bool =
  let q = create () in
  enq 1 q;
  enq 2 q;
  enq 3 q;
  to_list q = [1; 2; 3]
;; run_test "enq on non-empty" test

(* DEQ TESTS *)

let test () : bool =
  let q = create () in
  begin match deq q with
  | None -> true
  | Some _ -> false
  end 
;; run_test "deq on empty" test

(* Property: `deq` removes the first element; result matches `to_list`. *)
let property (ops : int op list) : bool =
  let q = init_with ops in
  let lst = to_list q in
  begin match lst with
  | [] -> deq q = None
  | hd :: t ->
      (match deq q with
        | None -> false
        | Some e -> e = hd && to_list q = t)
  end
;; run_property_test
  "deq removes first element"
  gen_ops
  property

(* TO_LIST TESTS *)

let test () : bool =
  let q = create () in
  to_list q = []
;; run_test "to_list empty" test

let test () : bool =
  let q = create () in
  enq 2 q;
  enq 3 q;
  to_list q = [2; 3]
;; run_test "to_list non-empty" test

(* LENGTH TESTS *)

let test () : bool =
  let q = create () in
  length q = 0
;; run_test "length empty" test

(* Property: `length` agrees with the length of the list from 
  `to_list. *)
let rec list_length acc lst =
  match lst with
  | [] -> acc
  | _ :: t -> list_length (acc + 1) t

let property (ops : int op list) : bool =
  let q = init_with ops in
  length q = list_length 0 (to_list q)
;; run_property_test
  "length agrees with list length from to_list"
  gen_ops
  property

(* Property: `length` increases by 1 when an element is enqueued. *)
let property ((ops, x) : int op list * int) : bool =
  let q = init_with ops in
  let len = length q in
  enq x q;
  length q = len + 1
;; run_property_test
  "length increases by 1 when an element is enqueued"
  (gen_pair gen_ops gen_int)
  property

(* Property: `length` decreases by 1 or original queue `is_empty` when an 
    element is dequeued. *)
let property (ops : int op list) : bool =
  let q = init_with ops in
  let empty = is_empty q in
  let len = length q in
  begin match deq q with
  | None -> empty
  | Some _ -> length q = len - 1
  end
;; run_property_test
  "length decreases by 1 or original queue `is_empty` when applying `deq`"
  gen_ops
  property

(* FROM_LIST TESTS *)

let test () : bool =
  let q = from_list [] in
  valid q && to_list q = []
;; run_test "from_list empty" test

(* Property: `from_list` then `to_list` is round-trip and queue is valid *)
let property (lst : int list) : bool =
  let q = from_list lst in
  to_list q = lst && valid q
;; run_property_test
      "from_list then to_list is identity"
      (gen_list gen_int)
      property

(* CONTAINS TESTS *)

let test () : bool =
  let q = create () in
  not (contains 1 q)
;; run_test "contains empty" test

(* Property: `contains` returns true if element is in queue *)
let property ((ops, x) : int op list * int) : bool =
  let q = init_with ops in
  enq x q;
  contains x q
;; run_property_test
      "contains returns true if element is in queue"
      (gen_pair gen_ops gen_int)
      property

let test () : bool =
  let q = from_list [2; 3] in
  not (contains 4 q)
;; run_test "contains non-empty false" test

(* TRUNCATE TESTS *)

(* DELETE TESTS *)

(* ---------- Write your own test cases above. ---------- *)

end

(* We now instantiate the tests so they are executed for both LinkedQueue
and SimpleQueue.  Don't modify anything below this comment. *)

module TestSimpleQueue = QueueTest(SimpleQueue.SimpleQueue)
;; print_newline ()

module TestLinkedQueue = QueueTest(LinkedQueue.LinkedQueue)
;; print_newline ()
