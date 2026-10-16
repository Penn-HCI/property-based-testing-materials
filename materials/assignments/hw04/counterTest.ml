(*****************************************************************************)
(* COUNTER TESTING MODULE                                                    *)
(*****************************************************************************)

(* Now we begin the main part of the assignment, which revolves around the
implementation and testing of queue data structures. Before we get to queues,
though, we need to brief you on some essentials for how you will test your
queues. Read on for more context. *)

module CounterTest (CImpl: CounterInterface.COUNTER) = struct

;; open CImpl
;; open Assert
;; open CounterOps

  (* ------------------------------------------------------------------------ *)
  (* PBT INFRASTRUCTURE (skip to "Generating inputs as operation sequences")  *)
  (* ------------------------------------------------------------------------ *)

  (* This section provides some basic PBT infrastructure for counters. 
    counterOps.ml also contains infrastructure code for PBT. Don't modify
    or worry about the code in this section and counterOps.ml. *)

  module CounterOpGen = MakeOpGen (CounterOps)
  type op = unit CounterOps.op

  let gen_ops : op list gen_spec =
    CounterOpGen.gen_ops [Increment; Increment; Increment; Decrement] gen_unit

  let apply_op (op : op) (c : counter) : int option = match op with
    | Increment -> increment c; None
    | Decrement -> decrement c; None

  let string_of_ops (ops : op list) : string =
    string_of_list string_of_op ops

  (* `init_with ops` builds a random counter state by applying `ops` 
      to a new counter created with `create ()`. *)
  let init_with (ops : op list) : counter =
    fold (fun c op -> ignore (apply_op op c); c) (create ()) ops

  (* Redefine local versions of the `run_test` and
    `run_failing_test` functions so that they prepend the name of the
    implementation we're testing to the test description. *)

let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)
let run_property_test desc gen prop = Assert.run_property_test 
  (debug_name ^ ": " ^ desc) gen prop

;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

(* ------------------------------------------------------------------------ *)
(* Generating inputs as operation sequences                                 *)
(* ------------------------------------------------------------------------ *)

(* Throughout this assignment, you will be testing the data structures you
  implement, including with PBT (see HW03). Recall that PBT requires the
  generation of inputs on which properties can be tested.

  When you are testing mutable data structures like queues, there is a
  wrinkle. Say you generate a queue and a property fails on it. How do
  you access the initial queue? In many cases you can't! This is because
  the operations will have tampered with when you applied the property,
  which may have added or removed elements from the queue.

  In cases like this, we need to be more clever. This assignment provides
  one solution to this problem of recovering what a mutable data structure
  looked like before application of the property.

  Imagine if, instead of generating a concrete queue, you generated a
  sequence of operations by which that queue came to be --- a series of
  enqueue and dequeue operations, for instance. Then your property could
  instantiate a queue using those operations and, if it fails, give you
  back the generated sequence of operations. From this set of operations you
  could recover how the initial queue appeared.

  As a short hand, we call this approach ``generating inputs as operation
  sequences.'' Consider this simpler, concrete example. Here, we test a counter
  with operations to `increment`, `decrement`, `reset` to 0, and `view` the
  value (see counterInterface.ml for the full interface). Imagine we wanted
  to test the property that a counter always has a value of 0 or greater (you
  can check in counter.ml that this property should indeed hold).

  Here is our test: *)

let property (ops : op list) : bool =
  let c = init_with ops in
  view c >= 0
;; run_property_test 
  "viewing after random operations never results in a non-negative"
  gen_ops
  property

(* Let's start at the generators and work our way to the properties.

  The generator here is `gen_ops`. It generates a sequence of operations to
  initialize a counter -- a random series of increment and decrement
  operations. Run the following code snippet to see for yourself. (Feel free to
  comment it out afterwards.) *)

;; print_samples
  gen_ops
  5
  string_of_ops

(* Next, let's look at the property definition, which begins with `let c =
  init_with ops`. The property uses the `ops` to set up a counter `c`. To
  do so, it uses the `init_with` method, which takes in the `ops`, creates
  a counter using `create ()`, and then applies the operations in order. *)

let property (ops : op list) (* Take in a list of random operations, `ops` *) 
  : bool = (* Return a boolean, as with all properties *)
  let c = init_with ops in (* Create a counter with the random operations *)
  (* Check the property that count is non-negative using `view` *)
  view c >= 0

(* The check for validity happens at `view c >= 0`. This checks to make sure
  that, after initializing the counter with the series of provided operations,
  a tested counter's value is not less than 0. `view` is a function for
  accessing the current value of the counter. *)

(* Often you will want to do something to the counter *after* initializing it
  and *before* checking it. For instance, imagine you were testing the `reset`
  function. You could use `init_with ops` to set up a counter with an arbitrary
  count, and then add a call in your property to call `reset` and then check
  that the value became 0. *)

  let property (ops :op list) : bool =
      let c = init_with ops in (* randomly initialize the data structure *)
      reset c; (* perform n important operation before the check *)
      view c = 0 (* `view` returns the current value of the counter *)
  ;; run_property_test
      "resetting a counter correctly sets its value to 0"
      gen_ops
      property

(* To recap, the pattern we have described is: 
  1. Generating a sequence of initialization operations with `gen_ops` 
      (in this homework `gen_ops` is always provided for you)
  2. Testing the property, including...
    a. Initializing the data structure with random data using `init_with ops`
    b. Running key operations you want to test
    c. Checking the property
    
  Practice this pattern by completing the test below. This test should check
  that for any counter `c`, incrementing it will lead its value (as accessed
  by `view`) to be 1 more than before it was incremented. *)

  let property (ops : op list) : bool = 
      failwith "decrement_property: unimplemented"
  ;; run_property_test
      "incrementing a counter correctly raises its value by 1"
      gen_ops
      property

(* When a test written like this fails, its output will be the sequence of
  operations used to construct the initial state of data structure at the
  beginning of the test.

  See for yourself by running the following property, which suggests (wrongly!)
  that decrementing a counter twice always results in a negative count. *)

  let property (ops :op list) : bool =
      let c = init_with ops in
      decrement c;
      decrement c;
      view c < 0
  ;; run_property_test
      "decrementing a counter twice can return a negative value"
      gen_ops
      property

(* What did you see in the output? It should be an empty list. This means that
  this property fails on a counter to which *no* operations have been applied,
  or one with a value of 0. The shrinking reported suggests that, while other
  series of operations may have cause the property to fail, having no
  operations is a smaller set that also causes failure.

  Now that you know about this testing pattern, you are ready to do Problem
  4. *)

end 

(* The next line instantiates `CounterTest` with the `Counter` implementation so
  that the tests actually run... *)
module Test = CounterTest(Counter.Counter)
;; print_newline ()
