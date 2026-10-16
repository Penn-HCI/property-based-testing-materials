(* CIS 120 OCaml testing interface      *)
(* You do not need to modify this file. *)

(* Run a test function, printing the result of the test.  If the
   function returns true, the test passes. If the function returns
   false or throws an error, the test fails. *)
   val run_test: string -> (unit -> bool) -> unit

   (* Instruct the testing infrastructure to stop the program execution
      on the first failing test. *)
   val stop_on_failure: unit -> unit
   
   (* Get the current value of stop_on_failure flag
      (for use by property-based tests). *)
   val get_stop_on_failure_flag: unit -> bool
   
   (* Run a test function, printing the result of the test.  If the
      function returns true or false, the test fails. If the function
      throws an error, the test passes. *)
   val run_failing_test: string -> (unit -> bool) -> unit
   
   (* Assert that the result of a test function equals an expected value. *)
   val assert_eqf: string -> (unit -> 'a) -> 'a -> unit
   
   (* Assert that two values are equal. *)
   val assert_eq: string -> 'a -> 'a -> unit
   
   (* Generator specification: composable builders for `run_property_test` and
      `print_samples`.
      Base: `gen_int`, `gen_int_no_shrinker`, `gen_string`.
      Combinators: `gen_pair (g1, g2)`, `gen_triple (g1, g2, g3)`,
                   `gen_list g`.
      Examples:
        `gen_pair (gen_int, gen_int)`           -> `(int * int)`
        `gen_pair (gen_list gen_int, gen_int)`  -> `(int list * int)` *)
   type 'a gen_spec
   
   val gen_int : int gen_spec
   val gen_string : string gen_spec
   (* For ops with no element type; use [gen_ops [Increment; Reset] gen_unit]. *)
   val gen_unit : unit gen_spec
   
   val gen_pair : 'a gen_spec -> 'b gen_spec -> ('a * 'b) gen_spec
   val gen_triple : 'a gen_spec -> 'b gen_spec -> 'c gen_spec ->
      ('a * 'b * 'c) gen_spec
   val gen_list : 'a gen_spec -> 'a list gen_spec
   val gen_list_max : ?print:('a list -> string) -> int -> 'a gen_spec -> 'a list gen_spec
   
   (* Constant generator: always produces [x]. *)
   val gen_pure : 'a -> 'a gen_spec
   
   (* Map a generator's values by [f]. *)
   val gen_map : 'a gen_spec -> ('a -> 'b) -> 'b gen_spec
   
   (* Pick one of the given generators with equal probability. List must be non-empty. *)
   val gen_oneof : 'a gen_spec list -> 'a gen_spec
   
   (* Property-based testing abstraction using QCheck.
      Usage: `run_property_test "desc" gen_int (fun x -> property)`
             `run_property_test "desc" (gen_pair (gen_list gen_int, gen_int))
             (fun (xs, n) -> property)`
      Always runs 100 test cases. QCheck and QCheck_runner must be opened in the
      calling context. *)
   val run_property_test: string -> 'a gen_spec -> ('a -> bool) -> unit
   
   (* Print the first [num_samples] generated values from a generator. *)
   val print_samples: 'a gen_spec -> int -> ('a -> string) -> unit
   
   (* Property-based testing utilities *)
   val with_printer : ('a -> string) -> 'a QCheck.arbitrary -> 'a QCheck.arbitrary
   
   (* Generic list printer: takes an element-to-string converter and a list.
      Returns a string like "[a; b; c]". *)
   val string_of_list : ('a -> string) -> 'a list -> string
   
   (* Generic op-list generator. Instantiate with a module that describes your
      op type (e.g. queue: Enq/Deq/Contains, deque: Insert_head/Remove_tail/...).
      Each test file keeps its own op and which types; this functor provides
      [gen_ops] so you get random lists of ops from a subset of operation kinds. *)
   module type OP_DESC = sig
     type 'a op
     type which
     (* For each tag in [which], produce a gen_spec for one op. Duplicates in
        [which] make that op more likely. Result must be non-empty. *)
     val specs_for : which list -> 'a gen_spec -> 'a op gen_spec list
     (* Used to print counterexamples, e.g. "[enq 3, deq, contains 4]". *)
     val string_of_op : 'a op -> string
   end
   
   module MakeOpGen : functor (D : OP_DESC) -> sig
     (* [gen_ops which gen_elem]: e.g. [gen_ops [Increment; Reset] gen_unit] or [gen_ops [Enq; Deq] gen_int]. *)
     val gen_ops : D.which list -> 'a gen_spec -> 'a D.op list gen_spec
   end
   