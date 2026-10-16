(* CIS 1200 OCaml testing interface      *)
(* You do not need to modify this file. *)
(* ------------------------------------ *)

(* Run a test function, printing the result of the test.
   If the function returns true, the test passes. If the function
   	returns false or throws an error, the test fails. *)
       val run_test: string -> (unit -> bool) -> unit

       (* Instruct the testing infrastructure to stop the program execution
          on the first failing test. *)
       val stop_on_failure: unit -> unit
       
       (* Get the current value of stop_on_failure flag 
          (for use by property-based tests). *) 
       val get_stop_on_failure_flag: unit -> bool
       
       (* Run a test function, printing the result of the test.
              If the function returns true or false, the test fails. If the function
              throws an error, the test passes. *)
       val run_failing_test: string -> (unit -> bool) -> unit
       
       (* Grading API: used by private test binaries to produce feedback.json *)
       val start_problem: string -> unit
       val start_kudos_problem: string -> unit
       val allocate_points: int -> unit
       val end_test: unit -> unit
       
       (* Generator specification: composable builders for `run_property_test` and
          `print_samples`.
       
          Base: `gen_int`, `gen_int_no_shrinker`, `gen_string`.
          Combinators: `gen_pair (g1, g2)`, `gen_triple (g1, g2, g3)`,
                       `gen_list g`, `gen_list_no_shrinker g`, 
                       `gen_tree g`, `gen_set g`.
          Examples:
            `gen_pair (gen_int, gen_int)`           -> `(int * int)`  
            `gen_pair (gen_list gen_int, gen_int)`  -> `(int list * int)`   
            `gen_triple (gen_set gen_int, gen_set gen_int, gen_int)` -> 
             `(int set * int set * int)`
          Tree/set: call `gen_tree gen_int` (in tree.ml) or `gen_set gen_int` 
          (in setTest.ml after `register_int_set`). *)
       type 'a gen_spec
       
       val gen_int : int gen_spec
       val gen_int_no_shrinker : int gen_spec
       val gen_string : string gen_spec
       
       val gen_pair : 'a gen_spec -> 'b gen_spec -> ('a * 'b) gen_spec
       val gen_triple : 'a gen_spec -> 'b gen_spec -> 'c gen_spec -> 
          ('a * 'b * 'c) gen_spec
       val gen_list : 'a gen_spec -> 'a list gen_spec
       val gen_list_no_shrinker : 'a gen_spec -> 'a list gen_spec
       val gen_tree : 'a gen_spec -> 'b gen_spec
       val gen_set : 'a gen_spec -> 'b gen_spec
       
       (* Property-based testing abstraction using QCheck.
          Usage: `run_property_test "desc" gen_int (fun x -> property)`
                 `run_property_test "desc" (gen_tree gen_int) (fun t -> property)`  
                      (* in tree.ml *)
                 `run_property_test "desc" (gen_pair (gen_set gen_int, gen_int)) 
                 (fun (s, x) -> property)`
          Always runs 100 test cases. QCheck and QCheck_runner must be opened in the 
          calling context. *)
       val run_property_test: string -> 'a gen_spec -> ('a -> bool) -> unit
       
       (* Print the first [num_samples] generated values from a generator.
          Use to show sample output from a generator. *)
       val print_samples: 'a gen_spec -> int -> ('a -> string) -> unit
       
       (* Property-based testing utilities *)
       val with_printer : ('a -> string) -> 'a QCheck.arbitrary -> 'a QCheck.arbitrary  
       
       (* Generic list printer: takes an element-to-string converter and a list.
          Returns a string like "[a; b; c]". *)
       val string_of_list : ('a -> string) -> 'a list -> string
       
       (* Register int set arbitrary for use with 
          `run_property_test (gen_set gen_int)`. *)
       val register_int_set : 'a -> unit
       
       (* Assert that the result of a test function equals an expected value. *)
       val assert_eqf: string -> (unit -> 'a) -> 'a -> unit
       
       (* Assert that two values are equal. *)
       val assert_eq: string -> 'a -> 'a -> unit
       