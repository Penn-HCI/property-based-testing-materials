(******************************************************************************)
(* PROBLEM 5: WRITING TESTS FOR SETS                                          *)
(******************************************************************************)

(* The module `SetTest` defined below is a reuseable component that we'll use
   to test multiple modules that conform to the `SET` interface. When `SetTest`
   is instantiated with a particular set implementation, it will run all of its
   test cases against the set type defined in that implementation.  This means
   that the _same_ tests -- both unit tests and property-based tests -- can be
   used for both the OrderedListSet and BSTSet implementations.  This makes
   sense because all implementations of `SET` should behave the same!

   WARNING: Make sure NOT to test for structural equality between sets using the
   `=` operator. Instead, use the `equals` function provided in the `SET`
   interface.

   Your TAs will be grading your completion of the PBT and unit test
   writing requirements. *)

(* Start by reading through the file `setInterface.ml` if you haven't already,
   to see what values and functions a module conforming to the `SET` interface
   must provide.  Then come back here. *)

   module SetTest (SetImpl: SetInterface.SET) = struct
   ;; open SetImpl
   ;; open QCheck
   ;; open Assert
 
   (* We first redefine the `run_test` and `run_failing_test` functions so that
      they prepend the name of the set we're testing to the test description. *)
 
   let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
   let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)
 
   ;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")
 
   (* ------------------------------------------------------------------------ *)
   (* PBT INFRASTRUCTURE (skip)                                                *)
   (* ------------------------------------------------------------------------ *)
   
   (* This section provides property-based testing infrastructure for sets.
 
      You do NOT need to read or understand the code in this section. Skip ahead
      to the next section. *)
   
   (* Printer for int sets (for better test output when tests fail) *)
   let int_set_to_string (s : int set) : string =
     let elems = list_of_set s in
     "{" ^ (String.concat ", " (List.map string_of_int elems)) ^ "}"
 
   (* Register this set implementation's arbitrary so `gen_set gen_int` works. *)
   let () =
     let open QCheck in
     let printer s = Printf.sprintf "Generated set: %s" 
       (int_set_to_string s) in
     let arb = Assert.with_printer printer 
       (set_arbitrary Gen.small_signed_int) in
     Assert.register_int_set arb
   ;;
 
   (* ------------------------------------------------------------------------ *)
   (* TESTS FOR SETS                                                           *)
   (* ------------------------------------------------------------------------ *)
   
 (* Now it's your turn!  In this Write tests to comprehensively test the
    functions defined in the `SET` interface.
 
    NOTE: Once again, remember that your tests should not use `=` to compare
    sets: use the `equals` function instead.
 
    We advise you to write tests for the functions in the order they appear in
    the interface, and to write tests for all of the functions here before you
    start implementing them. You will then be able to implement the functions
    one at a time in the files `listSet.ml` and `treeSet.ml` in the same order
    and see your tests incrementally pass.
 
    Your TAs will be manually grading your tests for engagement with the 
    requirements below. *)
 
   (* First, here is a list of facts about operations on sets, written
      informally in English.  Add at least six more at the bottom. 
      This will be graded for completeness.
 
        - If an element is already in a set, then adding it again does not
          change the set.
 
        - If an element is *not* already in a set, then adding it will make
          member yield true for this element and will not not change the other
          elements in the set.
 
        - Adding an element to a set changes its size by either zero or one.
 
     [ADD YOURS HERE]
 
  *)
 
   (* Next, write at least four property-based tests and at least four unit
      tests for the set operations, based on the ideas above. We've given you
      one to get started, combining the first two ideas above. Feel free to
      add more tests of either variety as you see fit to ensure the correctness 
      of your implementation. *)
 
   let property ((s, x) : int set * int) : bool =
     let was_member = member x s in
     let s_after_add = add x s in
        was_member     && equals s s_after_add
     || not was_member && member x s_after_add
   ;; run_property_test (debug_name ^ ": add element to set") 
      (gen_pair (gen_set gen_int) gen_int) property
 
   (* ------------- Add your tests here -------------- *)
 
 
 
   (* ------------------------------------------------ *)
 end
 
 (* The rest of the file instantiates the above tests so they are executed for
    both OrderedListSet and BSTSet.  Don't modify anything below. *)
 
 module TestOrderedListSet = SetTest(ListSet.OrderedListSet)
 ;; print_newline ()
 
 module TestBSTSet = SetTest(TreeSet.BSTSet)
 ;; print_newline ()
 