;; open QCheck
;; open Assert
;; stop_on_failure ()

(******************************************************************************)
(* PROBLEM 0: INTRODUCTION TO PROPERTY-BASED TESTING                          *)
(******************************************************************************)

(* This file is an introduction to property-based testing (PBT) and a guide to
   using property-based tests in this assignment and HW04. *)

(* ------------------------------------------------------------------------- *)
(* Properties                                                                *)
(* ------------------------------------------------------------------------- *)

(* Property-based testing is a technique for testing programs by writing down
   "properties" that the program should satisfy and checking that the programs
   satisfy them on many test inputs.
   
   PBT was first developed in the early 2000s. Since then, it has been widely
   adopted in industry and continuously studied in academia. The role it plays
   in software development is helping developers check critical code that *must*
   work, such as critical data structure libraries.  

   PBT is now supported in over 50 languages, including Java, Python, and of
   course, OCaml. PBT is taught in 1200 so you can learn to write more powerful
   tests of your code in Homeworks 3 and 4. It is no coincidence that you will
   using PBT to test the same kinds of structures (sets, trees, queues) that
   developers in industry might be testing with PBT.
   
   This homework introduces you to PBT and provides you first experiences in
   learning to apply it productively in your coding practice. *)

(* Let's start with an example. Imagine you want to test a function
   that computes the absolute value of an integer. We'll call this function
   `abs_value`. *)

let abs_value (n : int) : int =
   if n < 0 then -n else n
 
(* A good way to learn how to write a property-based test is to generalize from
   unit tests, which you have been writing in previous homeworks. As you know,
   unit tests check that a function returns an expected output for a provided
   input. For example, Here are some unit tests for `abs_value`. *)
 
 let test () : bool =
   abs_value 1 = 1
 ;; run_test "abs_value positive" test
 
 let test () : bool = 
   abs_value 0 = 0
 ;; run_test "abs_value zero" test
 
 let test () : bool = 
   abs_value (-1) = 1
 ;; run_test "abs_value negative" test

(* Let's say you wanted to be extremely confident of the correctness of
   `abs_value`. (Obviously, `abs_value` is a one-liner and it is not hard to
   convince ourselves that it is correct without any tests at all. We're just
   using it as a first example to illustrate the ideas.)

   One way to increase confidence is to write more unit tests, each checking the
   function's behavior on a different value. If `abs_value` were more
   complicated, we might be able to think of some more, but this will become
   pretty boring at some point. Moreover, we might easily miss corner cases.

   A different approach is to step back, think a bit more abstractly about what
   we intend `abs_value` to do, and try to write down one or more generic
   PROPERTIES that express this intention. For example, one obvious property of
   `abs_value` is that its result should always be non-negative. *)
 
let property (n : int) : bool =
   abs_value n >= 0

(* Broadly, a property is a general statement about the expected behavior of a
   piece of code. This property states that "the result of `abs_value` should
   always be greater than or equal to 0." 
   
   Properties are written as functions returning a boolean -- they take
   potential input value `n` as their argument and they return `true` if the
   function under test does the right thing on `n`.

   This is a general statement about the behavior of the function that should
   hold for arbitrary inputs. Such statements are often called POSTCONDITION
   PROPERTIES, since they state a condition that must hold "post" (i.e., after)
   the function returns. *)
   
(* Having written down a property, one thing we can do is to use `run_test` to
   apply it to specific arguments. *)

 let test () : bool =
   property 1
 ;; run_test "abs_value positive returns >= 0" test
 
 let test () : bool = 
   property 0 
 ;; run_test "abs_value zero returns >= 0" test
 
 let test () : bool = 
   property (-1)
 ;; run_test "abs_value negative returns >= 0" test

(* ------------------------------------------------------------------------- *)
(* Generators                                                                *)
(* ------------------------------------------------------------------------- *)

(* But we can do better. Instead of manually applying `property` to several
   different inputs, we can generate a large number of *random* inputs and check
   that it yields `true` for all of them.

   This is where GENERATORS come in.  For example, here is a generator for
   random integers (run this file and see what the output looks like): *)

 (* Print some sample generated values... *)
 ;; print_samples 
      gen_int        (* ... from this generator *)
      5              (* ... 5 of them *)
      string_of_int  (* ... using this function to convert them to strings *)
 
(* Think of generators as functions that produce random values of specific
   types. The generator here, `gen_int`, produces random integers. The following
   code snippet samples input values from a different generator. Check what gets
   printed. *)

 (* Print 5 random lists. *)
 ;; print_samples
      (gen_list gen_int)
      5
      (string_of_list string_of_int)

(* For present purposes, you don't need to know exactly how generators work or
   how to write them: we've provided all the generators you will need.
   
   Now let's put these two pieces -- properties and generators -- together into
   a test. We use the `run_property_test` function to sample many values from a
   generator and apply the property to each. Check what the output looks
   like. *)

 ;; run_property_test "property-based test, `abs_value` non-negative" 
    gen_int property

(* The test output tells us how many random input values were generated, how
   many errored, how many failed, and how many passed. The total number of
   generated inputs defaults to 100. *)

(* ------------------------------------------------------------------------- *)
(* Shrinking                                                                 *)
(* ------------------------------------------------------------------------- *)

(* One last concept is useful here in making the most of output from
   property-based tests. That concept is *shrinking*. Since we are relying on
   generators to produce many random inputs, an input that causes a property to
   fail may be large and difficult to interpret.

   Shrinking is a feature of PBT that helps identify a minimal input that causes
   a property to fail. Shrinking is defined within the generators you'll be
   using, so you do not have to implement it yourself.

   To visualize what shrinking does, let's take a look at a property-based test
   that fails. Below we assert the (incorrect) property that the `abs_value`
   function always returns a negative value. It uses the generator
   `gen_int_no_shrinker`, which does not perform shrinking. As a result, the
   test will fail and print out the first input that causes the property to
   fail.
   
   Run the file and inspect the output of the test below. You should see that
   the output states: `Test property-based test, `abs_value` negative failed (0
   shrink steps)` followed by the input that caused the failure. The "0 shrink
   steps" indicates that it did not attempt to shrink the input. 

   Next, switch the generator to `gen_int` and run the file again. You will see
   that the test still fails, but now the output reports some shrink steps and
   the output should be a smaller failing value.

   Finally, comment out this test and continue with the rest of this problem.
   
   Note: `gen_int_no_shrinker` is a special generator that does not shrink,
   created solely for this demonstration. You should not use it for your own
   tests! *)

let property (n : int) : bool = abs_value n < 0 (* Bad property *)
;; run_property_test "property-based test, `abs_value` negative" 
  gen_int_no_shrinker (* Switch to gen_int after running *) property

(* To illustrate how shrinking can be helpful, here is a nonsensical
   property about lists (a more complex data structure compared to integers), 
   stating that all lists have a length that is divisible by 3. *)

 let property (lst : int list) : bool =
   List.length lst mod 3 = 0
;; run_property_test 
   "property-based test, incorrect list size property (no shrinking)" 
   (gen_list_no_shrinker gen_int_no_shrinker) property
 
(* Run the test multiple times to see what the outputs look like with shrinking
   off.
    
   Then, change `(gen_list_no_shrinker gen_int_no_shrinker)` to 
   `(gen_list gen_int)` and run the test again a couple of times to see what 
   happens. After you finish observing the behavior, comment this test out 
   and continue with the rest of the file. *)
 
(* To recap, property-based testing relies on the programmer to declare
   properties about the behavior of the code under test. These properties are
   then tested on a large number of random inputs, and the test is considered to
   pass if the property holds for all of the inputs. The essential ingredients
   are:

     - (1) a PROPERTY (describing some expectation about the behavior of the
       code),

     - (2) a GENERATOR (of random inputs), and

     - (3) SHRINKING (to reduce failing tests to be as simple as possible). 

   This assignment is focused on writing properties: all the generators and
   shrinkers you will need are provided. *)
      
(* ------------------------------------------------------------------------- *)
(* Counter-examples                                                          *)
(* ------------------------------------------------------------------------- *)

(* Here's another example of a property -- this time, an incorrect one, so that
   you can see what the testing framework does when it finds a counter-example.
   Look at the test output, then fix the property so that it succeeds. *)
   
 let property (n : int) : bool =
   n - n = 42
   (* Nonsensical property: fix the right-hand side of the equation! *)
 ;; run_property_test 
    "property-based test, incorrect mathematical property" 
    gen_int property
 
(* ------------------------------------------------------------------------- *)
(* Round-trip properties                                                     *)
(* ------------------------------------------------------------------------- *)

(* To illustrate a slightly different property using a more complex example.
   Imagine you want to test the functions `zip` and `unzip` introduced in
   Lecture 4. As a reminder, `zip` combines two lists by pairing their elements,
   and `unzip` splits a list of pairs into two lists. *)

let rec zip (l1:int list) (l2:string list) : (int * string) list =
  begin match (l1, l2) with
    | ([], []) -> []
    | (x::xs, s::ss) -> (x,s)::(zip xs ss)
    | (_, _) -> [] (* truncates to the shorter list length *)
end
  
let rec unzip (l:(int *string) list) : (int list) * (string list) =
  begin match l with
  | [] -> ([], []) 
  | (x,y)::tl -> 
    let (xs, ys) = unzip tl in 
    (x::xs, y::ys) 
end
   
(* As we did above, let's start with unit tests.

   One approach to writing tests for `zip` and `unzip` is testing both `zip` and
   `unzip` together. This is not a complete test -- we would still want to test
   each function individually -- but it's a strong sanity check that catches
   many bugs at once and provides added confidence in the correctness of both
   functions.

   Below we show two unit tests: one for the empty list and one for a non-empty
   list. *)

(* Test with empty list *)
let test () : bool =
  let (l1, l2) = unzip [] in
  zip l1 l2 = []
;; run_test "zip unzip empty" test

(* Test with non-empty list *)
let test () : bool =
  let (l1, l2) = unzip [(1, "a"); (2, "b"); (3, "c")] in
  zip l1 l2 = [(1, "a"); (2, "b"); (3, "c")]
;; run_test "zip unzip non-empty" test

(* We can abstract these two tests into a generic property: "given any list of
   pairs, unzipping it and then zipping the resulting lists should return the
   original list of pairs."  Here is a function that represents this
   property: *)

let unzip_zip_property (l:(int * string) list) : bool =
  let (l1, l2) = unzip l in
  zip l1 l2 = l

(* You can rewrite the tests above to use this property like this: *)

let test () : bool = unzip_zip_property []
;; run_test "unzip zip round trip: empty" test

let test () : bool = unzip_zip_property [(1, "a")]
;; run_test "unzip zip round trip: singleton" test

let test () : bool = unzip_zip_property [(1, "a"); (2, "b"); (3, "c")]
;; run_test "unzip zip round trip: three pairs" test

(* Alternatively -- and preferably -- we can use a generator (for random lists
   of pairs) to test our property on MANY inputs. *)

let property (l:(int * string) list) : bool = unzip_zip_property l
;; run_property_test "unzip zip round trip" 
   (gen_list (gen_pair gen_int gen_string)) 
   property

(* Note the use of `gen_pair` here. You can create a generator for tuples of
   random values using `gen_pair` or `gen_triple`. For example `gen_pair gen_int
   (gen_list gen_int)` will generate a (int * int list). *)

(* The property relating `zip` and `unzip` is a ROUND-TRIP PROPERTY because it 
   makes a claim about something that should hold after performing then 
   "undoing" an operation. *)

(* When a property-based test fails, it can mean either of two things: (1) the
   function being tested is wrong and needs to be fixed, or (2) the function is
   fine but we wrote down a property that doesn't describe the behavior we
   actually wanted.

   For example, having seen the `unzip_zip_property` above, one might imagine
   that the opposite round-trip property should also hold.

   Complete the property below so that it states: "When given an `int list` and
   a `string list`, zipping them together and then unzipping the result should
   yield the two original lists again." *)

let zip_unzip_property (l1: int list) (l2: string list) : bool =
   failwith "zip_unzip_property: unimplemented"

(* Next, think carefully about whether this "reverse" property is actually true.

   Finally, complete the following property-based test to see what the
   property-based test finds.  Did the behavior match your expectations?
   
   Note: This property-based test uses two arguments. The generator
   `gen_pair (gen_list gen_int) (gen_list gen_string)` for a
   `(int list * string list)`. *)

let property ((l1, l2) : int list * string list) : bool = 
   failwith "zip_unzip_property_test: unimplemented"
;; run_property_test 
   "zip unzip round trip" 
   (gen_pair (gen_list gen_int) (gen_list gen_string)) 
   property

(* Comment this bad property-based test out. With these examples in mind, 
   let's do some more practice writing properties for lists... *)

(* ------------------------------------------------------------------------- *)
(*  Practice: Writing Properties for Lists                                   *)
(* ------------------------------------------------------------------------- *)

(* Let's look at a more examples of PBT extending the previous example of list 
   functions. Imagine we are implementing some functions on lists: 

     - `my_sort` should produce a sorted list.
     - `my_reverse` should take a list and produce a list with the
       same elements in reverse order.
     - `my_add` should add an element at the end of a list.

   We will use PBT to test our implementation of these functions. Pay close
   attention to the properties provided and convince yourself that they are
   correct.
   
   Let's first define the functions we will be testing. *)

let my_sort (lst : 'a list) : 'a list =
  let rec insert x = function
    | [] -> [x]
    | h :: t as lst -> if x <= h then x :: lst else h :: insert x t
  in
  let rec sort = function
    | [] -> []
    | h :: t -> insert h (sort t)
  in
  sort lst

let my_reverse (lst : 'a list) : 'a list =
  let rec process acc = function
    | [] -> acc
    | h :: t -> process (h :: acc) t
  in
  process [] lst

let my_add (x : 'a) (lst : 'a list) : 'a list =
  let rec process acc = function
    | [] -> my_reverse (x :: acc)  
    | h :: t -> process (h :: acc) t  
  in
  process [] lst

(* Next, let's write down the properties that we will be testing. We will
   describe them in English and provide any helper functions needed to define
   them in OCaml. *)

(* PROPERTY 1: `my_sort` produces a sorted list.  The output list should be in
   sorted order, where each element is less than or equal to the next. *)

(* Helper function to check if a list is sorted *)
let is_sorted (lst : int list) : bool =
  let rec check = function
    | [] | [_] -> true
    | x :: y :: rest -> x <= y && check (y :: rest)
  in check lst

(* Complete the following property-based test using `is_sorted` as a
   helper. *)
let property (lst : int list) : bool =
  failwith "test_sort_produces_sorted_list: unimplemented"
;; run_property_test "sort produces sorted list" (gen_list gen_int) property

(* PROPERTY 2: `my_sort` preserves elements.
   The output list should contain exactly the same elements as the input list
   (no elements were added or removed). *)

(* Helper function to check if two lists (`lst1` and `lst2`) contain the same
   elements (with the same multiplicities - same count of each element). *)
let same_elements (lst1 : int list) (lst2 : int list) : bool =
  let rec count x = function
    | [] -> 0
    | h :: t -> (if h = x then 1 else 0) + count x t
  in
  let rec check = function
    | [] -> true
    | h :: t -> count h lst1 = count h lst2 && check t
  in
  let rec length = function
    | [] -> 0
    | _ :: t -> 1 + length t
  in
  length lst1 = length lst2 && check lst1

(* Complete the property-based test below using `same_elements` as a helper
   function. 
   
   Note that this property may take a few seconds to test. This is expected. *)
let property (lst : int list) : bool =
  failwith "test_sort_preserves_elements: unimplemented"
;; run_property_test "sort preserves elements" (gen_list gen_int) property
    
(* To categorize these properties, both of these properties are postcondition  
   properties. They both make a claim about the result after applying
   the function `my_sort`. This also demonstrates that we can often write 
   multiple properties for a single function. 

   Notice how these two properties for `my_sort` together tell us something very
   strong about the `my_sort` function: It returns a list with the same elements
   as its input, and the list it returns is sorted.  I.e., it returns a sorted
   version of its input. This is everything we care about in a list-sorting
   function! In some cases, you will be able to find this "complete" coverage,
   but in other cases, you will not (such as our next example). Nevertheless, 
   more confidence is better than less confidence, which is why properties are 
   a powerful tool for testing. *)

(* PROPERTY 3: `my_reverse` is "involutive" -- a fancy way of saying that, if I
   reverse a list twice, we get the original list back. This is a roundtrip 
   property, it happens to be that the reverse function is its own inverse. *)

(* Complete the property-based test below. *)
let property (lst : int list) : bool =
  failwith "test_reverse_twice: unimplemented"
;; run_property_test "reverse round trip" (gen_list gen_int) property

(* PROPERTY 4: `my_add` matches OCaml's implementation.  If I add an element
   using my own implementation, it should match an already correct
   implementation, such as using OCaml's built-in append operator. This can be
   considered a new type of property, a "differential" property. Differential
   properties are properties that compare the output of two different
   implementations of the same function. Here, we rely on OCaml's built in
   implementation to see if there are any _differences_ between our own
   implementation and OCaml's. *)
   
(* Complete the property-based test below. *)
let property ((lst, x) : int list * int) : bool =
   failwith "test_add_matches_ocaml_append: unimplemented"
;; run_property_test 
   "add matches OCaml's append operator" 
   (gen_pair (gen_list gen_int) gen_int) property

(* ------------------------------------------------------------------------- *)
(* Recap                                                                     *)
(* ------------------------------------------------------------------------- *)

(* We've seen three sorts of properties in this file:

     - ROUND-TRIP PROPERTIES that test whether doing and then undoing an
       operation gives back the original input.

       Example: unzipping and zipping a list of pairs results in the original
       list.

     - POSTCONDITION PROPERTIES that evaluates the output of a function against
       some expected result.

       Example: sorting a list results in a list with the same elements as the
       original.

     - DIFFERENTIAL PROPERTIES that test if two implementations of the same
       function produce the same result.

       Example: A list append function should produce the same result as OCaml's
       built-in append operator.

   A few other common sorts of properties are:

     - VALIDITY PROPERTIES are a special flavor of postcondition properties that
       tests whether the result of a obeys some set of invariants, particularly
       data structure invariants.

       You will see this in tree.ml.

     - METAMORPHIC PROPERTIES compare the outputs between different series of
       operations, ensuring that known properties hold between them after the
       fact. They are used to test correctness without predicting the exact 
       result, or "related calls return related results".

       You will see this in tree.ml.
 *)
