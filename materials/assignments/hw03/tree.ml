;; open QCheck
;; open QCheck_runner
;; open Assert
;; stop_on_failure ()

(******************************************************************************)
(* PROBLEM 1: BINARY TREES                                                    *)
(******************************************************************************)

(* Here is the definition of the datatype of generic binary trees from class. *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* In this file, we'll define some operations on binary trees and, later, binary
   search trees. We have provided starter tests for the more complex tree
   methods so that you can focus on implementing the functions; we'll also ask
   you to write your own tests. *)

(* The `leaf` helper function constructs a tree containing just a single `Node`
   with two empty subtrees. *)

let leaf x = Node (Empty, x, Empty)

(* Here's an example binary tree.

          5
         / \
        7   9

   Its OCaml representation is... *)

let example_tree: int tree =
  Node (
    Node (Empty, 7, Empty),
    5,
    Node (Empty, 9, Empty)
  )

(* We also provide the following printing function for you to visualize your
   trees. Feel free to use this while debugging your functions. For example, if
   the tree looks like this,

         7
        / \
       4   6

   then print_tree will print this:

       Node ( 
         Node ( 
             Empty,
             4,
             Empty
         ),
         7,
         Node ( 
             Empty,
             6,
             Empty
         )
       )
*)

let print_tree (t: int tree) : unit =
  let rec w d = if d = 0 then "" else " " ^ w (d - 1) in
  let rec aux t d =
    w (d * 4) ^
    match t with
    | Empty -> "Empty"
    | Node (lt, n, rt) ->
      "Node ( \n" ^
      aux lt (d + 1) ^ ",\n" ^
      w ((d + 1) * 4) ^ (string_of_int n) ^ ",\n" ^
      aux rt (d + 1) ^ "\n" ^
      w (d * 4) ^ ")"
  in print_string @@ "\n" ^ (aux t 0) ^ "\n"

(* Now it's your turn. Write an OCaml expression that builds the following tree:

         7
        / \
       4   6
      / \
     2   9
        / \
       5   8
*)

let your_tree: int tree = Empty (* replace this with your tree *)

(* Next, let's define a "perfect" binary tree as one where
      - every leaf is the same distance from the root, and
      - every node has either 0 or 2 children.

   For example, this is a perfect tree:

           19
          /  \
        52    16
       / \    / \
      5   2  4   1

   Write a function `is_perfect` that tests whether a tree is perfect.

   Hint: you'll probably want to write a helper function -- e.g., one that
   calculates the height of a tree. *)


let rec is_perfect (t: 'a tree) : bool =
  failwith "Tree.is_perfect: unimplemented"

(* Here are some unit test cases. You should also write some of your own --
   including some where the tree being tested is _not_ perfect!

   We will invite you to practice writing property-based tests in Problem 2. 
   For `is_perfect` and `tree_max`, we find unit tests are easier to think 
   about. *)

let test () : bool =
  is_perfect Empty
;; run_test "is_perfect: empty tree returns true" test

let test () : bool =
  is_perfect (Node (Empty, 6, Empty))
;; run_test "is_perfect: leaf returns true" test

let test () : bool =
  let ex_tree = Node (Node (Node (Empty, 5, Empty), 52, Node (Empty, 2, Empty)),
    19, Node (Node (Empty, 4, Empty), 16, Node (Empty, 1, Empty))) in
  is_perfect ex_tree
;; run_test "is_perfect: perfect multi-level tree returns true" test

(* [ADD YOUR OWN TESTS HERE...] *)

(* Next, write a function that finds the maximum element in a binary tree, as
   determined by the polymorphic function `max`, which can take in elements of
   various data types.

   Your function should work for *all* binary trees, not just binary search
   trees.

   Because an empty tree does not have a maximum element, you should call
   `failwith` if `tree_max` is passed an empty tree. *)

let rec tree_max (t: 'a tree) : 'a =
  failwith "Tree.tree_max: unimplemented"

let test () : bool =
  tree_max Empty = 42
;; run_failing_test "tree_max: running on empty fails" test

let test () : bool =
  tree_max example_tree = 9
;; run_test "tree_max: depth-2 tree" test

(* [ADD YOUR OWN TESTS HERE...] *)

(******************************************************************************)
(* PROBLEM 2: BINARY SEARCH TREES                                             *)
(******************************************************************************)


(* Next, we will write some functions that operate on binary SEARCH
   trees. Recall that a binary search tree is a binary tree that
   follows some additional invariants:

   - `Empty` is a binary search tree, and
   - `Node (lt, v, rt)` is a binary search tree if both
     - `lt` is a binary search tree, and every value in `lt` is less than `v`
     - `rt` is a binary search tree, and every value in `rt` is greater than `v`

   Notice that this description is recursive, just like our datatype
   definition!

   You may assume that all of the trees that are provided to the tests for this
   problem will satisfy this invariant. *)

(* NOTE: Again, many of the functions in the remainder of this file are
   available in the CIS 1200 lecture notes. Although it is okay to use the notes
   as a reference if you get stuck, you should ensure you _understand_ them.
   The best way to do this is to review a function in the notes (as needed),
   think about how it works, and then rewrite it from scratch without looking at
   the notes.

   We've provided some unit tests and asked you to write some more.  Also, for
   some of the functions, we ask you to write some property-based tests. 
   This is an opportunity to practice writing the tests as well as encounter
   different types of properties. *)

(* ------------------------------------------------------------------------- *)
(* PROPERTY-BASED TESTING INFRASTRUCTURE (skip)                              *)
(* ------------------------------------------------------------------------- *)

(* NOTE: There are no questions for you in this section, and you do NOT need to
   understand or modify any of the code here. These are helper functions and
   values that are used internally for some of the property-based tests.

   You can safely skip to the next section. *)

(* Use `run_property_test "name" (gen_tree gen_int) property` (and similar) for
   PBT. *)

(* Helper function to check if all elements in a tree satisfy a predicate *)
let rec all_tree (p: 'a -> bool) (t: 'a tree) : bool =
  begin match t with
  | Empty -> true
  | Node (lt, v, rt) -> p v && all_tree p lt && all_tree p rt
  end
  
(* Check if a tree is a valid BST *)
let rec is_valid (t: 'a tree) : bool =
  begin match t with
  | Empty -> true
  | Node (lt, v, rt) ->
    all_tree (fun x -> x < v) lt &&
    all_tree (fun x -> x > v) rt &&
    is_valid lt &&
    is_valid rt
  end

(* ------------------------------------------------------------------------- *)
(* BINARY SEARCH TREES                                                       *)
(* ------------------------------------------------------------------------- *)

(* Your first job in this section will be to write the `lookup` function, which
   searches a generic binary search tree for a particular value. You should
   leverage the BST invariants here -- i.e., your implementation should NOT have
   to search every subtree.

   (Again, this function was discussed in class, but you should try to recreate
   it from scratch, to make sure you understand it.) *)

let rec lookup (x: 'a) (t: 'a tree) : bool =
  failwith "Tree.lookup: unimplemented"

(* Note that the `lookup` function should assume that its argument is a binary
   search tree. This means that elements that are out of place should NOT be
   found by `lookup`, as demonstrated by the first test below, which calls
   `lookup` on a tree that is NOT a BST. *)

let test () : bool =
  not (lookup 7 example_tree)
;; run_test "lookup: assumes BST invariants" test

let test () : bool =
  let t = Node (Node (Empty, 1, Empty), 2,  Node (Empty, 3, Empty)) in
  not (lookup 5 t)
;; run_test "lookup: not in tree" test

let test () : bool =
  let t = Node (Node (Empty, 2, Empty), 5,  Node (Empty, 7, Empty)) in
  lookup 2 t
;; run_test "lookup: element in tree" test

(* The next function, `inorder` should return all of the elements of a binary
   search tree in ascending order. This is called the "in-order traversal" of a
   BST. *)

let rec inorder (t: 'a tree) : 'a list =
  failwith "Tree.inorder: unimplemented"

(* One important fact about `inorder` is the fact that it should always return a
   sorted list. This is a nice opportunity to use a property-based test! Think a
   bit about what category of property this is amongst the ones we've seen so
   far.

   Complete the test below that encodes this property. As a reminder the 
   `gen_tree gen_int` generator generates random, valid `int` trees. To answer
   the question above about the category of property, this is a postcondition 
   property.

   Hint: You may find it helpful to reuse the code for `is_sorted` function from
   pbtIntro. *)



let property (t : int tree) : bool =
    failwith "Tree.test_inorder_sorts: unimplemented"
;; run_property_test "inorder always returns a sorted list" 
   (gen_tree gen_int) property

(* That property test is great and pretty obvious, but it's incomplete! 
   It doesn't check for other aspects of correct behavior.
   
   The `is_sorted` function only verifies that the output is in order. But 
   what if `inorder` returns a sorted list that's missing elements? Or 
   has duplicates? Or contains completely different numbers? 
   
   This is a limitation of that property: being sorted is necessary but not 
   sufficient for correctness.
   
   How could we test the other aspects of correct behavior? We could write 
   unit tests! *)

let test () : bool =
  inorder Empty = []
;; run_test "inorder: empty" test

let test () : bool =
  inorder (Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))) = [1; 2; 3]
;; run_test "inorder: regular BST" test

(* Next, write the function that inserts an element into a binary search tree.
   Again, write it from scratch if possible, rather than looking back at the
   lecture notes. *)

let rec insert (x: 'a) (t: 'a tree) : 'a tree =
  failwith "Tree.insert: unimplemented"

(* Let's use `insert` to get some more PBT practice. This is a scneario where
   PBT really shines: there are infinitely possible tree and values where we 
   could insert. Along with that, we can check the correctness of hundreds 
   of examples just by pulling properties that are very simple to write. Both 
   the properties we present are examples of this.                        

   Since we expect `insert` to modify the inputted tree, we should make sure 
   that to modify the inputted tree, we should make sure that the outputted tree
   is still a valid BST.
   
   Define the following property of `insert`: After applying `insert` to a BST,
   the resulting BST should be a valid BST.
   
   You should use `is_valid`, defined above in the PBT infrastructure, 
   that checks if a tree is a valid BST. *)
let property ((t, x) : int tree * int) : bool =
    failwith "Tree.test_insert_preserves_validity: unimplemented"
;; run_property_test "insert preserves BST validity after inserting a value" 
   (gen_pair (gen_tree gen_int) gen_int) property

(* This is another example of a postcondition property, specifically checking
   the validity of a data structure invariant. Checking that invariants hold
   after an operation is a very common use case for PBT -- so common that such
   properties have a special name: "validity" properties. *)

(* Another obvious property of `insert` is that the element we inserted is found
   in the tree after insertion using `lookup`.

   This is another example of a postcondition property, but instead of having to
   define a helper function like `is_sorted`, we rely on the `lookup` function
   we already wrote.

   Complete the property-based test below to test this property. *)
let property ((t, x) : int tree * int) : bool =
    failwith "Tree.test_insert_finds_value: unimplemented"
;; run_property_test "insert finds value" (gen_pair (gen_tree gen_int) gen_int) 
   property

(* With these properties, think about if we need any other tests for `insert`.
   An alternative perspective to ask is if there are any places you are unsure 
   that the current properties don't test. If so, write them below. *)

(* [ADD YOUR OWN TESTS HERE...] *)

(* Now use `insert` to write a function `tree_of_list` that returns a list of
   all the labels in a tree. *)

let rec tree_of_list (l: 'a list) : 'a tree =
  failwith "Tree.tree_of_list: unimplemented"

let test () : bool =
  tree_of_list [2; 4] = Node (Node (Empty, 2, Empty), 4, Empty)
;; run_test "tree_of_list: depth-2 tree" test

let test () : bool =
  tree_of_list [5; 2; 4] =
    Node (Node (Empty, 2, Empty), 4, Node (Empty, 5, Empty))
;; run_test "tree_of_list: multiple leaves" test

(* The `tree_of_list` function has a nice round-tripping property: if we convert
   any list `l` into a tree using `tree_of_list`, then converting the resulting
   tree into a list using `inorder` should result in the original sorted list
   with no duplicates. It is implemented below with a `List` module function. *)
   
let property (l : int list) : bool = 
    inorder (tree_of_list l) = List.sort_uniq compare l
;; run_property_test "tree_of_list round trip" (gen_list gen_int) property

(* Notice the extra step `List.sort_uniq` required to compare the lists. It 
   sorts the list and removes duplicates. To demonstrate the functionality,
   we provide a couple of unit tests below. *)

let test () : bool =
  List.sort_uniq compare [2; 4; 3] = [2; 3; 4]
;; run_test "List.sort_uniq: sorts" test

let test () : bool =
  List.sort_uniq compare [7; 7; 7] = [7]
;; run_test "List.sort_uniq: removes duplicates" test

let test () : bool =
  List.sort_uniq compare [2; 3; 7; 7; 6] = [2; 3; 6; 7]
;; run_test "List.sort_uniq: sorts and removes duplicates" test

(* This property-based test is a reminder that properties have to be carefully
   crafted. What happens to the validity of the property if we remove the 
   `List.sort_uniq` call?
   
   Since we generates random lists that *might* contain duplicates, when we 
   convert the list to a tree, those duplicates may be lost. Moreover,
   the method we have of converting a tree back to a list, `inorder`, returns a 
   sorted list. Thus, we need to both sort and remove the duplicates from the 
   randomly generated list to make a correct property. *) 

(* The `delete` function returns a tree that is like `t` except with the
   element `x` removed.  If `x` is not present, the resulting tree is the
   same `t`. *)


let rec delete (x: 'a) (t: 'a tree) : 'a tree =
  failwith "Tree.delete: unimplemented"

(* Here are some simple unit tests for delete. *)

let test () : bool =
  delete 3 Empty = Empty
;; run_test "delete: empty" test

let test () : bool =
  delete 2 (Node (Node (Empty, 2, Empty), 5, Node (Empty, 7, Empty))) =
    Node (Empty, 5, Node (Empty, 7, Empty))
;; run_test "delete: element in tree" test

let test () : bool =
  let t = (Node (Node (Empty, 2, Empty), 5, Node (Empty, 7, Empty))) in
  delete 1 t = t
;; run_test "delete: element not in tree" test

(* Write a property-based test for `delete` testing its functionality with 
  `lookup`

   Hint: It might be helpful to follow the model of the property of 
   `insert` above. *)

let property ((t, x) : int tree * int) : bool =
  failwith "Tree.delete_property: unimplemented"
;; run_property_test "delete" (gen_pair (gen_tree gen_int) gen_int) property

(* Finally, let's write two properties to test `insert` and `delete` together.
   These tests will both build further confidence in our implementation and
   introduce a new "flavor" of property. These properties are METAMORPHIC 
   PROPERTIES. Metamorphic properties are properties that compare
   relationships between different series of operations. Metamorphic properties 
   are useful because they allow you to test for consistency when it's hard to 
   assess structure or correctness directly. Additionally, they explicitly 
   test interactions between operations, instead of testing them in isolation. 

   In the first property, we check the relationship between a tree that has had 
   both`delete` and `insert` applied to it, and a tree that has had only 
   `insert` applied to it.

   Property 1: If we delete an element `x` and then insert the same element `x`,
   the values in the tree should be the same as simply inserting the element `x`
   into the original tree. *)
let property ((t, x) : int tree * int) : bool =
  failwith "Tree.test_delete_then_insert_same: unimplemented"
;; run_property_test "delete then insert same element" 
   (gen_pair (gen_tree gen_int) gen_int) property

(* Property 2: If we delete an element `d` and then insert a *different* element
   `i`, the tree's containded values should be the same as inserting `i` then
   deleting `d` -- the order of the two operations doesn't matter.
   
   Hint: Since `d` and `i` are both randomly generated, are they guaranteed to
   always to be different? *)
let property ((t, d, i) : int tree * int * int) : bool =
    failwith
       "Tree.test_delete_then_insert_different: unimplemented"
;; run_property_test "delete then insert different element" 
   (gen_triple (gen_tree gen_int) gen_int gen_int) property
