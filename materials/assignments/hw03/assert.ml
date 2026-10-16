(* -------------------------------------------------- *)
(* CIS 1200 OCaml testing interface                   *)
(* You do not need to modify or understand this file. *)
(* -------------------------------------------------- *)

open Printf
open List
open Yojson

let commandline : bool = not !Sys.interactive
(* When true, we are the grading binary (run with one arg: output path for 
feedback.json); do not exit on stub top-level test failures. *)
let grading_mode : bool = commandline && (Array.length Sys.argv) = 2

(******************************************************************************)
(* COMMON PRINTING UTILITIES                                                  *)
(******************************************************************************)

let sep_line = String.make 79 '#'

(* Print a clear "PBT block start" so it's obvious when a new 
property test begins. *)
let pbt_start (desc : string) : unit =
  print_endline "";
  print_string "\x1b[34m";  (* Change terminal to blue *)
  print_endline sep_line;
  print_string "\x1b[0m";  (* Change terminal back to default *)
  print_endline ("PBT: " ^ desc);
  print_endline ""

(* Print a clear "PBT block end" with pass/fail so it's obvious when 
the test finishes. *)
let pbt_footer (desc : string) (n_fail : int) : unit =
  print_endline "";
  if n_fail = 0 then
    print_endline ("  --- " ^ desc ^ ": PASSED ---")
  else
    print_endline ("  *** " ^ desc ^ ": FAILED (" ^ string_of_int n_fail ^ " 
    failure(s)) ***");
  print_endline (String.make 79 '-');
  print_endline ""

(******************************************************************************)
(* UNIT TESTING                                                               *)
(******************************************************************************)

exception Timeout

(* Make sure that a test case does not run too long. Raises Failure 
   on a time out after t seconds. *)
let run_with_timeout (t:int) (f:'a -> 'b) (x:'a) : 'b =
  try
    Sys.set_signal Sys.sigalrm 
      (Sys.Signal_handle (fun _ -> raise Timeout));
    ignore (Unix.alarm t);
    let y = f x in
    ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm Sys.Signal_default;
    y
  with Timeout -> 
    (Sys.set_signal Sys.sigalrm Sys.Signal_default; 
     raise (Failure ("Timeout: test case ran for " 
                     ^ string_of_int t ^ " seconds.")))

(* The result of a test *)
type result =
  | Succ             (* Test succeeds *)
  | Fail             (* Test fails normally, usually returning false *)
  | Err of string    (* Test throws an error, indicated *)

(* Grading API: used by private test binaries to produce feedback.json *)
type testresult = { name : string; success : result }
type problem = { name : string; kudos : bool; mutable points : int; 
    mutable results : testresult list }
let problems : problem list ref = ref []
let kudos : problem list ref = ref []

let string_of_result (r : result) : string =
  match r with
  | Succ -> "Success"
  | Fail -> "Failure"
  | Err x -> "Error: " ^ x

let float_score (results : testresult list) : float =
  let succ, total =
    List.fold_right (fun res acc ->
      let (s, tot) = acc in
      (s + if res.success = Succ then 1 else 0), 1 + tot
    ) results (0, 0)
  in (float_of_int succ) /. (float_of_int total)

let allocate_points (points : int) : unit =
  (List.hd !problems).points <- points

let start_problem (name : string) : unit =
  problems := { name; kudos = false; points = 0; results = [] } :: !problems

let start_kudos_problem (name : string) : unit =
  problems := { name; kudos = true; points = 0; results = [] } :: !problems

let process_outcome (msg : string) (outcome : result) : unit =
  match !problems with
  | [] -> ()
  | current :: _ ->
      print_newline ();
      current.results <- { name = msg; success = outcome } :: current.results

let end_test () : unit =
  let out_path =
    if commandline && Array.length Sys.argv >= 2 then Sys.argv.(1)
    else "feedback.json"
  in
  let feedback_out = open_out out_path in
  let json =
    `Assoc [
      ("scores",
       `List (List.rev_map (fun (prob : problem) ->
        `Assoc [
          ("name", `String prob.name);
          ("kudos", `Bool prob.kudos);
          ("points", `Int prob.points);
          ("score", 
          `Float (float_of_int prob.points *. float_score prob.results));
          ("tests", `List (List.map (fun (res : testresult) ->
            `Assoc [("name", `String res.name); ("result", 
            `String (string_of_result res.success))]
          ) prob.results))
        ]
      ) !problems))
    ]
  in
  Yojson.Basic.pretty_to_string json |> fprintf feedback_out "%s";
  close_out feedback_out

(* When set to true, stop immediately on a failing test *)
let stop_on_failure_flag = ref false

let stop_on_failure ()     = stop_on_failure_flag := true
let continue_on_failure () = stop_on_failure_flag := false

(* Export the flag for use by property-based tests *)
let get_stop_on_failure_flag () = !stop_on_failure_flag

(* Global registry for int_set arbitrary (set by SetTest module) *)
(* Note: We use Obj.magic to store the arbitrary since 'a set is abstract *)
let int_set_registry : Obj.t option ref = ref None

(* Register int set arbitrary for use with run_property_test 
(gen_set gen_int) *)
let register_int_set (arb: 'a) : unit =
  int_set_registry := Some (Obj.repr arb)

(* Display a message about a failing test *)
let error_mesg (s: string) : unit =
  print_endline s;
  (* In grading mode, never exit so stub top-level run_test failures 
  don't abort before end_test(). *)
  if not grading_mode && !stop_on_failure_flag then exit 1 else ()

(* Run a testing function (actual_fcn) and compare its value with
   an expected value, using OCaml's polymorphic structural equality. *)
let assert_eqf (msg: string) (actual_fcn: unit -> 'a) (expected: 'a) : unit =
  let _ = print_string ("Running: "^msg^" ... ") in
  let _ = flush_all () in
  let timeout_s = if !problems <> [] then 30 else 10 in
  let outcome = try if expected = run_with_timeout timeout_s actual_fcn ()
      then Succ else Fail
    with Failure s -> Err s
       | e         -> Err (Printexc.to_string e) in

  if !problems <> [] then
    process_outcome msg outcome
  else
    begin match outcome with
      | Succ -> print_endline ("Test passed!")
      | Fail ->
        print_newline ();
        error_mesg ("Test failed: "^msg^"\n")
      | Err s ->
        print_newline ();
        let stack = Printexc.get_backtrace () in
        error_mesg ("Test error: `"^msg^"` reported `" ^ s ^ "`\n" ^ stack)
    end

(* Assert that the result of a test (actual) is the same as the expected
   value *)
let assert_eq (msg:string) (actual:'a) (expected:'a) : unit =
  assert_eqf msg (fun () -> actual) expected

(* Run a test function, printing the result of the test.
   If the function returns true, the test passes. If the function
   	returns false or throws an error, the test fails. *)
let run_test (msg:string) (f:unit -> bool) : unit = assert_eqf msg f true

(* Run a test function, printing the result of the test.
   	If the function returns true or false, the test fails. If the function
   	throws an error, the test passes. *)
let run_failing_test (msg:string) (f: unit -> bool) : unit =
  let _ = print_string ("Running: "^msg^" ... ") in
  let _ = flush_all () in
  let outcome = (try (ignore (f ()) ; Fail) with _ -> Succ) in
  if !problems <> [] then
    process_outcome msg outcome
  else
    match outcome with
    | Succ -> print_endline ("Test passed!")
    | Fail -> error_mesg ("Test error: should have failed.")
    | Err _ -> error_mesg ("run_failing_test BUG: shouldn't get here.")

(******************************************************************************)
(* PROPERTY-BASED TESTING UTILITIES                                           *)
(******************************************************************************)

(* Tree type matching tree.ml's tree type structure *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Helper function to build a balanced BST from a sorted list of
   unique integers (used only if we want balanced trees). *)
let rec build_bst_from_sorted (sorted_list : int list) : int tree =
  match sorted_list with
  | [] -> Empty
  | [x] -> Node (Empty, x, Empty)
  | _ ->
      let mid = List.length sorted_list / 2 in
      let rec split_at n lst =
        if n = 0 then ([], lst)
        else match lst with
        | [] -> ([], [])
        | h :: t ->
            let (left, right) = split_at (n - 1) t in
            (h :: left, right)
      in
      let (left_part, rest) = split_at mid sorted_list in
      match rest with
      | root :: right_part ->
          Node (build_bst_from_sorted left_part, root, 
          build_bst_from_sorted right_part)
      | [] -> Empty  (* Should not happen, but handle for safety *)

(* Shrinker for int tree: try Empty, then left subtree, then right subtree,
   then recursively shrink left and right. Yields smaller trees so QCheck
   can find a minimal counterexample (more shrink steps). *)
let rec shrink_tree (t : int tree) (yield : int tree -> unit) : unit =
  match t with
  | Empty -> ()
  | Node (lt, v, rt) ->
      yield Empty;
      (match lt with Empty -> () | _ -> yield lt);
      (match rt with Empty -> () | _ -> yield rt);
      shrink_tree lt (fun lt' -> yield (Node (lt', v, rt)));
      shrink_tree rt (fun rt' -> yield (Node (lt, v, rt')))

(* Build a valid BST from a sorted list of unique ints by construction:
   at each step pick a random element as root, partition the rest into
   smaller and larger, and recursively build Node(left, root, right).
   Uses only the tree constructors Empty and Node; shape varies because
   the root index is random at each step. *)
let rec bst_from_sorted_gen (sorted : int list) : int tree QCheck.Gen.t =
  let open QCheck.Gen in
  match sorted with
  | [] -> return Empty
  | _ ->
      (* pick a root uniformly from the list (by construction, no insert) *)
      oneofl sorted >>= fun root ->
      let left_elems = List.filter (fun x -> x < root) sorted in
      let right_elems = List.filter (fun x -> x > root) sorted in
      bst_from_sorted_gen left_elems >>= fun left_t ->
      bst_from_sorted_gen right_elems >>= fun right_t ->
      return (Node (left_t, root, right_t))

(* Generic tree printer that takes an element-to-string converter *)
let tree_to_string (elem_to_string : 'a -> string) (t: 'a tree) : string =
  let rec w d = if d = 0 then "" else " " ^ w (d - 1) in
  let rec aux t d =
    w (d * 4) ^
    match t with
    | Empty -> "Empty"
    | Node (lt, n, rt) ->
      "Node ( \n" ^
      aux lt (d + 1) ^ ",\n" ^
      w ((d + 1) * 4) ^ (elem_to_string n) ^ ",\n" ^
      aux rt (d + 1) ^ "\n" ^
      w (d * 4) ^ ")"
  in "\n" ^ (aux t 0) ^ "\n"

(* Generate a valid BST by construction (Node/Empty only, no insert):
   1. Generate a list of unique integers (max length 20)
   2. Sort to get unique_sorted
   3. Build a tree via bst_from_sorted_gen: at each step pick a random
      element as root, partition into smaller/larger, recurse. Yields a
      mix of balanced and unbalanced shapes. *)
let bst_gen () : 'a QCheck.arbitrary =
  let open QCheck in
  let open Gen in
  let tree_gen = Gen.sized (fun size ->
    (* Cap the size at 20 to limit tree size *)
    let capped_size = min size 20 in
    (* Generate a list of unique small natural numbers *)
    let list_gen = Gen.list_size (Gen.return capped_size) Gen.small_nat in
    list_gen >>= fun lst ->
    (* Remove duplicates and sort - we need unique elements for a valid BST *)
    let unique_sorted = List.sort_uniq Stdlib.compare lst in
    (* Build by construction: random root choice at each step *)
    bst_from_sorted_gen unique_sorted
  ) in
  let printer (t: int tree) : string =
    Printf.sprintf "Generated BST:%s" (tree_to_string string_of_int t)
  in
  let int_tree_arb = QCheck.make ~print:printer ~shrink:shrink_tree tree_gen in
  (* Cast to polymorphic type for compatibility with tree.ml's tree type *)
  (Obj.magic int_tree_arb : 'a arbitrary)

(* Helper to add printer to arbitrary while preserving shrinking *)
let with_printer (printer : 'a -> string) (arb : 'a QCheck.arbitrary)
    : 'a QCheck.arbitrary =
  let open QCheck in
  let module M = struct
    type 'b t = {gen : 'b Gen.t; shrink : ('b -> 'b Iter.t) option}
  end in
  let {M.gen; shrink} = (Obj.magic arb : 'a M.t) in
  match shrink with
  | Some s -> QCheck.make ~print:printer ~shrink:s gen
  | None -> QCheck.make ~print:printer gen

(* Integer generator WITHOUT shrinker to demonstrate raw failure cases.  Use
   with run_property_test gen_int_no_shrinker to show un-shrunk
   counterexamples. *)
let int_no_shrink : int QCheck.arbitrary =
  let open QCheck in
  make ~print:Print.int Gen.small_signed_int
  (* No ~shrink parameter = no shrinker *)

(* Generic list printer: takes an element-to-string converter and a list.
   Returns a string like "[a; b; c]". *)
let string_of_list (elem_to_string : 'a -> string) (lst : 'a list) : string =
  "[" ^ (String.concat "; " (List.map elem_to_string lst)) ^ "]"


(* English-only string generator: ASCII printable (32-126 + newline),
   max len 20. *)
let english_string_gen =
  QCheck.Gen.string_size ~gen:QCheck.Gen.printable (QCheck.Gen.int_bound 21)

let english_string_arb : string QCheck.arbitrary =
  let open QCheck in
  make ~print:(fun s -> "\"" ^ s ^ "\"")
       ~shrink:(Shrink.string ~shrink:Shrink.char_printable)
       english_string_gen

(******************************************************************************)
(* PROPERTY-BASED TESTING ABSTRACTION                                         *)
(******************************************************************************)

(* Generator spec: carries a QCheck arbitrary and a view function. *)
type 'a gen_spec = Gen : 'b QCheck.arbitrary * ('b -> 'a) -> 'a gen_spec

let id x = x

let int_set_arb () : 'a QCheck.arbitrary =
  match !int_set_registry with
  | Some arb_obj -> (Obj.magic (Obj.obj arb_obj) : 'a QCheck.arbitrary)
  | None ->
     failwith "run_property_test: gen_set gen_int not registered. 
     Call Assert.register_int_set first."

(* Composable generators *)
let gen_pair (Gen (arb1, f1)) (Gen (arb2, f2)) : ('a * 'b) gen_spec =
  let open QCheck in
  Gen (pair arb1 arb2, fun (a, b) -> (f1 a, f2 b))

let gen_triple (Gen (arb1, f1)) (Gen (arb2, f2)) (Gen (arb3, f3)) : 
('a * 'b * 'c) gen_spec =
  let open QCheck in
  Gen (pair (pair arb1 arb2) arb3, fun ((a, b), c) -> (f1 a, f2 b, f3 c))

let gen_list (Gen (arb, view)) : 'a list gen_spec =
  let open QCheck in
  Gen (list arb, fun lst -> List.map view lst)

(* List-of-elements generator WITHOUT shrinker (no list shrinker).  Use to
   show un-shrunk counterexamples, e.g. (gen_list_no_shrinker gen_int_no_shrinker). *)
let gen_list_no_shrinker (Gen (arb, view)) : 'a list gen_spec =
  let open QCheck in
  let module M = struct
    type 'b t = { gen : 'b Gen.t; shrink : ('b -> 'b Iter.t) option; print : ('b -> string) option }
  end in
  let { M.gen = elem_gen; M.print = elem_print } = (Obj.magic arb : 'b M.t) in
  let list_gen = Gen.list elem_gen in
  (* Use string_of_int when this list is built from int_no_shrink (extracted print often wrong) *)
  let elem_to_string =
    if arb == (Obj.magic int_no_shrink) then (Obj.magic string_of_int : 'b -> string)
    else Option.value elem_print ~default:(fun _ -> "?")
  in
  let list_arb = make ~print:(string_of_list elem_to_string) list_gen in
  (* Coerce closure so existential from Gen(arb,view) does not escape *)
  let view_list = (Obj.magic (fun lst -> List.map view lst) : 'b list -> 'a list) in
  Gen (Obj.magic list_arb, view_list)

(* gen_tree takes an element generator; we only build int trees (BST). Use
   gen_tree gen_int.  Return type is 'b gen_spec so that at use site
   (e.g. tree.ml) 'b unifies with Tree.tree. *)
let gen_tree (_ : 'a gen_spec) : 'b gen_spec =
  Obj.magic (Gen (bst_gen (), id) : int tree gen_spec)

(* gen_set uses the registered set arbitrary (from set module). Use gen_set
   gen_int after register_int_set. *)
let gen_set (_ : 'a gen_spec) = Gen (int_set_arb (), id)

(* Base and demo generators *)
let gen_int : int gen_spec =
  let open QCheck in
  Gen (small_signed_int, id)

let gen_int_no_shrinker : int gen_spec = Gen (int_no_shrink, id)
let gen_string : string gen_spec = Gen (english_string_arb, id)

(* Property-based testing: 100 test cases. *)
let run_property_test (type a) (desc : string) (Gen (arb, view) : a gen_spec) 
(prop : a -> bool) : unit =
  let count = 100 in
  let open QCheck in
  let open QCheck_runner in
  let prop' x = prop (view x) in
  let test = Test.make ~count ~name:desc arb prop' in
  pbt_start desc;
  let n_fail = run_tests ~verbose:true [test] in
  pbt_footer desc n_fail;
  if not grading_mode && n_fail > 0 && get_stop_on_failure_flag () then exit 1

(* Print first [num_samples] generated values for a generator. *)
let print_samples (type a) (Gen (arb, view) : a gen_spec) 
(num_samples : int) (printer : a -> string) : unit =
  let desc = "sample generator output" in
  pbt_start desc;
  let _ = flush_all () in
  let count = ref 0 in
  let property (x : _) : bool =
    if !count < num_samples then (
      print_endline ("  Generated: " ^ printer (view x));
      count := !count + 1
    );
    true
  in
  let open QCheck in
  let test = Test.make ~count:100 ~name:desc arb property in
  Test.check_exn test;
  pbt_footer desc 0  (* samples run: no failure *)

