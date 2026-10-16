(* CIS 120 OCaml testing interface      *)
(* You do not need to modify this file. *)
(* ------------------------------------ *)

open Printf
open List

(* The result of a test *)
type result =
  | Succ             (* Test succeeds *)
  | Fail             (* Test fails normally, usually returning false *)
  | Err of string    (* Test throws an error, indicated *)

(* When set to true, stop immediately on a failing test *)
let stop_on_failure_flag = ref false

let stop_on_failure ()     = stop_on_failure_flag := true
let continue_on_failure () = stop_on_failure_flag := false

(* Export the flag for use by property-based tests *)
let get_stop_on_failure_flag () = !stop_on_failure_flag

(* Display a message about a failing test *)
let error_mesg (s: string) : unit =
  print_endline s;
  if !stop_on_failure_flag then exit 1 else ()

(* Make sure that a test case does not run too long. 
   Raises Failure on a timeout after t seconds. *)
exception Timeout
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

(* Run a testing function (actual_fcn) and compare its value with
   an expected value, using OCaml's polymorphic structural equality. *)
let assert_eqf (msg: string) (actual_fcn: unit -> 'a) (expected: 'a) : unit =
  let _ = print_string ("Running: "^msg^" ... ") in
  let _ = flush_all () in
  let outcome = try if expected = run_with_timeout 30 actual_fcn () then Succ else Fail
    with Failure s -> Err s
       | e         -> Err (Printexc.to_string e) in

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
  let result = (try (ignore (f ()) ; Fail) with
      | _ -> Succ) in
  match result with
  | Succ -> print_endline ("Test passed!")
  | Fail -> error_mesg ("Test error: should have failed.")
  | Err s -> error_mesg ("run_failing_test BUG: shouldn't get here.")

(******************************************************************************)
(* PROPERTY-BASED TESTING UTILITIES                                           *)
(******************************************************************************)

let sep_line = String.make 79 '#'

let pbt_start (desc : string) : unit =
  print_endline "";
  print_string "\x1b[34m";
  print_endline sep_line;
  print_string "\x1b[0m";
  print_endline ("PBT: " ^ desc);
  print_endline ""

let pbt_footer (desc : string) (n_fail : int) : unit =
  print_endline "";
  if n_fail = 0 then
    print_endline ("  --- " ^ desc ^ ": PASSED ---")
  else
    print_endline ("  *** " ^ desc ^ ": FAILED (" ^ string_of_int n_fail ^ " failure(s)) ***");
  print_endline (String.make 79 '-');
  print_endline ""

(* Helper to add printer to arbitrary while preserving shrinking *)
let with_printer (printer : 'a -> string) (arb : 'a QCheck.arbitrary) : 'a QCheck.arbitrary =
  let open QCheck in
  let module M = struct
    type 'b t = { gen : 'b Gen.t; shrink : ('b -> 'b Iter.t) option }
  end in
  let { M.gen; shrink } = (Obj.magic arb : 'a M.t) in
  match shrink with
  | Some s -> QCheck.make ~print:printer ~shrink:s gen
  | None -> QCheck.make ~print:printer gen

(* Integer generator WITHOUT shrinker *)
let int_no_shrink : int QCheck.arbitrary =
  let open QCheck in
  make ~print:Print.int Gen.small_signed_int

let string_of_list (elem_to_string : 'a -> string) (lst : 'a list) : string =
  "[" ^ (String.concat "; " (List.map elem_to_string lst)) ^ "]"

(* English-only string generator *)
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

type 'a gen_spec = Gen : 'b QCheck.arbitrary * ('b -> 'a) -> 'a gen_spec

let id x = x

let gen_pair (Gen (arb1, f1)) (Gen (arb2, f2)) : ('a * 'b) gen_spec =
  let open QCheck in
  Gen (pair arb1 arb2, fun (a, b) -> (f1 a, f2 b))

let gen_triple (Gen (arb1, f1)) (Gen (arb2, f2)) (Gen (arb3, f3)) : ('a * 'b * 'c) gen_spec =
  let open QCheck in
  Gen (pair (pair arb1 arb2) arb3, fun ((a, b), c) -> (f1 a, f2 b, f3 c))

let gen_list (Gen (arb, view)) : 'a list gen_spec =
  let open QCheck in
  Gen (list arb, fun lst -> List.map view lst)

let gen_list_max ?(print : ('a list -> string) option) (max_len : int) (Gen (arb, view)) : 'a list gen_spec =
  let open QCheck in
  let module M = struct
    type 'b t = { gen : 'b Gen.t; shrink : ('b -> 'b Iter.t) option; print : ('b -> string) option }
  end in
  let { M.gen = elem_gen; M.shrink = elem_shrink; M.print = elem_print } = (Obj.magic arb : 'b M.t) in
  let list_gen = Gen.list_size (Gen.int_bound max_len) elem_gen in
  let list_print =
    match print with
    | Some p -> (Obj.magic p : 'b list -> string)
    | None ->
        let elem_to_string =
          if arb == (Obj.magic int_no_shrink) then (Obj.magic string_of_int : 'b -> string)
          else Option.value elem_print ~default:(fun _ -> "?")
        in
        string_of_list elem_to_string
  in
  let list_shrink =
    match elem_shrink with
    | None -> (Shrink.list : 'b list Shrink.t)
    | Some s -> Shrink.list ~shrink:s
  in
  let list_arb = make ~print:list_print ~shrink:list_shrink list_gen in
  let view_list = (Obj.magic (fun lst -> List.map view lst) : 'b list -> 'a list) in
  Gen (Obj.magic list_arb, view_list)

let gen_pure (x : 'a) : 'a gen_spec =
  let open QCheck in
  Gen (always x, id)

let gen_map (Gen (arb, view)) (f : 'a -> 'b) : 'b gen_spec =
  let open QCheck in
  Gen (map (fun x -> f (view x)) arb, id)

let gen_oneof (specs : 'a gen_spec list) : 'a gen_spec =
  let open QCheck in
  match specs with
  | [] -> invalid_arg "gen_oneof: list must be non-empty"
  | [ Gen (arb, view) ] -> Gen (arb, view)
  | _ ->
      let arbs = List.map (fun (Gen (arb, view)) -> map view arb) specs in
      Gen (oneof arbs, id)

(******************************************************************************)
(* OP-LIST GENERATOR FUNCTOR                                                  *)
(******************************************************************************)

module type OP_DESC = sig
  type 'a op
  type which
  val specs_for : which list -> 'a gen_spec -> 'a op gen_spec list
  val string_of_op : 'a op -> string
end

let op_list_max_len = 50

module MakeOpGen (D : OP_DESC) = struct
  (* [gen_ops which gen_elem]: no-elem ops use [gen_unit], e.g. [gen_ops [Increment; Reset] gen_unit];
     with-elem ops use the generator, e.g. [gen_ops [Enq; Deq] gen_int]. *)
  let gen_ops (which : D.which list) (gen_elem : 'a gen_spec) : 'a D.op list gen_spec =
    let op_specs = D.specs_for which gen_elem in
    match op_specs with
    | [] -> invalid_arg "MakeOpGen.gen_ops: which list must be non-empty"
    | _ ->
        let list_print (lst : 'a D.op list) : string =
          "[" ^ String.concat ", " (List.map D.string_of_op lst) ^ "]"
        in
        gen_list_max ~print:list_print op_list_max_len (gen_oneof op_specs)
end

let gen_int : int gen_spec =
  let open QCheck in
  Gen (small_signed_int, id)

let gen_string : string gen_spec = Gen (english_string_arb, id)

(* For ops with no element type; use [gen_ops [Increment; Reset] gen_unit]. *)
let gen_unit : unit gen_spec = gen_pure ()

let run_property_test (type a) (desc : string) (Gen (arb, view) : a gen_spec) (prop : a -> bool) : unit =
  let count = 100 in
  let open QCheck in
  let open QCheck_runner in
  let prop' x = prop (view x) in
  let test = Test.make ~count ~name:desc arb prop' in
  pbt_start desc;
  let n_fail = run_tests ~verbose:true [ test ] in
  pbt_footer desc n_fail;
  if n_fail > 0 && get_stop_on_failure_flag () then exit 1

let print_samples (type a) (Gen (arb, view) : a gen_spec) (num_samples : int) (printer : a -> string) : unit =
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
  pbt_footer desc 0
