(******************************************************************************)
(* ABSTRACT COUNTER INTERFACE                                                *)
(******************************************************************************)

(* This module signature defines the values associated with an abstract
   counter type. Any module struct that implements the `COUNTER` interface
   must contain all of the values defined in this interface.
   
   A counter is a simple abstraction that supports four operations: increment,
   decrement, reset, and view. The counter value cannot go below 0. *)

   module type COUNTER = sig

    (* The "abstract type" counter. *)
    type counter
  
    (* `create ()` creates a new counter initialized to 0 *)
    val create : unit -> counter
  
    (* `increment c` increments the counter by 1 *)
    val increment : counter -> unit
  
    (* `decrement c` decrements the counter by 1 *)
    val decrement : counter -> unit
  
    (* `reset c` resets the counter to 0 *)
    val reset : counter -> unit
  
    (* `view c` returns the current value of the counter *)
    val view : counter -> int
  
    (* For testing and debugging *)
    val debug_name : string
  
  end