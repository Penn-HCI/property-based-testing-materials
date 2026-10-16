(*****************************************************************************)
(* COUNTER IMPLEMENTATION                                                    *)
(*****************************************************************************)

(* A simple counter implementation using a mutable record.
   The counter value cannot go below 0. *)

   module Counter : CounterInterface.COUNTER = struct

    type counter = { mutable value : int }
  
    let create () : counter = { value = 0 }
  
    let increment (c : counter) : unit =
      c.value <- c.value + 1
  
    let decrement (c : counter) : unit =
      if c.value > 0 then
        c.value <- c.value - 1
      (* If counter is already 0, do nothing - don't allow negative values *)
  
    let reset (c : counter) : unit =
      c.value <- 0
  
    let view (c : counter) : int =
      c.value
  
    let debug_name = "Counter"
  
  end
  