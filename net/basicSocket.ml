(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)
    
type event = 
| CLOSED of string (* called when a task has been closed *)
| RTIMEOUT (* called after a timeout on reading *)
| WTIMEOUT (* called after a timeout on writing *)
| LTIMEOUT (* called after a timeout on lifetime *)
| CAN_READ (* called when read is possible *)
| CAN_WRITE (* called when write is possible *)

type t = {
(* DON'T CHANGE THE ORDER OF THESE FOUR FIELDS !!! *)
    mutable fd : Unix.file_descr;
    mutable flags : int;
    mutable want_to_write : bool;
    mutable want_to_read : bool;
    mutable closed : bool;
    
(* YOU CAN MODIFY THESE *)
    mutable rtimeout: float;
    mutable next_rtimeout : float;
    
    mutable wtimeout: float;
    mutable next_wtimeout : float;
    
    mutable lifetime : float;
    
    mutable event_handler : handler;
    mutable error : string;
    
    mutable before_select : (t -> unit);
  }

and handler = t -> event -> unit

type timer = {
    mutable next_time : float;
    time_handler : timer -> unit;
    mutable applied : bool;
    delay : float;
  }

let nb_sockets = ref 0
  
let infinite_timeout = 3600. *. 24. *. 365. (* one year ! *)

let current_time = ref (Unix.gettimeofday ())

let update_time () =
  current_time := Unix.gettimeofday ();
  !current_time

let last_time () = !current_time

let fd t = t.fd

let must_write t b  = t.want_to_write <- b
let must_read t b  = t.want_to_read <- b

let set_before_select t f = t.before_select <- f
    
let dummy_fd = Obj.magic (-1)

let closed_tasks = ref []
  
let close t msg =
  if t.fd <> dummy_fd then begin
      (try Unix.close t.fd with _ -> ());
      t.fd <- dummy_fd;
      closed_tasks := t :: !closed_tasks;
      t.closed <- true;
      t.error <- msg;
      decr nb_sockets
    end

let closed t = t.closed

let set_rtimeout t time =
  t.rtimeout <- time;
  t.next_rtimeout <- time +. !current_time

let set_wtimeout t time =
  t.wtimeout <- time;
  t.next_wtimeout <- time +. !current_time

let set_lifetime t time =
  t.lifetime <- time +. !current_time 

let set_handler t handler =
  t.event_handler <- handler

let handler t = t.event_handler

let tasks  = ref ([]: t list)

let before_select_hooks = ref []
  
let set_before_select_hook f =
  before_select_hooks := f :: !before_select_hooks

let after_select_hooks = ref []
  
let set_after_select_hook f =
  after_select_hooks := f :: !after_select_hooks
  
let default_before_select t = ()
  
let create_blocking fd handler =
  incr nb_sockets;
  Unix.set_nonblock fd;
(*  Printf.printf "NEW FD %d" (Obj.magic fd); print_newline (); *)
  current_time := Unix.gettimeofday ();
  let t = {
      fd = fd;
      
      want_to_write = false;
      want_to_read = true;
      closed = false;
      
      flags = 0;
      
      rtimeout = infinite_timeout;
      next_rtimeout = !current_time +. infinite_timeout;
      
      wtimeout = infinite_timeout;
      next_wtimeout = !current_time +. infinite_timeout;
      
      lifetime = !current_time +. infinite_timeout;

      event_handler = handler;
      error = "";
      before_select = default_before_select;
    } in
(*  Printf.printf "ADD ONE TASK"; print_newline (); *)
  tasks := t :: !tasks; 
  t

  
let create fd handler =
  Unix.set_nonblock fd;
  create_blocking fd handler
  
external select: t list -> float -> unit = "ml_select"

let timeout = ref infinite_timeout
let timers = ref []
  
let rec iter_task old_tasks time =
  match old_tasks with
    [] -> []
  | t :: old_tail ->
(*      Printf.printf "NEXT TASK"; print_newline (); *)
      let tail = iter_task old_tail time in
      if t.closed then 
          tail
        else
        begin
          t.before_select t;
          
          if t.want_to_write then 
            timeout := min (t.next_wtimeout -. time) !timeout;
          
          if t.want_to_read then
            timeout := min (t.next_rtimeout -. time) !timeout;
(*          Printf.printf "NEXT TIMEOUT: %f/%f" t.next_rtimeout time;
print_newline ();
  *)
          if old_tail == tail then old_tasks else t :: tail
        end

let rec iter_timer timers time =
  match timers with
    [] -> []
  | t :: timers ->
      if t.applied then
        iter_timer timers time
      else
        begin
          timeout := min (t.next_time -. time) !timeout;
          t :: (iter_timer timers time)
        end
          
let add_timer delay f =
  timers := {
    next_time = last_time () +. delay;
    time_handler = f;
    applied = false;
    delay = delay;
  } :: !timers
  
let reactivate_timer t =
  if t.applied then begin
      t.next_time <- last_time () +. t.delay;
      t.applied <- false;
    end
    
let can_read = 1
let can_write = 2
    
let loop () =
  Sys.set_signal  Sys.sigpipe Sys.Signal_ignore;
  while true do
    try
      let time = update_time () in
      List.iter (fun t -> 
          if not t.closed && t.next_rtimeout < time then 
            (try t.event_handler t RTIMEOUT with _ -> ());
          if not t.closed && t.next_wtimeout < time then 
            (try t.event_handler t WTIMEOUT with _ -> ());
          if not t.closed && t.lifetime < time then 
            (try t.event_handler t LTIMEOUT with _ -> ());
          if not t.closed && t.flags land can_read <> 0 then 
            (try 
                t.next_rtimeout <- time +. t.rtimeout;
                t.event_handler t CAN_READ with _ -> ());
          if not t.closed && t.flags land can_write <> 0 then 
            (try 
                t.next_wtimeout <- time +. t.wtimeout;                
                t.event_handler t CAN_WRITE with _ -> ());
      ) !tasks;
      List.iter (fun f -> try f () with _ -> ()) !after_select_hooks;
      List.iter (fun t ->
          if (not t.applied) && t.next_time <= !current_time then begin
              t.applied <- true;
              try t.time_handler t with _ -> ()
            end
      ) !timers;

      
      while !closed_tasks <> [] do
        match !closed_tasks with
          [] -> ()
        | t :: tail ->
            closed_tasks := tail;
            (try t.event_handler t
                      (CLOSED t.error) with _ -> ());
      done;
      
(*      Printf.printf "before iter_timer"; print_newline (); *)
      let time = update_time () in
      timeout := infinite_timeout;
      timers := iter_timer !timers time;
(*      Printf.printf "before iter_task"; print_newline ();*)
      tasks := iter_task !tasks time;

(*      Printf.printf "timeout %f" !timeout; print_newline (); *)
      
      if !timeout < 0.0001 then timeout := 0.0001;
(*      
      Printf.printf "TASKS: %d" (List.length !tasks); print_newline ();
      Printf.printf "TIMEOUT: %f" !timeout; print_newline ();
timeout := 5.;
*)
      List.iter (fun f -> try f () with _ -> ()) !before_select_hooks;
      select !tasks !timeout; 
    with e ->
        Printf.printf "Exception %s in Select.loop" (Printexc.to_string e);
        print_newline ();
  done

  
  
let shutdown t s =
  if t.fd <> dummy_fd then begin
(*      Printf.printf "SHUTDOWN"; print_newline (); *)
      (try Unix.shutdown t.fd Unix.SHUTDOWN_ALL with _ -> ());
      close t s
    end
    
let nb_sockets () = !nb_sockets
  
let _ =
  Printexc.register_exn (fun e ->
      match e with
        Unix.Unix_error (e, f, arg) ->
          Printf.sprintf "%s failed%s: %s" f (if arg = "" then "" else 
              "on " ^ arg) (Unix.error_message e)
      | _ -> raise e
  )
  
  