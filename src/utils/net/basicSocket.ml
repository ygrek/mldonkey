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

open Unix
open Printf2
open Options

(*************************************************************************)
(*                                                                       *)
(*                         TYPES                                         *)
(*                                                                       *)
(*************************************************************************)

type close_reason =
    Closed_for_timeout    (* timeout exceeded *)
  | Closed_for_lifetime   (* lifetime exceeded *)
  | Closed_by_peer        (* end of file *)
  | Closed_for_error of string
  | Closed_by_user
  | Closed_for_overflow
  | Closed_connect_failed
  | Closed_for_exception of exn

type event =
| CLOSED of close_reason
| RTIMEOUT  (* called after a timeout on reading *)
| WTIMEOUT  (* called after a timeout on writing *)
| LTIMEOUT  (* called after a timeout on lifetime *)
| CAN_READ  (* called when read is possible *)
| CAN_WRITE (* called when write is possible *)

type t = {
(* DON'T CHANGE THE ORDER OF THESE FOUR FIELDS !!! *)
    mutable fd : Unix.file_descr;
    mutable flags : int;
    mutable want_to_write : bool;
    mutable want_to_read : bool;
    mutable closed : bool;
    mutable pollpos : int;
    mutable read_allowed : bool ref;
    mutable write_allowed : bool ref;

(* YOU CAN MODIFY THESE *)
    mutable rtimeout: float;
    mutable next_rtimeout : float;

    mutable wtimeout: float;
    mutable next_wtimeout : float;

    mutable lifetime : float;

    mutable event_handler : handler;
    mutable error : close_reason;

    mutable name : string;
    mutable printer : (t -> string);
    born : float;
    mutable dump_info : (Buffer.t -> unit);
(*    mutable before_select : (t -> unit); *)
    mutable can_close : bool;
  }

and handler = t -> event -> unit

type timer = {
    mutable next_time : float;
    time_handler : timer -> unit;
    mutable applied : bool;
    mutable delay : float;
  }


(*************************************************************************)
(*                                                                       *)
(*                         EXTERNALS                                     *)
(*                                                                       *)
(*************************************************************************)

external change_fd_event_setting : t -> unit = "ml_change_fd_event_setting"  "noalloc"
external add_fd_to_event_set : t -> unit = "ml_add_fd_to_event_set"  "noalloc"
external remove_fd_from_event_set : t -> unit = "ml_remove_fd_from_event_set"  "noalloc"

external get_fd_num : Unix.file_descr -> int = "ml_get_fd_num" "noalloc"

external select: t list -> float -> unit = "ml_select"
external use_poll : bool -> unit = "ml_use_poll"

external setsock_iptos_throughput: Unix.file_descr -> int = "ml_setsock_iptos_throughput"

(*************************************************************************)
(*                                                                       *)
(*                         Global values                                 *)
(*                                                                       *)
(*************************************************************************)

let can_read = 1
let can_write = 2


let debug = ref false
let nb_sockets = ref 0
let allow_read = ref true
let allow_write = ref true

(*let infinite_timeout = 3600. *. 24. *. 365. (* one year ! *) *)
let infinite_timeout = float_of_int Date.year_in_secs
let current_time = ref (Unix.gettimeofday ())
let last_time = ref (int_of_float (!current_time -. 1000000000.))

let dummy_fd = Obj.magic (-1)
let closed_tasks = ref []
let fd_tasks  = ref ([]: t list)
let before_select_hooks = ref []
let after_select_hooks = ref []
let timeout = ref infinite_timeout
let timers = ref []
let loop_delay = ref 0.005
let socket_keepalive = ref false
let verbose_bandwidth = ref 0
let bandwidth_second_timers = ref []


(*************************************************************************)
(*                                                                       *)
(*                         Accessors                                     *)
(*                                                                       *)
(*************************************************************************)

let fd t = t.fd
(* let set_before_select t f = t.before_select <- f *)

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


let set_before_select_hook f =
  before_select_hooks := f :: !before_select_hooks

let set_after_select_hook f =
  after_select_hooks := f :: !after_select_hooks


let add_bandwidth_second_timer f =
  bandwidth_second_timers := f :: !bandwidth_second_timers

(*************************************************************************)
(*                                                                       *)
(*                         PRINTERS                                      *)
(*                                                                       *)
(*************************************************************************)

let string_of_reason c =
  match c with
    Closed_for_timeout -> "timeout"
  | Closed_for_lifetime -> "lifetime"
  | Closed_by_peer -> "peer"
  | Closed_for_error error -> Printf.sprintf "error %s" error
  | Closed_by_user -> "user"
  | Closed_for_overflow -> "overflow"
  | Closed_connect_failed -> "connect failed"
  | Closed_for_exception e -> Printf.sprintf "exception %s"
        (Printexc2.to_string e)

let string_of_basic_event = function
| CLOSED s -> Printf.sprintf "CLOSED %s" (string_of_reason s)
| RTIMEOUT -> "RTIMEOUT"
| LTIMEOUT -> "LTIMEOUT"
| WTIMEOUT -> "WTIMEOUT"
| CAN_READ -> "CAN_READ"
| CAN_WRITE -> "CAN_WRITE"

(*************************************************************************)
(*                                                                       *)
(*                         Simple functions                              *)
(*                                                                       *)
(*************************************************************************)

let minf (x: float) (y: float) =
  if x > y then y else x

let maxf (x: float) (y: float) =
  if x < y then y else x

(*************************************************************************)
(*                                                                       *)
(*                         Some functions                                *)
(*                                                                       *)
(*************************************************************************)

let set_allow_read s ref =
  s.read_allowed <- ref;
  change_fd_event_setting s

let set_allow_write s ref =
  s.write_allowed <- ref;
  change_fd_event_setting s

let update_time () =
  current_time := Unix.gettimeofday ();
  last_time := (int_of_float (!current_time -. 1000000000.));
  !current_time

let must_write t b  =
  if b <> t.want_to_write then begin
    t.want_to_write <- b;
    change_fd_event_setting t
  end

let must_read t b  =
  if b <> t.want_to_read then begin
    t.want_to_read <- b;
    change_fd_event_setting t
  end

let print_socket buf s =
  Printf.bprintf buf "FD %s:%d"
     (s.name) (get_fd_num s.fd)

let sprint_socket s =
  let buf = Buffer.create 100 in
  print_socket buf s;
  Buffer.contents buf

(*************************************************************************)
(*                                                                       *)
(*                         close                                         *)
(*                                                                       *)
(*************************************************************************)

let exn_log name f x = 
  try 
    f x
  with e -> 
    lprintf_nl "[bS] %s : unexpected exn %s" name (Printexc2.to_string e)

let close t msg =
  if t.fd <> dummy_fd then begin
      if !debug then
          lprintf_nl "[bS] CLOSING: %s (%s)" (sprint_socket t) (string_of_reason msg);
      exn_log "close" Unix.close t.fd;
      t.fd <- dummy_fd;
      closed_tasks := t :: !closed_tasks;
      t.closed <- true;
      t.error <- msg;
      decr nb_sockets
    end

let default_before_select t = ()

let dump_basic_socket buf = ()


(*************************************************************************)
(*                                                                       *)
(*                         create_blocking                               *)
(*                                                                       *)
(*************************************************************************)

let create_blocking name fd handler =

  let (_ : int) = get_fd_num fd in
(*
  if fdnum >= Unix32.fds_size then begin
      Unix.close fd;
      lprintf "**** File descriptor above limit %d!!\n" fdnum;
      failwith "File Descriptor removed";
    end;
*)
  incr nb_sockets;
  MlUnix.set_nonblock fd;
(*  lprintf "NEW FD %d\n" (Obj.magic fd); *)
  ignore (update_time ());
  let t = {
      fd = fd;

      want_to_write = false;
      want_to_read = true;
      closed = false;
      pollpos = -1;
      flags = 0;

      rtimeout = infinite_timeout;
      next_rtimeout = !current_time +. infinite_timeout;

      wtimeout = infinite_timeout;
      next_wtimeout = !current_time +. infinite_timeout;

      lifetime = !current_time +. infinite_timeout;

      read_allowed = allow_read;
      write_allowed = allow_write;

      event_handler = handler;
      error = Closed_by_peer;
(*      before_select = default_before_select; *)
      name = name;
      printer = (fun _ -> "");
      born = !current_time;

      dump_info = dump_basic_socket;
      can_close = true;
    } in
(*  lprintf "ADD ONE TASK\n"; *)
  if !debug then
      lprintf_nl "[bS] OPENING: %s" ( sprint_socket t);
  fd_tasks := t :: !fd_tasks;
  add_fd_to_event_set t;
  t



(*************************************************************************)
(*                                                                       *)
(*                         create                                        *)
(*                                                                       *)
(*************************************************************************)

let create name fd handler =
  MlUnix.set_nonblock fd;
  if not (Autoconf.windows || Autoconf.system="solaris") then setsockopt fd SO_KEEPALIVE !socket_keepalive;
  create_blocking name fd handler


(*************************************************************************)
(*                                                                       *)
(*                         iter_task                                     *)
(*                                                                       *)
(*************************************************************************)

let rec iter_task old_tasks time =
  match old_tasks with
    [] -> ()
  | t :: old_tail ->
(*      lprintf "NEXT TASK\n"; *)
      if t.closed then begin
          if t.fd <> dummy_fd then remove_fd_from_event_set t;
          iter_task old_tail time
       end else
        begin
          fd_tasks := t :: !fd_tasks;

(*          t.before_select t; *)

          if t.want_to_write then
            timeout := minf (t.next_wtimeout -. time) !timeout;

          if t.want_to_read then
            timeout := minf (t.next_rtimeout -. time) !timeout;

          iter_task old_tail time;
        end

(*************************************************************************)
(*                                                                       *)
(*                         iter_timer                                    *)
(*                                                                       *)
(*************************************************************************)

let rec iter_timer_filter timers time acc =
  match timers with
    [] -> acc
  | t :: timers ->
      if t.applied then
        iter_timer_filter timers time acc
      else
        begin
          timeout := minf (t.next_time -. time) !timeout;
          iter_timer_filter timers time (t::acc)
        end

(* fast version that doesn't allocate new list if no timers have expired
  TODO use double-linked list instead? *)
let iter_timer timers time =
  let rec loop l =
    match l with
    | [] -> timers
    | t :: l ->
      if t.applied then
        (* need to filter, reiterate and rebuild the list *)
        iter_timer_filter timers time []
      else
        begin
          timeout := minf (t.next_time -. time) !timeout;
          loop l
        end
  in
  loop timers

(*************************************************************************)
(*                                                                       *)
(*                         TIMERS                                        *)
(*                                                                       *)
(*************************************************************************)

let add_timer delay f =
  timers := {
    next_time = !current_time +. delay;
    time_handler = f;
    applied = false;
    delay = delay;
  } :: !timers

let reactivate_timer t =
  if t.applied then begin
      t.next_time <- !current_time +. t.delay;
      t.applied <- false;
    end

let add_session_timer enabler delay f =
  let f t =
    if !enabler then begin
        reactivate_timer t;
        f ()
      end
  in
  add_timer delay f


let add_infinite_timer delay f = add_session_timer (ref true) delay f

let add_session_option_timer enabler option f =
  let f t =
    if !enabler then
      let delay = !!option in
(* Negative delays are re-examined every minute, and the timer is only called
     if the delay becomes positive again. *)
      t.delay <- (if delay > 0. then delay else 60.);
      reactivate_timer t;
      if delay > 0. then f ()
  in
  add_timer !!option f

let add_infinite_option_timer option f =
  add_session_option_timer (ref true) option f

(*************************************************************************)
(*                                                                       *)
(*                         iter_hooks                                    *)
(*                                                                       *)
(*************************************************************************)

let rec exec_hooks list =
  match list with
    [] -> ()
  | f :: tail ->
      exn_log "hook" f ();
      exec_hooks tail

(*************************************************************************)
(*                                                                       *)
(*                         exec_tasks                                    *)
(*                                                                       *)
(*************************************************************************)

let rec exec_tasks =
  function [] -> ()
  | t :: tail ->
      (
        let time = !current_time in
        if not t.closed && t.next_rtimeout < time then
          exn_log "exec rtimeout" (t.event_handler t) RTIMEOUT;
        if not t.closed && t.next_wtimeout < time then
          exn_log "exec wtimeout" (t.event_handler t) WTIMEOUT;
        if not t.closed && t.lifetime < time then
          exn_log "exec ltimeout" (t.event_handler t) LTIMEOUT;
        if not t.closed && t.flags land can_read <> 0 then
          begin try
              t.next_rtimeout <- time +. t.rtimeout;
              t.event_handler t CAN_READ
          with 
          | Unix_error ((ECONNRESET | ETIMEDOUT | ECONNREFUSED), _, _) -> ()
          | exn -> lprintf_nl "[bS] %s : unexpected exn exec can_read" (Printexc2.to_string exn)
          end;
        if not t.closed && t.flags land can_write <> 0 then
          exn_log "exec can_write" (fun () ->
              t.next_wtimeout <- time +. t.wtimeout;
              t.event_handler t CAN_WRITE) ();
      );
      exec_tasks tail

(*************************************************************************)
(*                                                                       *)
(*                         exec_timers                                   *)
(*                                                                       *)
(*************************************************************************)

let rec exec_timers = function
    [] -> ()
  | t :: tail ->
      (
        if (not t.applied) && t.next_time <= !current_time then begin
            t.applied <- true;
            begin try t.time_handler t with _ -> () end (* exn_log -> many Fifo.empty *)
          end
      );
      exec_timers tail

(*************************************************************************)
(*                                                                       *)
(*                         loop                                          *)
(*                                                                       *)
(*************************************************************************)

let loop () =
  add_infinite_timer 1.0 (fun _ ->
      if !verbose_bandwidth > 0 then
        lprintf_nl "[BW1] Resetting bandwidth counters";
      List.iter (fun f -> exn_log "reset bw ctr" f ()) !bandwidth_second_timers
  );
  while true do
    try
      if !loop_delay > 0. then (try select [] !loop_delay;  with _ -> ());
      let _ = update_time () in
      exec_tasks !fd_tasks;
      exec_hooks !after_select_hooks;
      exec_timers !timers;

      while !closed_tasks <> [] do
        match !closed_tasks with
          [] -> ()
        | t :: tail ->
            closed_tasks := tail;
            exn_log "exec closed" (t.event_handler t) (CLOSED t.error)
      done;

(*      lprintf "before iter_timer\n"; *)
      let time = update_time () in
      timeout := infinite_timeout;
      timers := iter_timer !timers time;
(*      lprintf "before iter_task\n"; *)

      let old_tasks = !fd_tasks in
      fd_tasks := [];
      iter_task old_tasks time;

(*      lprintf "timeout %f\n" !timeout; *)

      if !timeout < 0.01 then timeout := 0.01;
(*
      lprintf "TASKS: %d\n" (List.length !tasks);
      lprintf "TIMEOUT: %f\n" !timeout;
timeout := 5.;
*)
      exec_hooks !before_select_hooks;
(*      lprintf "Tasks %d\n" (List.length !fd_tasks); *)
      try select !fd_tasks !timeout with _ -> ();
    with
    | e ->
        lprintf_nl "Exception %s in Select.loop" (Printexc2.to_string e);
  done


let shutdown t s =
  if t.fd <> dummy_fd then begin
(*      lprintf "SHUTDOWN\n";  *)
      (try Unix.shutdown t.fd Unix.SHUTDOWN_ALL with _ -> ());
      close t s
    end

let nb_sockets () = !nb_sockets

let stats buf t =
  lprintf_nl "Socket %d" (get_fd_num t.fd)

let sock_num t = get_fd_num t.fd

let print_socket buf s =
  print_socket buf s;
  Printf.bprintf buf "  rtimeout %5.0f/%5.0f read %s & %s write %s & %s (born %f)\n"
    (s.next_rtimeout -. !current_time)
  s.rtimeout
    (string_of_bool s.want_to_read)
  (string_of_bool !(s.read_allowed))
  (string_of_bool s.want_to_write)
  (string_of_bool !(s.write_allowed))
  (!current_time -. s.born)

let print_sockets buf =
  Printf.bprintf buf "PRINT SOCKETS: %d\n" (List.length !fd_tasks);
  List.iter (print_socket buf) !fd_tasks;
  ()

let info t = t.name

let set_printer s f =
  s.printer <- f

let set_dump_info s f =
  s.dump_info <- f

let prevent_close s = s.can_close <- false
let close_all () =
  List.iter (fun s ->
      if s.can_close then
        close s Closed_by_user
  ) !fd_tasks


let last_time () = !last_time
let start_time = last_time ()
let date_of_int date =
  if date >= 1000000000 then (float_of_int date) else (float_of_int date +. 1000000000.)

let string_of_date date =
  if date < 1 then "never" else
    Date.to_string (date_of_int date)
let normalize_time time =
  if time >= 1000000000 || time < 0 then time - 1000000000 else time

let get_rtimeout t = t.rtimeout, t.next_rtimeout -. !current_time

let int64_time () = Int64.of_float !current_time

let update_time () = ignore (update_time ())
let current_time () = !current_time

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

let _ =
  Heap.add_memstat "BasicSocket" (fun level buf ->
      Printf.bprintf buf "  before_select_hooks: %d\n" (List.length !before_select_hooks);
      Printf.bprintf buf "  after_select_hooks: %d\n" (List.length !after_select_hooks);
      Printf.bprintf buf "  bandwidth_second_timers: %d\n" (List.length !bandwidth_second_timers);
      Printf.bprintf buf "  %d timers\n" (List.length !timers);
      Printf.bprintf buf "  %d fd_tasks:\n" (List.length !fd_tasks);
      if level > 0 then
        List.iter (fun t -> t.dump_info buf) !fd_tasks;
      Printf.bprintf buf "  %d closed_tasks:\n" (List.length !closed_tasks);
      if level > 0 then
        List.iter (fun t -> t.dump_info buf) !closed_tasks;
  );

  add_timer 300. (fun t ->
      reactivate_timer t;
      if !debug then
        let buf = Buffer.create 100 in
        print_sockets buf;
        lprintf_nl "%s" (Buffer.contents buf);
  )
