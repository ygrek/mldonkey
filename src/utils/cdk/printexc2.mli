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

(* Module [Printexc2]: facilities for printing exceptions *)

val to_string : exn -> string
        (* [Printexc2.to_string e] returns a string representation of
           the exception [e]. *)

val printers : (exn -> string) list ref
  

val print: ('a -> 'b) -> 'a -> 'b
        (* [Printexc2.print fn x] applies [fn] to [x] and returns the result.
           If the evaluation of [fn x] raises any exception, the
           name of the exception is printed on standard error output,
           and the exception is raised again.
           The typical use is to catch and report exceptions that
           escape a function application. *)

val catch: ('a -> 'b) -> 'a -> 'b
        (* [Printexc2.catch fn x] is similar to [Printexc2.print], but
           aborts the program with exit code 2 after printing the
           uncaught exception.  This function is deprecated: the runtime
           system is now able to print uncaught exceptions as precisely
           as [Printexc2.catch] does.  Moreover, calling [Printexc2.catch]
           makes it harder to track the location of the exception
           using the debugger or the stack backtrace facility.
           So, do not use [Printexc2.catch] in new code.  *)

val register_exn : (exn -> string) -> unit
(*d [register_exn printer] registers a printer for user-defined 
exceptions. [printer] is a function which takes an exception as argument
and converts it to a string. If it cannot convert the exception, it should
raise it again. *)
  
val catchexn : string -> (unit -> unit) -> unit
  
(*d [catchexn msg f] executes the function [f] and returns unit. If an 
exception is raised during the execution of [f], the message [msg] is 
    printed on standard output with the exception using [printexn], and unit
    is returned. *)

val vcatchexn : string -> (unit -> 'a) -> 'a option
(*d [vcatchexn msg f] executes the function [f] and returns (Some [v]) if 
[v] is the value returned by [f]. If an 
exception is raised during the execution of [f], the message is printed
    on standard output with the exception using [printexn], and 
  None is returned. *)
