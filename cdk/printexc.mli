
(* Module [Printexc]: facilities for printing exceptions *)

val to_string : exn -> string
        (* [Printexc.to_string e] returns a string representation of
           the exception [e]. *)

val printers : (exn -> string) list ref
  

val print: ('a -> 'b) -> 'a -> 'b
        (* [Printexc.print fn x] applies [fn] to [x] and returns the result.
           If the evaluation of [fn x] raises any exception, the
           name of the exception is printed on standard error output,
           and the exception is raised again.
           The typical use is to catch and report exceptions that
           escape a function application. *)

val catch: ('a -> 'b) -> 'a -> 'b
        (* [Printexc.catch fn x] is similar to [Printexc.print], but
           aborts the program with exit code 2 after printing the
           uncaught exception.  This function is deprecated: the runtime
           system is now able to print uncaught exceptions as precisely
           as [Printexc.catch] does.  Moreover, calling [Printexc.catch]
           makes it harder to track the location of the exception
           using the debugger or the stack backtrace facility.
           So, do not use [Printexc.catch] in new code.  *)

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
