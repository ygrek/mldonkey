(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Int64 of (int64 -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Rest of (string -> unit)   (* Stop interpreting keywords and call the
                                  function with each remaining argument *)
  | Array of int * (string array -> unit)
                           (* Call the function with an array of arguments *)
  
exception Bad of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of int * string
  | Message of string

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3)::t when y1 = x -> y2
  | _::t -> assoc3 x t
;;

let usage speclist errmsg =
  eprintf "%s\n" errmsg;
  List.iter (function (key, _, doc) -> 
        eprintf "  %s %s\n" 
          (if String.length key = 2 then key else "-"^key) doc) speclist;
  try ignore (assoc3 "-help" speclist)
  with Not_found -> eprintf "  -help  display this list of options\n";
  try ignore (assoc3 "--help" speclist)
  with Not_found -> eprintf "  --help display this list of options\n";
;;

let current = ref 0;;

let parse_argv argv speclist anonfun errmsg =
  let initpos = !current in
  let stop error =
    let progname =
      if initpos < Array.length argv then argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          eprintf "%s: unknown option `%s'.\n" progname s
      | Missing (n, s) ->
          eprintf "%s: option `%s' needs %d arguments.\n" progname s n
      | Wrong (opt, arg, expected) ->
          eprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
            progname arg opt expected
      | Message s ->
          eprintf "%s: %s.\n" progname s
    end;
    usage speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then exit 0
    else exit 2
  in
  let l = Array.length argv in
  incr current;
  while !current < l do
    let s = argv.(!current) in
    let sl = String.length s in
    if sl >= 2 && 
      String.get s 0 = '-' &&
      String.get s 1 = '-' 
    then begin
        let action =
          try assoc3 (String.sub s 1 (sl-1)) speclist
          with Not_found -> stop (Unknown s)
        in
        begin try
            match action with
            | Unit f -> f ();
            | Set r -> r := true;
            | Clear r -> r := false;
            | String f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                f arg;
                incr current;
            | Int f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                begin try f (int_of_string arg)
                  with Failure _ -> stop (Wrong (s, arg, "an integer"))
                end;
                incr current;
            | Int64 f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                begin try f (Int64.of_string arg)
                  with Failure _ -> stop (Wrong (s, arg, "an integer64"))
                end;
                incr current;
            | Float f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                begin try f (float_of_string arg);
                  with Failure _ -> stop (Wrong (s, arg, "a float"))
                end;
                incr current;
            | Array (n, f) when n>0 && !current + n < l ->
                let args = Array.sub argv (!current+1) n in
                f args;
                current := !current + n
            | Array (n, f)  ->
                stop (Missing (l - !current - n, s))
            | Rest f ->
                while !current < l-1 do
                  f argv.(!current+1);
                  incr current;
                done;
            | _ -> stop (Missing (1, s))
          with Bad m -> stop (Message m);
        end;
        incr current;
      end else
    if sl>=1 && s.[0] = '-' then begin
        for i = 1 to sl-1 do
          let s = Printf.sprintf "-%c" s.[i] in
          let action =
            try assoc3 s speclist
            with Not_found -> stop (Unknown s)
          in
          begin try
              match action with
              | Unit f -> f ();
              | Set r -> r := true;
              | Clear r -> r := false;
              | String f when !current + 1 < l ->
                  let arg = argv.(!current+1) in
                  f arg;
                  incr current;
              | Int f when !current + 1 < l ->
                  let arg = argv.(!current+1) in
                  begin try f (int_of_string arg)
                    with Failure _ -> stop (Wrong (s, arg, "an integer"))
                  end;
                  incr current;
              | Int64 f when !current + 1 < l ->
                  let arg = argv.(!current+1) in
                  begin try f (Int64.of_string arg)
                    with Failure _ -> stop (Wrong (s, arg, "an integer64"))
                  end;
                  incr current;
              | Float f when !current + 1 < l ->
                  let arg = argv.(!current+1) in
                  begin try f (float_of_string arg);
                    with Failure _ -> stop (Wrong (s, arg, "a float"))
                  end;
                  incr current;
              | Array (n, f) when n>0 && !current + n < l ->
                  let args = Array.sub argv (!current+1) n in
                  f args;
                  current := !current + n
              | Rest f ->
                  while !current < l-1 do
                    f argv.(!current+1);
                    incr current;
                  done;
              | _ -> stop (Missing (1,s))
            with Bad m -> stop (Message m);
          end;
        
        done;
        incr current        
      end else begin
    (try anonfun s with Bad m -> stop (Message m));
    incr current;
  end;
done;
;;

let parse = parse_argv Sys.argv;;


let parse2_argv argv speclist anonfun errmsg =
  let initpos = !current in
  let stop error =
    let progname =
      if initpos < Array.length argv then argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          eprintf "%s: unknown option `%s'.\n" progname s
      | Missing (n,s) ->
          eprintf "%s: option `%s' needs %d more argument(s).\n" progname s n
      | Wrong (opt, arg, expected) ->
          eprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
            progname arg opt expected
      | Message s ->
          eprintf "%s: %s.\n" progname s
    end;
    usage speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then exit 0
    else exit 2
  in
  let l = Array.length argv in
  incr current;
  while !current < l do
    let s = argv.(!current) in
    let sl = String.length s in
    if sl >= 1 && String.get s 0 = '-' 
    then begin
        let action =
          try assoc3 s speclist
          with Not_found -> stop (Unknown s)
        in
        begin try
            match action with
            | Unit f -> f ();
            | Set r -> r := true;
            | Clear r -> r := false;
            | String f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                f arg;
                incr current;
            | Int f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                begin try f (int_of_string arg)
                  with Failure _ -> stop (Wrong (s, arg, "an integer"))
                end;
                incr current;
            | Int64 f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                begin try f (Int64.of_string arg)
                  with Failure _ -> stop (Wrong (s, arg, "an integer64"))
                end;
                incr current;
            | Float f when !current + 1 < l ->
                let arg = argv.(!current+1) in
                begin try f (float_of_string arg);
                  with Failure _ -> stop (Wrong (s, arg, "a float"))
                end;
                incr current;
            | Array (n, f) when n>0 && !current + n < l ->
                let args = Array.sub argv (!current+1) n in
                f args;
                current := !current + n
            | Array (n, f)  ->
                stop (Missing (n - (l - !current) + 1, s))                
            | Rest f ->
                while !current < l-1 do
                  f argv.(!current+1);
                  incr current;
                done;
            | _ -> stop (Missing (1,s))
          with Bad m -> stop (Message m);
        end;
        incr current;
      end else begin
    (try anonfun s with Bad m -> stop (Message m));
    incr current;
  end;
done;
;;

let parse2 = parse2_argv Sys.argv;;
