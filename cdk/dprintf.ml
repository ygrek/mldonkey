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

(* This module implements Printf like functions that take care of the
level of verbosity to choose when something should be printed. *)

open Printf

let debug_level = ref 0
let debug_oc = ref stderr
let line_mode = ref true
  
  
let printf_string level s =
  if !debug_level >= level then begin
      if !line_mode then line_mode := false;
      fprintf !debug_oc "%s" s;
      flush !debug_oc
    end
    
let printf0 level fmt =
    if !debug_level >= level then begin
      if not !line_mode then begin
          fprintf !debug_oc "\n";
          line_mode := true;
        end;
      fprintf !debug_oc fmt;
      fprintf !debug_oc "\n";
      flush !debug_oc
    end
    
let printf1 level fmt arg1 =
    if !debug_level >= level then begin
      if not !line_mode then begin
          fprintf !debug_oc "\n";
          line_mode := true;
        end;
      fprintf !debug_oc fmt arg1;
      fprintf !debug_oc "\n";
      flush !debug_oc
    end
    
let printf2 level fmt arg1 arg2 =
    if !debug_level >= level then begin
      if not !line_mode then begin
          fprintf !debug_oc "\n";
          line_mode := true;
        end;
      fprintf !debug_oc fmt arg1 arg2;
      fprintf !debug_oc "\n";
      flush !debug_oc
    end
    
let printf3 level fmt arg1 arg2 arg3 =
    if !debug_level >= level then begin
      if not !line_mode then begin
          fprintf !debug_oc "\n";
          line_mode := true;
        end;
      fprintf !debug_oc fmt arg1 arg2 arg3;
      fprintf !debug_oc "\n";
      flush !debug_oc
    end
    
let printf4 level fmt arg1 arg2 arg3 arg4 =
    if !debug_level >= level then begin
      if not !line_mode then begin
          fprintf !debug_oc "\n";
          line_mode := true;
        end;
      fprintf !debug_oc fmt arg1 arg2 arg3 arg4;
      fprintf !debug_oc "\n";
      flush !debug_oc
    end
    
let printf5 level fmt arg1 arg2 arg3 arg4 arg5 =
    if !debug_level >= level then begin
      if not !line_mode then begin
          fprintf !debug_oc "\n";
          line_mode := true;
        end;
      fprintf !debug_oc fmt arg1 arg2 arg3 arg4 arg5;
      fprintf !debug_oc "\n";
      flush !debug_oc
    end
