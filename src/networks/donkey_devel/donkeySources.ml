(* Copyright 2001, 2002 Simon, INRIA *)
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

(*

New idea: what about trying to connect anyway if not all the slots
where tried ? We could reconnect more frequently to bad sources if we
have time to do it.

*)

open Options
open CommonOptions
open DonkeyOptions
open CommonTypes
open BasicSocket
open DonkeyTypes
open DonkeyGlobals
      
          
let client_connected x = 
  match !!source_management with
  | 1 -> DonkeySources1.client_connected x  
  | 2 -> DonkeySources2.client_connected x
  | _ -> DonkeySources3.client_connected x

  
let recompute_ready_sources x = 
  match !!source_management with
  | 1 -> DonkeySources1.recompute_ready_sources x  
  | 2 -> DonkeySources2.recompute_ready_sources x
  | _ -> DonkeySources3.recompute_ready_sources x

  
let print_sources x = 
  match !!source_management with
  | 1 -> DonkeySources1.print_sources x  
  | 2 -> DonkeySources2.print_sources x
  | _ -> DonkeySources3.print_sources x

  
let check_sources x = 
  match !!source_management with
  | 1 -> DonkeySources1.check_sources x  
  | 2 -> DonkeySources2.check_sources x
  | _ -> DonkeySources3.check_sources x

  
let need_new_sources x = 
  match !!source_management with
  | 1 -> DonkeySources1.need_new_sources x  
  | 2 -> DonkeySources2.need_new_sources x
  | _ -> DonkeySources3.need_new_sources x

  
let new_source x = 
  match !!source_management with
  | 1 -> DonkeySources1.new_source x  
  | 2 -> DonkeySources2.new_source x
  | _ -> DonkeySources3.new_source x

  
let reschedule_sources x = 
  match !!source_management with
  | 1 -> DonkeySources1.reschedule_sources x  
  | 2 -> DonkeySources2.reschedule_sources x
  | _ -> DonkeySources3.reschedule_sources x

  
let source_of_client x = 
  match !!source_management with
  | 1 -> DonkeySources1.source_of_client x  
  | 2 -> DonkeySources2.source_of_client x
  | _ -> DonkeySources3.source_of_client x

  
let iter x = 
  match !!source_management with
  | 1 -> DonkeySources1.iter x  
  | 2 -> DonkeySources2.iter x
  | _ -> DonkeySources3.iter x

  
let old_source x = 
  match !!source_management with
  | 1 -> DonkeySources1.old_source x  
  | 2 -> DonkeySources2.old_source x
  | _ -> DonkeySources3.old_source x
  
let add_source_request x = 
  match !!source_management with
  | 1 -> DonkeySources1.add_source_request x  
  | 2 -> DonkeySources2.add_source_request x
  | _ -> DonkeySources3.add_source_request x
  
let init (x: unit) = 
 DonkeySources1.init x;
 DonkeySources2.init x;
 DonkeySources3.init x

