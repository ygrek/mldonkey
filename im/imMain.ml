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

open Options
open BasicSocket
open ImTypes
open ImOptions
open ImAccount
open ImOptions
  
(*********************************************************************

                    FOR TESTING PURPOSE

*********************************************************************)

  
let _ =
  (*
  
  Msn.msn_login ();
  
  add_infinite_timer 10. (fun _ -> 
      Printf.printf "******** SENDING **********"; print_newline ();
      Msn.msn_send "b8_cro@hotmail.com" "Hello");
  
  BasicSocket.loop ()

  *)

  (try Options.load accounts_ini with e -> 
        Printf.printf "Exception during options load accounts"; 
        print_newline ());
  Options.save_with_help accounts_ini;
  add_infinite_timer 60. (fun _ ->
      List2.safe_iter account_keepalive !!accounts
  )

(********************************************************************

                         THE INTERFACE 

******************************************************************)
  
  
