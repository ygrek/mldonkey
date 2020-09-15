(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

open DonkeyProtoServer
open Ip
open Md4
open ServerGlobals
open BasicSocket
open Options
open ServerOptions


module ServerLog = struct 

type stand_by_log = {
        mutable ip : Ip.t;
        mutable md4 : Md4.t;
        mutable time : float;
        mutable req : DonkeyProtoServer.t list;
        mutable results : CommonTypes.tagged_file list;
        mutable note : string;
}



type t = {
       mutable oc : out_channel;
       mutable liste : stand_by_log list;
}       



let nlog = ref 1
          
let log = ref {
              oc = open_out "log.0";
              liste = [];
      } 



let tmp = ref { ip = Ip.null;
                md4 = Md4.null;
                time = 0.;
                req = [];
                results = [];
                note = "";
              }     



              
let put_rep msg =
        !tmp.req <- !tmp.req @ [msg] 

let put_results l =
        !tmp.results <- l
      
let print t  =
        (*lprintf "*********ADD to log at %f\n" t.time;*)
        Printf.fprintf !log.oc "<REQ>\n";
        Printf.fprintf !log.oc "%f\n" t.time;
        Printf.fprintf !log.oc "%s\n" (Ip.to_string t.ip);
        Printf.fprintf !log.oc "%s\n" (Md4.to_string t.md4);
        if (List.length t.req) >0 then
          DonkeyProtoServer.fprint !log.oc (List.hd t.req);
       (* with _ -> lprintf "vraiment pas cool";*)
        if (List.length t.req) >1 then 
                begin
                 Printf.fprintf !log.oc "<REP>\n";
                 DonkeyProtoServer.fprint !log.oc (List.nth t.req 2)
                end;
        if (List.length t.results) <> 0 then
                DonkeyProtoServer.QueryReply.fprint !log.oc t.results;
        if t.note <> "" then Printf.fprintf !log.oc "%s\n" t.note;
        ()
        
let rec save liste =
        match liste with 
        [] -> ()
        | hd::tl ->   print hd; 
                      save tl

                    
let add_to_liste () =
          (*lprintf "//////// Add to list \n";*)
          (*lprintf " Already Cool new t %s\n" (Ip.to_string !tmp.ip);*) 
          !log.liste <- !log.liste @ [!tmp]
       



let add_note s = 
        !tmp.note <- s
        
let new_log_req ip md4 msg = 
        tmp := {
                ip = ip;
                md4 = md4;
                time = Unix.time();
                req = [msg];
                results = [];
                note = "";
        }
        (*lprintf "Cool new t %s at time %f\n" (Ip.to_string !tmp.ip) (!tmp.time)*) 
   
let something_append ip md4 what =
  tmp := {
                ip = ip;
                md4 = md4;
                time = Unix.time();
                req = [];
                results = [];
                note = what;
        }


let initialized () =
      lprintf "INITIALISATION DU LOG "; lprint_newline();
      Printf.fprintf !log.oc "<LOGNUM>\n%d\n<SERVER STAT>\n%d\n%d\n%f\n" (!nlog) !nconnected_clients !nshared_md4 (Unix.time());
      add_infinite_option_timer log_time_out (fun timer ->
              save !log.liste;
              !log.liste <- [];
              flush !log.oc;
              lprintf "LOGS SAVED ON DISQUE\n"
      );
      add_infinite_option_timer change_log_file (fun timer ->
              save !log.liste;
              Printf.fprintf !log.oc "<SERVER STAT>\n%d\n%d\n%f\n <ENDOFFILE>" !nconnected_clients !nshared_md4 (Unix.time());
              close_out !log.oc;
              !log.liste <- [];
              !log.oc <- open_out ("log."^(string_of_int !nlog));
              Printf.fprintf !log.oc "<LOGNUM>\n%d\n<SERVER STAT>\n%d\n%d\n%f\n" !nlog !nconnected_clients !nshared_md4 (Unix.time());
              incr nlog
                                  )



end
