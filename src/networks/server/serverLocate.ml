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

open Md4

open ServerMessages
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open DonkeyProtoCom
open DonkeyProtoServer
open ServerTypes
open ServerOptions
open ServerGlobals
open Hashtbl




let rec add_source_list liste loc= 
  match liste with
      [] -> [loc]
    | hd::tl ->  
        (*lprintf"parcourage for %s\n" (Ip.to_string loc.loc_ip);*)
        if (Ip.equal hd.loc_ip loc.loc_ip) then
          begin
            if loc.loc_local then
              lprintf "WARNING: local equality  %s %b\n" (Ip.to_string loc.loc_ip)
                hd.loc_local
            else
              lprintf "WARNING: remote equality %s %b\n" 
                (Ip.to_string (
                   try
                     (match (Hashtbl.find clients_by_id loc.loc_ip) with
                          RemoteClient c -> c.remote_client_remote_id
                        | _ -> Ip.null)
                   with _-> (loc.loc_ip)
                 )) 
                hd.loc_local;
            (hd :: tl)
          end
        else 
          (hd :: add_source_list tl loc)
        
(*
let supp_source_list liste source =
  let newliste = List.filter 
                   (fun loc -> 
                      not (Ip.equal source.loc_ip loc.loc_ip)
                   ) liste in
    newliste
*)


let rec supp_source_list liste source =
  match liste with 
      [] -> lprintf "WARNING: FAIL TO REMOVE LOCATION TABLE %s \n" (Ip.to_string source.loc_ip);
        []
    | hd :: tl -> 
        if  (Ip.equal source.loc_ip hd.loc_ip) then
          tl
        else
          hd :: supp_source_list tl source
            

 
exception TooManyFiles
                      
let add md4 new_source = 
   try
     let liste = Hashtbl.find files_by_md4 md4 in
       if not (List.exists (fun l -> l.loc_local) liste) then
         incr nshared_md4;
       let newliste = add_source_list liste new_source in
         Hashtbl.replace files_by_md4 md4 newliste
    with _ -> 
       (* lprintf("eXeption"); lprint_newline();*)
       if (!!max_files <= !nshared_md4) then
           raise TooManyFiles;
       Hashtbl.add files_by_md4 md4 [new_source];
       incr nshared_md4


  

let notifications md4 sources = 
  try
    let liste = ref (Hashtbl.find files_by_md4 md4) in
    let adds,supps = List.partition (fun (add,notif) ->
                                       add) sources in
      List.iter (fun (add,source) -> 
                   if add then
                     begin
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         incr nshared_remote_md4;
                       liste :=  add_source_list !liste source;
                         end
                   else
                     begin
                       liste := supp_source_list !liste source;
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         decr nshared_remote_md4;
                     end
                ) adds;
      List.iter (fun (add,source) -> 
                   if add then
                     begin
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         incr nshared_remote_md4;
                       liste :=  add_source_list !liste source;
                     end
                   else
                     begin
                       liste := supp_source_list !liste source;
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         decr nshared_remote_md4;
                     end
                ) supps;

      if (!liste=[]) then
        begin 
          Hashtbl.remove files_by_md4 md4;
          (*decr nshared_md4*)
        end
      else
        Hashtbl.replace files_by_md4 md4 !liste;
  with _ -> 
    let liste = ref [] in
    let adds,supps = List.partition (fun (add,notif) ->
                              add) sources in
      List.iter (fun (add,source) -> 
                   if add then
                     begin
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         incr nshared_remote_md4;
                       liste :=  add_source_list !liste source;
                     end
                   else
                     begin
                       liste := supp_source_list !liste source;
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         decr nshared_remote_md4;
                     end
                ) adds;
      List.iter (fun (add,source) -> 
                   if add then
                     begin
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         incr nshared_remote_md4;
                       liste :=  add_source_list !liste source;
                     end
                   else
                     begin
                       liste := supp_source_list !liste source;
                       if not (List.exists (fun l -> not l.loc_local) !liste) then
                         decr nshared_remote_md4;
                     end
                ) supps;
      
      

      if (!liste<>[]) then
        begin
          Hashtbl.add files_by_md4 md4 !liste;
          incr nshared_remote_md4;
        end
                     

let supp md4 source_loc = 
  try
    (*lprintf "Supp For md4 : %s \n " (Md4.to_string md4);*)
    let liste = ref (Hashtbl.find files_by_md4 md4) in
      
       (*List.iter (fun s -> 
         lprintf "%s:%d\n" (Ip.to_string s.loc_ip) s.loc_port
         ) !liste;*)
      liste := supp_source_list !liste source_loc;
      
      (*lprintf "NEW LIST For md4 : %s \n " (Md4.to_string md4);
      List.iter (fun s -> 
                   lprintf "%s:%d\n" (Ip.to_string s.loc_ip) s.loc_port
                ) !liste;*)

      if source_loc.loc_local && not (List.exists (fun loc -> loc.loc_local) !liste) then
        begin
          (*lprintf "No more local source\n";*)
          decr nshared_md4
        end;
      
      if !liste=[] then
        begin
          Hashtbl.remove files_by_md4 md4;
          (*lprintf "decr the count\n";*)
        end
      else
        begin
          Hashtbl.replace files_by_md4 md4 !liste;
        end
  with _ -> lprintf "You want to remove a file that doesn't existe in location table\n"; 
    ()

let remote_supp md4 source_loc = 
  try
    (*lprintf "For md4 : %s \n " (Md4.to_string md4);*)
    let liste = ref (Hashtbl.find files_by_md4 md4) in
      
      (* List.iter (fun s -> 
         lprintf "%s:%d\n" (Ip.to_string s.loc_ip) s.loc_port
         ) !liste;*)
      liste := supp_source_list !liste source_loc;
      
      (*lprintf "NEW LIST For md4 : %s \n " (Md4.to_string md4);
        List.iter (fun s -> 
        lprintf "%s:%d\n" (Ip.to_string s.loc_ip) s.loc_port
        ) !liste;*)

      if source_loc.loc_local && not (List.exists (fun loc -> (not loc.loc_local)) !liste) then
        begin
          (*lprintf "No more local source\n";*)
          decr nshared_remote_md4
        end;
      
      if !liste=[] then
        begin
          Hashtbl.remove files_by_md4 md4;
          (*decr nshared_md4*)
        end
      else
        begin
          Hashtbl.replace files_by_md4 md4 !liste;
        end
  with _ -> lprintf "You want to remove a remote file that doesn't existe in location table\n"; 
    () 
    

let print () = 
         lprint_newline();
         lprintf("FILES BY MD4:");lprint_newline();
         Hashtbl.iter ( fun md4 liste -> 
                        lprintf("Document MD4 %s") (Md4.to_string md4); 
                        lprint_newline();
                        List.iter (fun loc -> 
                                lprintf("     -> to %s port %d valide to %f")
                                (Ip.to_string loc.loc_ip) loc.loc_port loc.loc_expired; 
                                lprint_newline()) 
                                liste;
                        ) files_by_md4;
         lprintf("FIN");lprint_newline();
         lprint_newline()

let get_list_of_md4 () =
  let l = ref [] in
    Hashtbl.iter( fun md4 sources ->
                    l := (md4,(List.length sources)) :: !l 
                ) files_by_md4;
    !l

let get_locate_table () =
  let l = ref [] in
    Hashtbl.iter( fun md4 sources ->
                    l := (md4,sources) :: !l 
                ) files_by_md4;
    !l                

module M = DonkeyProtoServer.QueryLocationReply
 
let rec map_sized f size liste= 
  match liste with
    [] -> []
  | a::l -> if size = 0 then []
              else 
            let r = f a in r :: map_sized f (size-1) l
                     
                   
let get md4 = 
        try  
             let liste = Hashtbl.find files_by_md4 md4 in 

             let reply = map_sized (fun loc ->
                     
                     {M.ip=loc.loc_ip;M.port=loc.loc_port}) 200
                     liste in

            (* lprintf("element de la liste %s") (Ip.to_string (List.hd
            reply).M.ip); 
            lprint_newline();*)
           
            {M.md4= md4;M.locs = reply}
        with _ -> (*lprintf("eXeption dans find_peer");
                  lprint_newline();*)
                  raise Not_found
                

let exist md4 id =
  try
    let sources = Hashtbl.find files_by_md4 md4 in
      (List.exists (fun loc -> loc.loc_ip = id ) sources)
  with _ -> raise Not_found

