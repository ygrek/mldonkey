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

open ServerMessages
open Unix
open TcpBufferedSocket
open DonkeyMftp
open Options
open Mftp_comm
open Mftp_server
open ServerTypes
open ServerOptions
open ServerGlobals
open Hashtbl




let rec add_source_list liste loc= 
         match liste with
          [] ->  (*Printf.printf("add liste"); print_newline();*) 
                 [loc]
         | hd::tl -> (* Printf.printf("parcourage"); print_newline();*)
                     if hd.loc_ip = loc.loc_ip && hd.loc_port = loc.loc_port then
                       tl
                      else add_source_list tl loc
        
let supp_source_list liste source =
  let newliste = List.filter 
		   (fun loc -> 
		      source.loc_ip<>loc.loc_ip ||
		      source.loc_port<>loc.loc_port) liste in
    newliste

 
exception ToMuchFiles
                      
let add md4 new_source = 
   try
     let liste = Hashtbl.find files_by_md4 md4 in
     let newliste = add_source_list liste new_source in
       Hashtbl.replace files_by_md4 md4 newliste 
    with _ -> 
       (* Printf.printf("eXeption"); print_newline();*)
       if (!!max_files <= !nshared_md4) then
           raise ToMuchFiles;
       Hashtbl.add files_by_md4 md4 [new_source];
       incr nshared_md4

let adds md4 new_sources =
  try  
    let liste = ref (Hashtbl.find files_by_md4 md4) in
    let module LI = ServerMessages.LocalisationInit in
      List.iter (fun new_loc -> 
		   liste := add_source_list !liste 
		   { 
		     loc_ip = new_loc.LI.source_ip;
		     loc_port = new_loc.LI.source_port;
		     loc_expired = Unix.time();
		   }
		) new_sources;
      Hashtbl.replace files_by_md4 md4 !liste 
  with _ -> 
    let module LI = ServerMessages.LocalisationInit in
      Hashtbl.add files_by_md4 md4 
      (List2.tail_map (fun x -> 
					      { 
						loc_ip = x.LI.source_ip;
						loc_port = x.LI.source_port;
						loc_expired = Unix.time();
					      }
					   )new_sources);
    incr nshared_remote_md4
  
  

let notifications md4 sources = 
  let module LN = ServerMessages.LocateNotif in
    try
      let liste = ref (Hashtbl.find files_by_md4 md4) in
	List.iter (fun source -> 
		     let tmp =  { 
		       loc_ip = source.LN.source_ip;
		       loc_port = source.LN.source_port;
		       loc_expired = 0.;
		     } in
		       if source.LN.add then
			 liste :=  add_source_list !liste tmp
		       else
			 liste := supp_source_list !liste tmp
		) sources;
      if (!liste=[]) then
	begin 
	  Hashtbl.remove files_by_md4 md4;
	  decr nshared_remote_md4
	end
      else
	Hashtbl.replace files_by_md4 md4 !liste;
	
  with _ -> ()
		     

let supp md4 source_loc = 
        let liste = ref (Hashtbl.find files_by_md4 md4) in
          liste := supp_source_list !liste source_loc;
          if !liste=[] then
	    begin
              Hashtbl.remove files_by_md4 md4;
              decr nshared_md4
            end
          else
            Hashtbl.replace files_by_md4 md4 !liste
               
 
let print () = 
         print_newline();
         Printf.printf("FILES BY MD4:");print_newline();
         Hashtbl.iter ( fun md4 liste -> 
                        Printf.printf("Document MD4 %s") (Md4.to_string md4); 
                        print_newline();
                        List.iter (fun loc -> 
                                Printf.printf("     -> to %s port %d valide to %f")
                                (Ip.to_string loc.loc_ip) loc.loc_port loc.loc_expired; 
                                print_newline()) 
                                liste;
                        ) files_by_md4;
         Printf.printf("FIN");print_newline();
         print_newline()
     
let get_local_sources c = ()                   

module M = Mftp_server.QueryLocationReply
 
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

            (* Printf.printf("element de la liste %s") (Ip.to_string (List.hd
            reply).M.ip); 
            print_newline();*)
           
            {M.md4= md4;M.locs = reply}
        with _ -> (*Printf.printf("eXeption dans find_peer");
                  print_newline();*)
                  raise Not_found
                

