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




let rec check liste lst loc md4= 
         match liste with
          [] ->  (*Printf.printf("add liste"); print_newline();*) 
                 let lst = loc::lst in Hashtbl.replace files_by_md4 md4 lst
         | hd::tl -> (* Printf.printf("parcourage"); print_newline();*)
                     if hd.loc_ip = loc.loc_ip && hd.loc_port = loc.loc_port
                     then begin
                          if hd.loc_expired<loc.loc_expired then
                             hd.loc_expired <- loc.loc_expired
                          end
                      else check tl lst loc md4
        
 
exception ToMuchFiles
                      
let add md4 new_source = 
   try
     let liste = Hashtbl.find files_by_md4 md4 in
      check liste liste new_source md4    
    with _ -> 
       (* Printf.printf("eXeption"); print_newline();*)
       if (!!max_files <= !nshared_files) then
           raise ToMuchFiles;
       Hashtbl.add files_by_md4 md4 [new_source];
       incr nshared_files


let supp file_md4 client_loc = 
        let liste = Hashtbl.find files_by_md4 file_md4 in
          let newliste = List.filter 
                (fun loc -> 
                (*Printf.printf(" test : %s et %s avec %d et %d") 
                 (Ip.to_string client_loc.loc_ip)
                 (Ip.to_string loc.loc_ip)
                 client_loc.loc_port
                 loc.loc_port;
                 print_newline();*)
                client_loc.loc_ip<>loc.loc_ip ||
                client_loc.loc_port<>loc.loc_port) liste in
                if newliste=[] then
                   begin 
                    Hashtbl.remove files_by_md4 file_md4;
                    decr nshared_files
                   end
                else
                   Hashtbl.replace files_by_md4 file_md4 newliste
               
 
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
                

