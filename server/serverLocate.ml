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
                
(*let get_loc liste new_source =
        match new_source.loc with 
        Firewalled_location fire -> 
                List.find (fun tmp -> match tmp.loc with
                                Firewalled_location fire2 ->
                                        fire.ip_s =
                                                fire2.ip_s 
                                        &&  fire.port_s =
                                                 fire2.port_s
                                        &&  fire.id_client = 
                                                fire2.id_client
                                | _ -> false ) liste
        | Knowed_location know ->
                List.find (fun tmp -> match tmp.loc with
                               Knowed_location know2 ->
                                       know.ip = know2.ip 
                                       && know.port = know2.port
                               |_ -> false) liste*)
        
 

                      
let add md4 new_source = 
   try
     let liste = Hashtbl.find files_by_md4 md4 in
     (*begin
        try
          let bob = get_loc liste new_source in
             if bob.expired < new_source.expired then
                     bob.expired <- new_source.expired 
        with _ -> let liste = new_source::liste in
                  Hashtbl.replace files_by_md4 md4 liste
     end
    with _-> Hashtbl.add files_by_md4 md4 [new_source] *)
         
      
      check liste liste new_source md4    
    with _ -> 
       (* Printf.printf("eXeption"); print_newline();*)
       Hashtbl.add files_by_md4 md4 [new_source]

let supp file_md4 client_loc = 
        let liste = Hashtbl.find files_by_md4 file_md4 in
          let newliste = List.filter 
                (fun loc -> client_loc.loc_ip<>loc.loc_ip &&
                client_loc.loc_port<>loc.loc_port) liste in
                Hashtbl.replace files_by_md4 file_md4 newliste

(*let supp file_md4 client_loc = 
        let liste = Hashtbl.find files_by_md4 file_md4 in
        match client_loc.loc with
        Firewalled_location fire ->
          let newliste = List.filter 
                (fun tmp -> 
                     match tmp.loc with
                     Firewalled_location tmp ->
                        fire.ip_s<>tmp.ip_s &&
                        fire.port_s<>tmp.port_s &&
                        fire.id_client<>tmp.id_client
                     | _ -> false
             ) liste in 
             Hashtbl.replace files_by_md4 file_md4 newliste
        |Knowed_location know ->
           let newliste = List.filter
                 (fun tmp -> 
                     match tmp.loc with
                     Knowed_location tmp->
                        know.ip<>tmp.ip &&
                        know.port<>tmp.port
                     | _ -> false ) 
                liste in
                Hashtbl.replace files_by_md4 file_md4 newliste *)
                
let print () = Hashtbl.iter ( fun md4 liste -> 
                        Printf.printf("Document MD4 %s") (Md4.to_string md4); 
                        print_newline();
                        List.iter (fun loc -> 
                                Printf.printf("get to %s port %d valide to %f")
                                (Ip.to_string loc.loc_ip) loc.loc_port loc.loc_expired; 
                                print_newline()) 
                                liste;
                        ) files_by_md4
                        
(*let print () = Hashtbl.iter ( fun md4 liste -> 
                        Printf.printf("Document MD4 %s ") (Md4.to_string md4); 
                        List.iter (fun tmp-> 
                                match tmp.loc with
                                Firewalled_location tmp-> 
                                        Printf.printf("Firewalled to server:%s
                                        port:%d and id:%s") (Ip.to_string
                                        tmp.ip_s) tmp.port_s
                                        (Ip.to_string tmp.id_client);
                                        print_newline()
                                | Knowed_location tmp->
                                Printf.printf("Knowed to %s port %d")
                                (Ip.to_string tmp.ip) tmp.port; 
                                print_newline()) 
                                liste;
                        ) files_by_md4*) 
module M = Mftp_server.QueryLocationReply
                        
(*let rec morph_list liste =
        match liste with
        [] -> []
        | hd::tl -> {M.ip = hd.loc_ip;M.port = hd.loc_port} :: morph_list tl
*)
                   
let get md4 = 
        try  
             let liste = Hashtbl.find files_by_md4 md4 in 

             let reply = List.map (fun loc ->
                     
                     {M.ip=loc.loc_ip;M.port=loc.loc_port})
                     liste in

            (* Printf.printf("element de la liste %s") (Ip.to_string (List.hd
            reply).M.ip); 
            print_newline();*)
            {M.md4= md4;M.locs = reply}
        with _ -> (*Printf.printf("eXeption dans find_peer");
                  print_newline();*)
                  raise Not_found
                
(*let find_peer md4 = 
        try  
             let liste = Hashtbl.find files_by_md4 md4 in 

             let reply = List.map (fun tmp ->
                     match tmp.loc with
                     Firewalled_location tmp-> 
                             {M.ip=tmp.id_client;M.port=0}
                     |Knowed_location tmp -> 
                             {M.ip=tmp.ip;M.port=tmp.port})
                     liste in

            (* Printf.printf("element de la liste %s") (Ip.to_string (List.hd
            reply).M.ip); 
            print_newline();*)
            {M.md4= md4;M.locs = reply}
        with _ -> (*Printf.printf("eXeption dans find_peer");
                  print_newline();*)
                  raise Not_found*)
