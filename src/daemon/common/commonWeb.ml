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

open Printf2
open CommonOptions
open BasicSocket  
open TcpBufferedSocket
open CommonGlobals
open Options
open CommonTypes
  
let days = ref 0      
let hours = ref 0    
  
let cut_messages f sock nread =
  let b = buf sock in
  try
    while b.len >= 4 do
      let msg_len = LittleEndian.get_int b.buf b.pos in
      if b.len >= 4 + msg_len then
        begin
          let s = String.sub b.buf (b.pos+4) msg_len in
          buf_used b (msg_len + 4);
          let opcode = LittleEndian.get_int16 s 0 in
          (f opcode s : unit)
        end
      else raise Not_found
    done
  with Not_found -> ()
  
let load_web_infos () =
(* Try to connect the redirector to get interesting information, since we
are not allowed to use savannah anymore. The redirector should be able to
support the charge, at least, currently. *)
  let (name, port) = !!mlnet_redirector in
  Ip.async_ip name (fun ip ->
      try
        lprintf "connecting to redirector\n";
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.connect token "connect redirector"
            (Ip.to_inet_addr ip) port            
            (fun sock event ->
              match event with
              | BASIC_EVENT (LTIMEOUT | RTIMEOUT) -> 
                  TcpBufferedSocket.close sock Closed_for_timeout
              | _ -> ())
        in
        TcpBufferedSocket.set_rtimeout sock 30.;
        let to_read = ref [] in
        set_reader sock (cut_messages (fun opcode s ->
              lprintf "redirector info received\n";
              let module L = LittleEndian in
              
              let motd_html_s, pos = L.get_string16 s 2 in
              let servers_met_s, pos = L.get_string16 s pos in
              let peers_ocl_s, pos = L.get_string16 s pos in
              let motd_conf_s, pos = L.get_string16 s pos in
              
              motd_html =:= motd_html_s;
              
              let servers_met_file = Filename.temp_file "servers" ".met" in
              File.from_string servers_met_file servers_met_s;
              load_file "servers.met" servers_met_file;

              let peers_ocl_file = Filename.temp_file "peers" ".ocl" in
              File.from_string peers_ocl_file peers_ocl_s;
              load_file "ocl" peers_ocl_file;

              let motd_conf_file = Filename.temp_file "motd" ".conf" in
              File.from_string motd_conf_file motd_conf_s;
              load_file "motd.conf" motd_conf_file;              

              let ip = L.get_ip s pos in
              last_high_id := ip;
              
              lprintf "Redirector info loaded (IP set to %s)\n"
                (Ip.to_string ip);
              TcpBufferedSocket.close sock Closed_by_user;
              
              
          ))
      with e -> 
          lprintf "Exception %s while connecting redirector\n"
            (Printexc2.to_string e)
  );
  
  if !!network_update_url <> "" then begin
    load_url "motd.html" (Filename.concat !!network_update_url "motd.html");
    load_url "motd.conf" (Filename.concat !!network_update_url "motd.conf");
  end;
  List.iter (fun (kind, period, url) ->
      if !days mod period = 0 then load_url kind url
  ) !!CommonOptions.web_infos
  