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
open Printf2
  
open AnyEndian
open LittleEndian

open BasicSocket  
open TcpBufferedSocket
  
open CommonGlobals
open CommonOptions
open CommonTypes
  
let days = ref 0      
let hours = ref 0    

(*************************************************************************)
(*                                                                       *)
(*                         load_url                                      *)
(*                                                                       *)
(*************************************************************************) 

let file_kinds = ref []

let add_web_kind kind f =
  file_kinds := (kind,f) :: !file_kinds

let mldonkey_wget url f = 
  let module H = Http_client in
  let r = {
      H.basic_request with
      H.req_url = Url.of_string url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_user_agent = 
      Printf.sprintf "MLdonkey/%s" Autoconf.current_version;
    } in
  
  H.wget r f  
  
  
let load_url kind url =
  lprintf "QUERY URL %s\n" url; 
  let f = 
    try 
      (List.assoc kind !file_kinds) url
    with e -> failwith (Printf.sprintf "Unknown kind [%s]" kind)
  in 
  try
    mldonkey_wget url f
  with e -> failwith (Printf.sprintf "Exception %s while loading %s"
          (Printexc2.to_string e) url)
  
let load_file kind file =
  try 
    (List.assoc kind !file_kinds) file file
  with e -> 
      lprintf "Exception %s while loading kind %s\n" 
        (Printexc2.to_string e)
      kind

(*************************************************************************)
(*                                                                       *)
(*                         cut_messages (internal)                       *)
(*                                                                       *)
(*************************************************************************) 
  
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
  

(*************************************************************************)
(*                                                                       *)
(*                         add_redirector_info                           *)
(*                                                                       *)
(*************************************************************************) 
    
(* Learn how many people are using mldonkey at a current time, and which 
servers they are connected to --> build a database of servers

Now, get some more information:
- Which version do they use ?
- How much data is shared ?

Note that the exact content/type/name of the files is not sent, nor
any private information. Just for statistics. Can be disabled in the 
  downloads.ini config file anyway.
*)
let buf = TcpBufferedSocket.internal_buf

let buf_addr buf (ip,port) =
  buf_ip buf ip;
  buf_int16 buf port

let buf_string buf s =
  buf_int16 buf (String.length s);
  Buffer.add_string buf s
  
let redirector_infos = ref []
let add_redirector_info (n : string) (f : Buffer.t -> unit) =
  redirector_infos := (n,f) :: !redirector_infos

let gen_redirector_packet () = 
  
  let infos =
    List.map (fun (n,f) ->
        n, 
        ( Buffer.clear buf;
          f buf;
          Buffer.contents buf)
    ) !redirector_infos in
  
  Buffer.clear buf;
  buf_int8 buf 212; (* udp_magic *)
  buf_int8 buf 2;   (* type of data sent *)
  let ip = client_ip None in
  buf_ip buf ip; (* The client IP *)

(* Some statistics on the network *)
  buf_string buf Autoconf.current_version;
  buf_int buf (last_time () - start_time); (* uptime in sec *)

(* Statistics for Supernode creation *)
  buf_int16 buf !!max_hard_upload_rate;
  buf_int16 buf !!max_hard_download_rate;
  buf_int buf (compute_lost_byte upload_control);
  buf_int buf (compute_lost_byte download_control);
  
  buf_list (fun buf (n,s) ->
      buf_string buf n;
      buf_string buf s
  ) buf infos;
  
  let s = Buffer.contents buf in    
  s

(*************************************************************************)
(*                                                                       *)
(*                         connect_redirector                            *)
(*                                                                       *)
(*************************************************************************) 
      
let propagation_socket = UdpSocket.create_sendonly ()  
let counter = ref 1
  
let connect_redirector () =
  if !!propagate_servers then begin
      decr counter;
      if !counter = 0 then begin
          counter := 6;
          let s = gen_redirector_packet () in
          try            
            let name, port = !!mlnet_redirector in
            UdpSocket.write propagation_socket false s (Ip.from_name name) port;
            
          with e ->
              lprintf "Exception %s in udp_sendonly\n" (Printexc2.to_string e);
        end      
    end

      
(*************************************************************************)
(*                                                                       *)
(*                         load_web_infos                                *)
(*                                                                       *)
(*************************************************************************) 

let load_web_infos () =
(* Try to connect the redirector to get interesting information, since we
are not allowed to use savannah anymore. The redirector should be able to
support the charge, at least, currently. *)
  let (name, port) = !!mlnet_redirector in
  let packet = gen_redirector_packet () in
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
              let pos = if motd_html_s <> "XX" then 
                  let servers_met_s, pos = L.get_string16 s pos in
                  let peers_ocl_s, pos = L.get_string16 s pos in
                  let motd_conf_s, pos = L.get_string16 s pos in
                  let peers_kad_s, pos = L.get_string16 s pos in
                  
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
                  
                  let peers_kad_file = Filename.temp_file "peers" ".kad" in
                  File.from_string peers_kad_file peers_kad_s;
                  load_file "kad" peers_kad_file;
                  
                  pos
                else
                
                
                let get_item s pos = 
                  let kind, pos = get_string32 s pos in
                  let file, pos = get_string32 s pos in
                  (kind, file), pos
                in
                let files, pos = L.get_list16 get_item s pos in
                List.iter (fun (kind, file) ->
                    let temp_file = Filename.temp_file "temp" ".mld" in
                    File.from_string temp_file file;
                    load_file kind temp_file
                ) files;
                pos
              in
              let ip = L.get_ip s pos in
              last_high_id := ip;
              
              lprintf "Redirector info loaded (IP set to %s)\n"
                (Ip.to_string ip);
              TcpBufferedSocket.set_lifetime sock 30.;
          
          
          ));
        write_string sock packet
        
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

type rss_feed = {
    mutable rss_date : int;
    mutable rss_value : Rss.channel;
  }
    
let rss_feeds = Hashtbl.create 10

  
let _ =
  add_web_kind "rss" (fun url filename ->
      let c = Rss.channel_of_file filename in
      let feed = 
        try Hashtbl.find rss_feeds url with
          Not_found ->
            let feed = {
                rss_date = 0;
                rss_value = c;
              } in
            Hashtbl.add rss_feeds url feed;
            feed
      in
      feed.rss_date <- last_time ();
      feed.rss_value <- c
  )

let initialized = ref false
let tcp_latencies_block = ref ""
let udp_latencies_block = ref ""
  
let _ =
(* Latency block *)
  add_redirector_info "LTCY" (fun buf ->
      
      if not !initialized then begin
          tcp_latencies_block := TcpBufferedSocket.get_latencies ();
          udp_latencies_block := UdpSocket.get_latencies ();
          initialized := true;
        end;
      
      buf_int buf !!loop_delay;
      
(* TCP block *)
      Buffer.add_string buf !tcp_latencies_block;
      
(* UDP block *)
      Buffer.add_string buf !udp_latencies_block;
  );
(* Every 6 hours *)
  add_infinite_timer 21600. (fun _ ->
      initialized := true;
      tcp_latencies_block := TcpBufferedSocket.get_latencies ();
      udp_latencies_block := UdpSocket.get_latencies ();
  )
  