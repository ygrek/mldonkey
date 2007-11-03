(* Copyright 2004 b8_bavard INRIA *)
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

(* Communicating with the mldonkey client. *)

open Printf2
open BasicSocket
open TcpBufferedSocket
open Options
open CommonGlobals
open Gettext
open GuiProto
open GuiTypes2

module M = GuiMessages
module O = GuiOptions
module G = GuiGlobal


let copy_message t =
  if !!O.gtk_misc_copy_messages then
    Marshal.from_string (Marshal.to_string t []) 0
  else t

let when_disconnected (gui : gui) =
  gui.set_corestatus Core_notconnected;
  G.clear ();
  gui.clear ()

  
let to_gui_protocol_used = Array.create (to_gui_last_opcode+1) 
  GuiProto.best_gui_version
let from_gui_protocol_used = Array.create
    (from_gui_last_opcode+1) 
  GuiProto.best_gui_version
  
module UseSocket = struct
  
let (!!) = Options.(!!)

let connection = ref None

let disconnect gui reason =
  match !connection with
    None -> ()
  | Some sock ->
      when_disconnected gui;
      TcpBufferedSocket.close sock reason;
      connection := None


let send t =
  match !connection with
    None -> ()
  | Some sock ->
      GuiEncoding.gui_send (GuiEncoding.from_gui from_gui_protocol_used) sock t
          
let reconnect (gui : gui) value_reader arg reason =
  (try disconnect gui reason with _ -> ());
  let hostname = if !!O.gtk_client_hostname = "" then Unix.gethostname () 
    else !!O.gtk_client_hostname in
  let token = create_token unlimited_connection_manager in
(*  lprintf "RECONNECTING...\n"; *)
  let sock = TcpBufferedSocket.connect token ""
      (try
        let h = Ip.from_name hostname in
        Ip.to_inet_addr h
      with 
        e -> 
          lprintf "Exception %s in gethostbyname" (Printexc2.to_string e);
          lprint_newline ();
          try 
            Unix.inet_addr_of_string !!O.gtk_client_hostname
          with e ->
              lprintf "Exception %s in inet_addr_of_string" 
                (Printexc2.to_string e);
              lprint_newline ();
              lprint_newline ();
              lprintf "mlgui was unable to find the IP address of the host [%s]"
                !!O.gtk_client_hostname; lprint_newline ();
              lprint_newline ();
              
              lprintf "Please, edit the $HOME/.mldonkey/mlgui.ini, and change the 'hostname' option\n"; 
              lprintf "to the correct IP address of the host running mldonkey.\n"; 
              raise Not_found
    )
    !!O.gtk_client_port
      (fun _ _ -> ()) 
  in
(*  lprintf "CONNECTION STARTED\n"; *)
  
  if not (List.mem (hostname,!!O.gtk_client_port) !!O.gtk_client_history) then
    begin
      O.gtk_client_history =:= (hostname, !!O.gtk_client_port) :: !!O.gtk_client_history;
      G.new_scanned_port := true
    end;  

  try
    connection := Some sock;
    TcpBufferedSocket.set_closer sock (fun _ msg -> 
        match !connection with
          None -> ()
        | Some s -> 
            if s == sock then begin
                connection := None;
                when_disconnected gui
              end
    );
    TcpBufferedSocket.set_max_output_buffer sock !!O.gtk_misc_interface_buffer;
    TcpBufferedSocket.set_max_input_buffer sock !!O.gtk_misc_interface_buffer;
    TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW  
    (fun _ -> 
        lprintf "BUFFER OVERFLOW\n"; 
        TcpBufferedSocket.close sock Closed_for_overflow);
    TcpBufferedSocket.set_reader sock (
      GuiDecoding.gui_cut_messages
        (fun opcode s ->
          try
            let m = GuiDecoding.to_gui to_gui_protocol_used opcode s in
            value_reader arg m;
          with e ->
              lprintf "Exception %s in decode/exec\n" (Printexc2.to_string e); 
              raise e
      ));
    gui.set_corestatus Core_connecting;
    send (GuiProto.GuiProtocol GuiProto.best_gui_version)
  with e ->
      lprintf "Exception %s in connecting\n" (Printexc2.to_string e);
      TcpBufferedSocket.close sock (Closed_for_exception e);
      connection := None
      
let connected _ = !connection <> None
      
end

module UseFifo = struct
    
    let timer_set = ref false
    
    let send m = Fifo.put gui_core_fifo (copy_message m)
    
    let disconnect gui reason = 
      when_disconnected gui
    
    let reconnect ( gui : gui) value_reader arg reason =       
      disconnect gui reason;      
      gui_reconnected := true;
      Fifo.clear core_gui_fifo;
      Fifo.clear gui_core_fifo;
      
      if not !timer_set then begin
          timer_set := true;
          BasicSocket.add_infinite_timer 0.1 (fun _ -> 
              try
                while true do
                  while true do
                    let m = copy_message (Fifo.take core_gui_fifo) in
                    value_reader arg m 
                  done
                done
              with Fifo.Empty -> ()
              | e ->
                  lprintf "Exception %s in handle core message"
                    (Printexc2.to_string e); 
                  lprint_newline ();
          );
        end;      
      gui.set_corestatus Core_connecting;
      send (GuiProto.GuiProtocol GuiProto.best_gui_version)
      
    let connected _ = true
      
  end
  
let disconnect = 
  if !core_included then UseFifo.disconnect
  else UseSocket.disconnect
  
let connected = 
  if !core_included then UseFifo.connected
  else UseSocket.connected
  
let reconnect = 
  if !core_included then UseFifo.reconnect
  else UseSocket.reconnect
  
let send = 
  if !core_included then UseFifo.send
  else UseSocket.send
      
let scan_ports () =
  let hostname = if !!O.gtk_client_hostname = "" then Unix.gethostname () else !!O.gtk_client_hostname
  in
  let ip = Ip.from_name hostname in
  let addr = Ip.to_inet_addr ip in
  let rec scan_port prev_next i max =
    if !prev_next && i < max then
      let next = ref true in
      prev_next := false;
      try
        let token = create_token unlimited_connection_manager in
        let sock = TcpBufferedSocket.connect token "" addr i (fun sock e -> 
              match e with
                BASIC_EVENT (RTIMEOUT) -> close sock Closed_for_timeout
              | _ -> ()
          ) in
        GuiEncoding.gui_send (GuiEncoding.from_gui from_gui_protocol_used) sock 
          (GuiProto.GuiProtocol GuiProto.best_gui_version);
        set_closer sock (fun _ _ -> 
            scan_port next (i+1) max);
        let proto = ref 0 in
        let nets = ref [] in
        let _console = ref "" in
        TcpBufferedSocket.set_reader sock (fun sock nread ->
            GuiDecoding.gui_cut_messages
              (fun opcode s ->
                try
                  let m = GuiDecoding.to_gui to_gui_protocol_used opcode s in
                  match m with
                    CoreProtocol (n, _, _) -> 
                      lprintf "GUI version %d on port %d" n i;
                      lprint_newline ();
                      proto := n
                  | Network_info n ->
                      nets := n.CommonTypes.network_netname :: !nets
                  | Console m ->
                      lprintf "GUI:\n proto %d\nnets:\n" !proto; lprint_newline ();
                      List.iter (fun n ->
                          lprintf "%s " n
                      ) !nets;
                      lprint_newline ();
                      lprintf " motd:\n%s" m;
                      lprint_newline ();
                      
                      if not (List.mem (hostname,i) !G.scanned_ports) then
                        begin
                          G.scanned_ports := (hostname, i) :: !G.scanned_ports;
                          G.new_scanned_port := true
                          
                        end
                      
                  | _ -> close sock Closed_by_user
              with e -> close sock Closed_by_user
            ) sock nread
            
            );
        
        set_rtimeout sock 0.5;
      with e -> 
          scan_port next (i+1) max
  in
  scan_port (ref true) 0 10000;
  scan_port (ref true) 10000 20000;
  scan_port (ref true) 20000 30000;
  scan_port (ref true) 30000 40000;
  scan_port (ref true) 40000 50000;
  scan_port (ref true) 50000 60000;
  scan_port (ref true) 60000 65536
