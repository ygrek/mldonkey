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

(** Communicating with the mldonkey client. *)

open Printf2
open BasicSocket
open TcpBufferedSocket
open Options
open CommonGlobals
open Gettext
open GuiProto
module M = Gui_messages
module O = Gui_options
module G = Gui_global

  
let copy_message t =
  if !!Gui_options.copy_messages then
    Marshal.from_string (Marshal.to_string t []) 0
  else t

let when_disconnected gui =
  gui#clear ;
  G.clear ()

  
let to_gui_protocol_used = Array.create (GuiDecoding.to_gui_last_opcode+1) 
  GuiEncoding.best_gui_version
let from_gui_protocol_used = Array.create
    (GuiDecoding.from_gui_last_opcode+1) 
  GuiEncoding.best_gui_version
  
module UseSocket = struct
  
let (!!) = Options.(!!)

let connection = ref None

let disconnect gui reason = 
  match !connection with
    None -> ()
  | Some sock ->
      TcpBufferedSocket.close sock reason;
      connection := None;
      when_disconnected gui

let send t =
  match !connection with
    None -> 
      lprintf "Message not sent since not connected";
      lprint_newline ();
  | Some sock ->
      GuiEncoding.gui_send (GuiEncoding.from_gui from_gui_protocol_used) sock t
          
let reconnect gui value_reader reason =
  (try disconnect gui reason with _ -> ());
  let hostname = if !!O.hostname = "" then Unix.gethostname () 
    else !!O.hostname in
  let sock = TcpBufferedSocket.connect ""
      (try
        let h = Ip.from_name hostname in
        Ip.to_inet_addr h
      with 
        e -> 
          lprintf "Exception %s in gethostbyname" (Printexc2.to_string e);
          lprint_newline ();
          try 
            Unix.inet_addr_of_string !!O.hostname
          with e ->
              lprintf "Exception %s in inet_addr_of_string" 
                (Printexc2.to_string e);
              lprint_newline ();
              lprint_newline ();
              lprintf "mlgui was unable to find the IP address of the host [%s]" !!O.hostname; lprint_newline ();
              lprint_newline ();
              
              lprintf "Please, edit the $HOME/.mldonkey_gui.ini, and change the 'hostname' option"; lprint_newline ();
              lprintf "to the correct IP address of the host running mldonkey."; lprint_newline ();
              
              raise Not_found
    )
    !!O.port
      (fun _ _ -> ()) 
  in
  
  
  if not (List.mem (hostname,!!O.port) !!O.history) then
    begin
      O.history =:= (hostname, !!O.port) :: !!O.history;
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
                gui#label_connect_status#set_text (gettext M.not_connected);
                when_disconnected gui
              end
    );
    TcpBufferedSocket.set_max_write_buffer sock !!O.interface_buffer;
    TcpBufferedSocket.set_handler sock TcpBufferedSocket.BUFFER_OVERFLOW
    (fun _ -> 
        lprintf "BUFFER OVERFLOW"; lprint_newline ();
        TcpBufferedSocket.close sock Closed_for_overflow);
    TcpBufferedSocket.set_reader sock (
      GuiDecoding.gui_cut_messages
        (fun opcode s ->
          try
            let m = GuiDecoding.to_gui to_gui_protocol_used opcode s in
            value_reader gui m;
          with e ->
              lprintf "Exception %s in decode/exec" (Printexc2.to_string e); 
              lprint_newline ();
              raise e
      ));
    gui#label_connect_status#set_text "Connecting";
    send (GuiProto.GuiProtocol GuiEncoding.best_gui_version)
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
    
    let reconnect gui value_reader reason =       
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
                    value_reader gui m 
                  done
                done
              with Fifo.Empty -> ()
              | e ->
                  lprintf "Exception %s in handle core message"
                    (Printexc2.to_string e); 
                  lprint_newline ();
          );
        end;      
      gui#label_connect_status#set_text "Connecting";
      send (GuiProto.GuiProtocol GuiEncoding.best_gui_version)
      
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
  let hostname = if !!O.hostname = "" then Unix.gethostname () else !!O.hostname
  in
  let ip = Ip.from_name hostname in
  let addr = Ip.to_inet_addr ip in
  let rec scan_port prev_next i max =
    if !prev_next && i < max then
      let next = ref true in
      prev_next := false;
      try
        let sock = TcpBufferedSocket.connect "" addr i (fun sock e -> 
              match e with
                BASIC_EVENT (RTIMEOUT) -> close sock Closed_for_timeout
              | _ -> ()
          ) in
        GuiEncoding.gui_send (GuiEncoding.from_gui from_gui_protocol_used) sock 
          (GuiProto.GuiProtocol GuiEncoding.best_gui_version);
        set_closer sock (fun _ _ -> 
            scan_port next (i+1) max);
        let proto = ref 0 in
        let nets = ref [] in
        let console = ref "" in
        TcpBufferedSocket.set_reader sock (fun sock nread ->
            GuiDecoding.gui_cut_messages
              (fun opcode s ->
                try
                  let m = GuiDecoding.to_gui to_gui_protocol_used
                      opcode s in
                  match m with
                    CoreProtocol n -> 
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
