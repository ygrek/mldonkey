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

  
let gui_protocol_used = ref 0
  
module UseSocket = struct
  
let (!!) = Options.(!!)

let connection = ref None

let disconnect gui = 
  match !connection with
    None -> ()
  | Some sock ->
      TcpBufferedSocket.close sock "user close";
      connection := None;
      when_disconnected gui

let send t =
  match !connection with
    None -> 
      Printf.printf "Message not sent since not connected";
      print_newline ();
  | Some sock ->
      GuiEncoding.gui_send (GuiEncoding.from_gui !gui_protocol_used) sock t

let reconnect gui value_reader =
  (try disconnect gui with _ -> ());
  let sock = TcpBufferedSocket.connect ""
      (try
        let h = Unix.gethostbyname 
            (if !!O.hostname = "" then Unix.gethostname () else !!O.hostname) in
        h.Unix.h_addr_list.(0)
      with 
        e -> 
          Printf.printf "Exception %s in gethostbyname" (Printexc2.to_string e);
          print_newline ();
          try 
            Unix.inet_addr_of_string !!O.hostname
          with e ->
              Printf.printf "Exception %s in inet_addr_of_string" 
                (Printexc2.to_string e);
              print_newline ();
              print_newline ();
              Printf.printf "mldonkey_gui was unable to find the IP address of the host [%s]" !!O.hostname; print_newline ();
              print_newline ();
              
              Printf.printf "Please, edit the $HOME/.mldonkey_gui.ini, and change the 'hostname' option"; print_newline ();
              Printf.printf "to the correct IP address of the host running mldonkey."; print_newline ();
              
              raise Not_found
      )
      !!O.port
      (fun _ _ -> ()) 
  in
  try
    connection := Some sock;
    TcpBufferedSocket.set_closer sock (fun _ _ -> 
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
    TcpBufferedSocket.set_reader sock (
      GuiDecoding.gui_cut_messages
        (fun opcode s ->
          try
            let m = GuiDecoding.to_gui !gui_protocol_used opcode s in
            value_reader gui m
          with e ->
            Printf.printf "Exception %s in decode/exec" (Printexc2.to_string e); 
	    print_newline ();
            raise e
        ));
    gui#label_connect_status#set_text "Connecting";
    send (GuiProto.GuiProtocol GuiEncoding.best_gui_version)
  with e ->
      Printf.printf "Exception %s in connecting" (Printexc2.to_string e);
      print_newline ();
      TcpBufferedSocket.close sock "error";
      connection := None

let connected _ = !connection <> None
      
end

module UseFifo = struct
    
    let timer_set = ref false
    
    let send m = Fifo.put gui_core_fifo (copy_message m)
    
    let disconnect gui = 
      when_disconnected gui
    
    let reconnect gui value_reader =       
      disconnect gui;      
      gui_reconnected := true;
      Fifo.clear core_gui_fifo;
      Fifo.clear gui_core_fifo;
      
      if not !timer_set then begin
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
                  Printf.printf "Exception %s in handle core message"
                    (Printexc2.to_string e); 
                  print_newline ();
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
    
