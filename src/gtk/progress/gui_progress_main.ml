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

open CommonOptions
open CommonTypes
open GuiTypes
open GuiProto
  
  
open Gui_options
open Gui_global

open Gui_progress
  
let verbose_gui_messages = false
  
(*module Gui_rooms = Gui_rooms2*)
let chmod_config () =
  let base_config = 
    (Filename.concat CommonOptions.config_dir "mldonkey_gui.ini")
  in
  let save_config =
    base_config^".old"
  in
  begin
    if Sys.file_exists base_config then
      Unix.chmod base_config 0o600
    else
      ()
  end;
  begin
    if Sys.file_exists save_config then
      Unix.chmod save_config 0o600
    else
      ()
  end

let watch_file = ref (-1)
  
let _ = 
  (try Options.load mldonkey_gui_ini with
      Sys_error _ ->
        (try Options.save mldonkey_gui_ini with _ -> ())
    | e ->
        lprintf "Exception %s in load options %s\n" 
          (Printexc2.to_string e)
        (Options.options_file_name mldonkey_gui_ini);
  );
  let args = 
    [
      "-file", Arg.Int ( (:=) watch_file)
      , ": the file you want to monitor"
    ] in
  Arg.parse args (fun s ->
      Arg.usage args s; exit 2)  "mlgui: the GUI to use with mldonkey"

let progress_window = ref None
let progress f =
  match !progress_window with
    None -> ()
  | Some box -> f box

      (*
let _ =
(*    ignore (GMain.Main.init ()); *)
  progress_window := Some 
    (new file_progress "Test" "Foo.avi" 20000000
      (fun () -> exit 0));
  let cpt = ref 0 in
  while true do
    cpt := (!cpt + 100) mod 20000000;
    progress !cpt
  done
*)
  
let value_reader gui t =
  try    
    if verbose_gui_messages then begin
        lprintf "MESSAGE RECEIVED: %s\n" 
          (string_of_to_gui t);
      end;
    
    match t with
    
    | CoreProtocol v -> 
        
        let version = min v GuiEncoding.best_gui_version in
        for i = 0 to GuiDecoding.to_gui_last_opcode do
          Gui_com.to_gui_protocol_used.(i) <- version;
        done;
        for i = 0 to GuiDecoding.from_gui_last_opcode do
          Gui_com.from_gui_protocol_used.(i) <- version;
        done;
        lprintf "Using protocol %d for communications\n" version;
        Gui_com.send (Password (!!login, !!password))

(* The messages we are interested in ... *)
    | File_add_source (num, src) -> 
        if num = !watch_file then begin
            lprintf "File_add_source %d\n" num;
            progress (fun win -> Gui_progress.add_source win src);  
          end
          
    | File_remove_source (num, src) -> 
        if num = !watch_file then begin
            lprintf "File_remove_source %d\n" num;        
            progress (fun win -> Gui_progress.remove_source win src);
          end
          
    | File_downloaded (num, downloaded, rate, last_seen) -> 
        if num = !watch_file then begin
            lprintf "File_downloaded %d\n" num;
            progress (fun win -> Gui_progress.set_downloaded win downloaded);
            progress (fun win -> Gui_progress.set_rate win rate);
          end
          
    | File_update_availability (num, src, avail) -> 
        if num = !watch_file then begin
            lprintf "File_update_availability %d\n" num;
          end
          
    | File_info f -> 
        if f.file_num = !watch_file then begin
            lprintf "File info %d\n" f.file_num;            
            begin
              match !progress_window with
              | Some _ -> ()
              | None ->
                  progress_window := Some 
                    (Gui_progress.create "MLdonkey" f.file_name f.file_size
                      (fun () -> exit 0));
            end;
            progress (fun win -> Gui_progress.set_downloaded win f.file_downloaded);
            progress (fun win -> Gui_progress.set_rate win f.file_download_rate);
          end
    | _ -> ()
  with e ->
      lprintf "Exception %s in reader\n" (Printexc2.to_string e)
  
class gui () =
  object
    method clear = ()
    method set_connect_status ( s : string) = ()
end

let gui = new gui ()

let _ =
  CommonGlobals.gui_included := true;
  let quit () = 
    chmod_config (); 
    CommonGlobals.exit_properly 0
  in
  chmod_config ();   
  (** connection with core *)
  Gui_com.reconnect gui value_reader gui BasicSocket.Closed_by_user ;
(*  BasicSocket.add_timer 2.0 update_sizes;*)
  let never_connected = ref true in
  BasicSocket.add_timer 1.0 (fun timer ->
      
      if !never_connected && not (Gui_com.connected ()) then  begin
          BasicSocket.reactivate_timer timer;
          Gui_com.reconnect gui value_reader () BasicSocket.Closed_by_user
        end else
        never_connected := false
  )
