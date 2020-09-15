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

(** GUI for console. *)

open CommonTypes
open GuiProto

module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)

let max_console_lines = ref 500
let line_threshold = 50
  
class box () =
  object (self)
    inherit Gui_console_base.box () as box
    
    val mutable lines = Fifo.create ()
    val mutable removed_size = 0
    val mutable current_pos = 0
    val mutable history = Fifo.create ()
    
    method set_list_bg bg font =
      let style = text#misc#style#copy in
      style#set_base [ (`NORMAL, bg)];
      style#set_font font;
      text#misc#set_style style

    method insert_line t =
      if Fifo.length lines > !max_console_lines + line_threshold then begin
          let (line_pos, len) = Fifo.take lines in
          let line_pos = line_pos - removed_size in
          let rec iter n len =
            if n = 0 then len else
            let (_, addlen) = Fifo.take lines in
            iter (n-1) (len+addlen)
          in
          let len = iter line_threshold len in
          text#delete_text ~start: line_pos ~stop: (line_pos+len);
          removed_size <- removed_size + len;
        end;
      let len = String.length t in
      Fifo.put lines (current_pos, len);
      current_pos <- current_pos + len;
      ignore (text#insert_text t ~pos: text#length (* ~pos:0 *) );
    
    method insert t =
      text#freeze ();
      let rec iter list =
        match list with
          [] | [""] -> ()
        | s:: tail -> self#insert_line (s ^ "\n"); iter tail 
      in 
      iter (String2.split t '\n');
      text#thaw ();
      text#set_position text#length
      
      
    method on_entry_return () =
      match we_command#entry#text with
        "" -> ()
      | s ->
          Gui_com.send (GuiProto.Command s);
          if not (Fifo.mem history s) then begin
              Fifo.put history s;
(*              we_command#set_item_string (GList.list_item ~label:s ()) s;*)
              
              let list = we_command#list in
              let strings = Fifo.to_list history in

              we_command#disable_activate ();
              list#clear_items ~start:0 ~stop:(-1);
              List.iter (fun s ->
                  let li = GList.list_item ~label: s () in
                  li#misc#show ();
                  list#add li;
              ) strings;

              
              (*we_command#set_popdown_strings strings*)
            end;
          we_command#entry#set_text "";

    initializer
      (try 
          let font = Gdk.Font.load_fontset "fixed" in
          let style = text#misc#style#copy in
          style#set_font font;
          text#misc#set_style style;
        with _ -> ());
      we_command#set_use_arrows `ALWAYS;
      Okey.add we_command#entry
        ~mods: []
        GdkKeysyms._Return
        self#on_entry_return;

      ignore (wb_clear_console#connect#clicked
          (fun () -> 
            text#delete_text 0 text#length;
            Fifo.clear lines;
            removed_size <- 0;
            current_pos <- 0;
            ));
  end
