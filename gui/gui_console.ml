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

class box () =
  object (self)
    inherit Gui_console_base.box () as box

    method insert t =
      ignore (text#insert_text t ~pos:0);
      text#set_position 0;

    method on_entry_return () =
      match we_command#text with
	"" -> ()
      |	s ->
	  Gui_com.send (GuiProto.Command s);
	  we_command#set_text ""

    initializer
      let font = Gdk.Font.load_fontset "fixed" in
      let style = text#misc#style#copy in
      style#set_font font;
      text#misc#set_style style;

      Okey.add we_command
	~mods: []
	GdkKeysyms._Return
	self#on_entry_return;

      ignore (wb_clear_console#connect#clicked
		(fun () -> text#delete_text 0 text#length));
  end
