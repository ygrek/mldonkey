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
  
  
module IntSet = Set.Make(struct 
      type t = int
      let compare = compare
    end)

type t =
  {
    progress_range :  GRange.progress_bar;
    mutable progress_sources : IntSet.t;
    progress_label_rate : GMisc.label;
    progress_label_sources : GMisc.label;
  }

  
let filename_max_length = 70

let ko = 1024.0
let mo = ko *. ko

(** To pretty-print a file size (int) *)
let size_of_int size =
  let f = Int64.to_float size in
  if f > mo then
    Printf.sprintf "%.1fM" (f /. mo)
  else
  if f > ko then
    Printf.sprintf "%.1fk" (f /. ko)
  else
    Int64.to_string size
    
let create title file (size : int64) f_stop =  
  let win =
    GWindow.window ~width:400 ~height:80 ~allow_shrink:true ~allow_grow:true
    ~auto_shrink:true ~modal:false ()
  in
  
  let vbox = GPack.vbox ~homogeneous:false ~packing:(win#add) () in
  let label = GMisc.label ~text:"Download" ~packing:vbox#add () in
  let label_filename = GMisc.label ~text:"" ~packing:vbox#add () in
  let hbox1 = GPack.hbox ~homogeneous:false ~packing:(vbox#add) () in
  let label_rate = GMisc.label ~text:"" ~packing:hbox1#add () in
  let label_sources = GMisc.label ~text:"0 sources" ~packing:hbox1#add () in
  let hbox = GPack.hbox ~homogeneous:false ~packing:(vbox#add) () in
  let range =
    GRange.progress_bar ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let wb_cancel =
    GButton.button ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let _65 =
    GMisc.label ~text:("Close") ~justify:`LEFT ~line_wrap:true
    ~packing:(wb_cancel#add) ()
  in
  let s_file =
    let len = String.length file in
    if len > filename_max_length then
      Printf.sprintf "%s...%s"
        (String.sub file (len - filename_max_length - 6) filename_max_length)
      (String.sub file (len - 3) 3)
    else
      file
  in
  let sources = IntSet.empty in
  let format_string = "%p%% of "^(size_of_int size) in
  ignore (wb_cancel#connect#clicked f_stop);
  win#set_title title;
  range#set_show_text true;
  range#set_format_string format_string;
  label_filename#set_text s_file;      
  range#configure ~current: 0.0 ~min: 0.0 ~max: (Int64.to_float size);
  win#show ();
  {
    progress_range = range;
    progress_sources = sources;
    progress_label_rate = label_rate;
    progress_label_sources = label_sources;
  }

let set_downloaded t  (done_size : int64) =
  t.progress_range#set_value (Int64.to_float done_size)  

let set_rate t rate =
  t.progress_label_rate#set_text (Printf.sprintf "%2.2f kB/s" (rate /. 1024.))  
      
let add_source t (src : int) = 
  t.progress_sources <- IntSet.add src t.progress_sources;
  t.progress_label_sources#set_text (Printf.sprintf "%d sources" 
      (IntSet.cardinal t.progress_sources))
      
let remove_source t (src : int) = 
  t.progress_sources <- IntSet.remove src t.progress_sources;
  t.progress_label_sources#set_text (Printf.sprintf "%d sources" 
      (IntSet.cardinal t.progress_sources))
