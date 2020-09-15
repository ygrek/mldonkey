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

open CommonTypes
(** Misc. functions. *)
module O = Gui_options
module G = Gui_global


open GuiProto

let ko = 1024L
let mo = Int64.mul ko ko
  
let unit_of_string s =
  match String.lowercase s with
    "mo" -> mo
  | "ko" -> ko
  | _ -> Int64.one

let ko = 1024.0
let mo = ko *. ko
let go = mo *. ko


let (!!) = Options.(!!)
let (=:=) = Options.(=:=)

let short_name n =
  let len = String.length n in
  if len > 35 then
    Printf.sprintf "%s...%s" (String.sub n 0 27) (String.sub n (len-5) 5)
  else n

let is_connected state =
  match state with
  | Connected_initiating
  | Connected_downloading _
  | Connected _ -> true
  | NotConnected _
  | Connecting
  | ServerFull
  | RemovedHost
  | BlackListedHost
  | NewHost -> false

let save_gui_options gui =
  (* Compute layout *)
  (
   match gui#window#children with
     [] -> ()
   | w :: _ ->
       let (w,h) = Gdk.Window.get_size w#misc#window in
       O.gui_width =:= w;
       O.gui_height =:= h
  );
  Options.save_with_help Gui_options.mldonkey_gui_ini

let set_hpaned (hpaned : GPack.paned) prop =
  let (w1,_) = Gdk.Window.get_size hpaned#misc#window in
  let ndx1 = (w1 * !!prop) / 100 in
  hpaned#child1#misc#set_geometry ~width: ndx1 ();
  hpaned#child2#misc#set_geometry ~width: (w1 - ndx1 - hpaned#handle_size) ()

let set_vpaned (hpaned : GPack.paned) prop =
  let (_,h1) = Gdk.Window.get_size hpaned#misc#window in
  let ndy1 = (h1 * !!prop) / 100 in
  hpaned#child1#misc#set_geometry ~height: ndy1 ();
  hpaned#child2#misc#set_geometry ~height: (h1 - ndy1 - hpaned#handle_size) ()
  

let get_hpaned gui (hpaned: GPack.paned) prop =
  
  ignore (hpaned#child1#coerce#misc#connect#size_allocate
      ~callback: (fun r ->
        let (w1,_) = Gdk.Window.get_size hpaned#misc#window in
        prop =:= r.Gtk.width * 100 / (max 1 (w1 - hpaned#handle_size));
        save_gui_options gui
    ))

let get_vpaned gui (hpaned: GPack.paned) prop =
  
  ignore (hpaned#child1#coerce#misc#connect#size_allocate
      ~callback: (fun r ->
        let (_,h1) = Gdk.Window.get_size hpaned#misc#window in
        prop =:= r.Gtk.height * 100 / (max 1 (h1 - hpaned#handle_size));
        save_gui_options gui
    ))

let create_search query_entry max_hits net search_type =
  let s = {
      GuiTypes.search_num = !Gui_global.search_counter ;
      GuiTypes.search_query = query_entry ;
      GuiTypes.search_max_hits = max_hits ;
      GuiTypes.search_type = search_type;
      GuiTypes.search_network = net;
  } 
  in
  incr Gui_global.search_counter;
  s

let rec rec_description_of_query q = 
  match q with
  | Q_HIDDEN l
  | Q_AND l
  | Q_OR l -> List.flatten (List.map rec_description_of_query l)

  | Q_ANDNOT (q1, q2) -> rec_description_of_query q1
  | Q_MODULE (_,q) -> rec_description_of_query q
        
  | Q_KEYWORDS (_,s) -> [s]
  | Q_MINSIZE _ 
  | Q_MAXSIZE _ -> []
  | Q_FORMAT (_,s)
  | Q_MEDIA  (_,s)
  | Q_MP3_ARTIST (_,s)
  | Q_MP3_TITLE (_,s)
  | Q_MP3_ALBUM (_,s) -> [s]

  | Q_COMBO _ -> []
      
  | Q_MP3_BITRATE _ -> []


(** Summarize a request in a few words *)
let description_of_query q =
  match rec_description_of_query q with 
    [] -> "stupid query"
  | [s] -> s
  | [s1 ; s2] -> s1^" "^s2
  | s1 :: s2 :: s3 :: _ -> s1^" "^s2^" "^s3
 

(** To pretty-print a file size (int32) *)
let size_of_int32 size =
  if !!Gui_options.use_size_suffixes then
    let f = Int64.to_float size in
    if f > go then
      Printf.sprintf "%.2fG" (f /. go)
    else
      if f > mo then
      Printf.sprintf "%.1fM" (f /. mo)
      else
        if f > ko then
          Printf.sprintf "%.1fk" (f /. ko)
        else
          Int64.to_string size
  else
    Int64.to_string size

(** To pretty-print a file size (int64) *)
let size_of_int64 size =
  if !!Gui_options.use_size_suffixes then
    let f = Int64.to_float size in
    if f > go then
      Printf.sprintf "%.2fG" (f /. go)
    else
      if f > mo then
      Printf.sprintf "%.1fM" (f /. mo)
      else
        if f > ko then
          Printf.sprintf "%.1fk" (f /. ko)
        else
          Int64.to_string size
  else
    Int64.to_string size

(** Return a color for a given name. *)
let color_of_name name =
  let accs = [| ref 0 ; ref 0 ; ref 0 |] in
  for i = 0 to (String.length name) - 1 do
    let m = i mod 3 in
    accs.(m) := !(accs.(m)) + Char.code name.[i]
  done;
  let r = !(accs.(0)) mod 210 in
  let g = !(accs.(1)) mod 210 in
  let b = !(accs.(2)) mod 210 in
  let s = Printf.sprintf "#%02X%02X%02X" r g b in
  `NAME s

let insert_buttons (wtool1: GButton.toolbar) (wtool2 : GButton.toolbar)
  ~text ~tooltip ~icon ~callback () =
  ignore
    (wtool1#insert_button 
      ~text: text
      ~tooltip: tooltip
      ~icon: (Gui_options.pixmap icon)#coerce
      ~callback: callback
      ());
  ignore
    (wtool2#insert_button 
      ~text: text
      ~tooltip: tooltip
      ~icon: (Gui_options.pixmap (icon ^ "_mini"))#coerce
      ~callback: callback
      ())
