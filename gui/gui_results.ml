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

(** GUI for the lists of files. *)

open Gui_global
open CommonTypes
open Gui_proto
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)

let first_name r = 
  match r.result_names with
    [] -> M.unknown
  | n :: _ -> n

let shorten_name s = Filename2.shorten !!O.max_result_name_len s
      
let string_of_tags tags =
  let buf = Buffer.create 100 in
      List.iter (fun t ->
          Buffer.add_string buf (Printf.sprintf "%-3s "
              (match t.tag_value with
                String s -> s
              | Uint32 i -> Int32.to_string i
              | Fint32 i -> Int32.to_string i
              | _ -> "???"
            ))
  ) tags;
  Buffer.contents buf

class box columns () =
  let titles = List.map Gui_columns.string_of_result_column columns in
  object (self)
    inherit [CommonTypes.result_info] Gpattern.plist `EXTENDED titles true as pl
    inherit Gui_results_base.box !!O.toolbars_style () as box 

    val mutable columns = columns
    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.string_of_result_column columns);
      self#update

    method download () = 
      List.iter
	(fun r -> 
	  Gui_com.send (Download_query (r.result_names, r.result_num)))
	self#selection

    method menu = 
      match self#selection with
	[] -> []
      |	_ -> [ `I (M.download, self#download) ]

    method compare_by_col col r1 r2 =
      match col with
	Col_result_name -> compare (first_name r1) (first_name r2)
      |	Col_result_md4 -> compare (Md4.to_string r1.result_md4) (Md4.to_string r2.result_md4)
      |	Col_result_size -> compare r1.result_size r2.result_size
      |	Col_result_format -> compare r1.result_format r2.result_format
      |	Col_result_props -> compare (string_of_tags r1.result_tags) (string_of_tags r2.result_tags)
      |	Col_result_comment -> compare r1.result_comment r2.result_comment
      | Col_result_network -> compare r1.result_network r2.result_network
          
    method compare r1 r2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
	try List.nth columns (abs - 1) 
	with _ -> Col_result_name
      in
      let res = self#compare_by_col col r1 r2 in
      current_sort * res

    method content_by_col col r =
      match col with
	Col_result_name -> shorten_name (first_name r)
      |	Col_result_md4 -> Md4.to_string r.result_md4
      |	Col_result_size -> Printf.sprintf "%10s" (Int32.to_string r.result_size)
      |	Col_result_format -> r.result_format
      |	Col_result_props -> string_of_tags r.result_tags
          | Col_result_network -> network_name r.result_network
      |	Col_result_comment -> match r.result_comment with None -> "" | Some s -> s
              
    method content r =
      let strings = List.map 
	  (fun col -> P.String (self#content_by_col col r))
	  columns
      in
      (strings, None)

    method set_tb_style = wtool#set_style

    initializer
      box#vbox#pack ~expand: true pl#box ;

      ignore
	(wtool#insert_button 
	   ~text: M.download
	   ~tooltip: M.download
	   ~icon: (Gui_icons.pixmap M.o_xpm_download)#coerce
	   ~callback: self#download
	   ()
	);
  end

class search_result_box () =
  let hbox_labels = GPack.hbox () in
  let wl_count = GMisc.label ~text: "" 
      ~packing: (hbox_labels#pack ~expand: false ~padding:3) 
      () 
  in
  let wl_wait = GMisc.label ~text: "" 
      ~packing: (hbox_labels#pack ~expand: false ~padding:3) 
      () 
  in
  object (self)
    inherit box !!Gui_options.results_columns () as box

    method add_result (res : CommonTypes.result_info) =
      data <- data @ [res];
      self#insert ~row: self#wlist#rows res;
      wl_count#set_text 
	(Printf.sprintf "%d %s" (List.length data) M.results)

    method set_waiting n =
      wl_wait#set_text (M.waiting_for_replies n)

    initializer
      box#vbox#pack ~expand: false hbox_labels#coerce ;
      
  end
