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
open GuiTypes
open GuiProto
open Gui_columns

module M = Gui_messages
module P = Gpattern
module O = Gui_options
module G = Gui_global

let (!!) = Options.(!!)

let file_first_name f = f.file_name

let string_of_file_state state =
  match state with
    FileDownloading -> M.downloading
  | FileCancelled -> M.cancelled
  | FilePaused -> M.paused
  | FileDownloaded
  | FileShared  -> M.dl_done
  | FileNew -> assert false
      
let some_is_available f =
  let b = ref false in
  let len = String.length f.file_availability in
  for i = 0 to len - 1 do
    b := !b or f.file_availability.[i] <> '0'
  done;
  !b

let color_opt_of_file f =
  if f.file_download_rate > 0. then
    Some !!O.color_downloading
  else if some_is_available f then
    Some !!O.color_available
  else
    Some !!O.color_not_available

let float_avail s = 
  try float_of_string s
  with _ -> 0.0

let string_availability s =
  let len = String.length s in
  let p = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] <> '0' then begin
      incr p
    end
  done;
  if len = 0 then "" else 
  Printf.sprintf "%5.1f" (float_of_int !p /. float_of_int len *. 100.)

let string_of_format format =
  match format with
    AVI f ->
      Printf.sprintf "AVI: %s %dx%d %d fps %d bpf"
	f.avi_codec f.avi_width f.avi_height 
	f.avi_fps f.avi_rate
  | Mp3 tag ->
      Printf.sprintf "MP3: %s - %s (%d): %s"
	tag.Mp3tag.artist tag.Mp3tag.album 
	tag.Mp3tag.tracknum tag.Mp3tag.title
  | _ -> M.unknown

class box columns sel_mode () =
  let titles = List.map Gui_columns.File.string_of_column columns in
  object (self)
    inherit [file_info] Gpattern.plist sel_mode titles true as pl
    inherit Gui_downloads_base.box () as box

    val mutable columns = columns

    method set_columns l =
      columns <- l;
      self#set_titles (List.map Gui_columns.File.string_of_column columns);
      self#update

    method box = box#coerce
    method vbox = box#vbox

    method compare_by_col col f1 f2 =
      match col with
      | Col_file_name -> compare f1.file_name f2.file_name
      |	Col_file_size -> compare f1.file_size f2.file_size
      |	Col_file_downloaded -> compare f1.file_downloaded f2.file_downloaded
      |	Col_file_percent -> compare 
	    (Int32.to_float f1.file_downloaded /. Int32.to_float f1.file_size) 
	    (Int32.to_float f2.file_downloaded /. Int32.to_float f2.file_size) 
      |	Col_file_rate-> compare f1.file_download_rate f2.file_download_rate
      |	Col_file_state -> compare f1.file_state f2.file_state
      |	Col_file_availability -> compare 
	    (float_avail (string_availability f1.file_availability))
	    (float_avail (string_availability f2.file_availability))
      |	Col_file_md4 -> compare (Md4.to_string f1.file_md4) (Md4.to_string f2.file_md4)
      |	Col_file_format -> compare f1.file_format f2.file_format
      | Col_file_network -> compare f1.file_network f2.file_network
          
    method compare f1 f2 =
      let abs = if current_sort >= 0 then current_sort else - current_sort in
      let col = 
	try List.nth columns (abs - 1) 
	with _ -> Col_file_name
      in
      let res = self#compare_by_col col f1 f2 in
      current_sort * res

    method content_by_col f col =
      match col with
	Col_file_name -> 
	  let s_file = Gui_misc.short_name f.file_name in
	  s_file
      |	Col_file_size ->
	  Gui_misc.size_of_int32 f.file_size 
      |	Col_file_downloaded ->
	  Gui_misc.size_of_int32 f.file_downloaded
      |	Col_file_percent ->
	  Printf.sprintf "%5.1f" 
	    (Int32.to_float f.file_downloaded /. Int32.to_float f.file_size *. 100.)
      |	Col_file_rate ->
	  if f.file_download_rate > 0. then
            Printf.sprintf "%5.1f" (f.file_download_rate /. 1024.)
          else ""
      |	Col_file_state ->
	  string_of_file_state f.file_state 
      |	Col_file_availability ->
	  let len = String.length f.file_availability in
          let p = ref 0 in
          for i = 0 to len - 1 do
            if f.file_availability.[i] <> '0' then begin
              incr p
            end
          done;
          if len = 0 then "" else 
          Printf.sprintf "%5.1f" (float_of_int !p /. float_of_int len *. 100.)
      | Col_file_md4 -> Md4.to_string f.file_md4
      | Col_file_format -> string_of_format f.file_format
      | Col_file_network -> Gui_global.network_name f.file_network

          
    method content f =
      let strings = List.map 
	  (fun col -> P.String (self#content_by_col f col))
	  columns 
      in
      let col_opt = 
	match color_opt_of_file f with
	  None -> Some `BLACK
	| Some c -> Some (`NAME c)
      in
      (strings, col_opt)

    method find_file num =
      let rec iter n l =
	match l with
	  [] -> raise Not_found
	| fi :: q ->
	    if fi.file_num = num then
	      (n, fi)
	    else
	      iter (n+1) q
      in
      iter 0 data

    method remove_file f row =
      self#wlist#remove row;
      data <- List.filter (fun fi -> fi.file_num <> f.file_num) data ;
      selection <- List.filter (fun fi -> fi.file_num <> f.file_num) selection

    method set_tb_style = wtool#set_style

    method clear = self#update_data []

    initializer
      box#vbox#pack ~expand: true pl#box;
      

  end
    
class box_downloaded wl_status () =
  object (self)
    inherit box !!O.downloaded_columns `SINGLE () as super

    method update_wl_status : unit =
      wl_status#set_text 
	(Gui_messages.downloaded_files !G.ndownloaded !G.ndownloads)

    method content f = 
      (fst (super#content f), Some (`NAME !!O.color_downloaded))

    method save_menu_items f =
      let items = 
	List.map
	  (fun name ->
	    `I (name, 
		(fun _ -> 
                  Gui_com.send (GuiProto.SaveFile (f.file_num, name))
                )
	       )
	  ) 
	  f.file_names
      in
      items

    method save () =
      match self#selection with
	f :: _ ->
	  let items = self#save_menu_items f in
	  GToolbox.popup_menu ~entries: items ~x: 0 ~y: 0
      |	_ ->
	  ()

    method save_all () = 
      List.iter
	(fun f -> Gui_com.send (GuiProto.SaveFile (f.file_num, file_first_name f)))
	data

    method save_as () = 
      match self#selection with
	file :: _ ->
	  (
	   let file_opt = GToolbox.input_string ~title: M.save 
               M.save in
           match file_opt with
             None -> ()
           | Some name -> 
               Gui_com.send (GuiProto.SaveFile (file.file_num, name))
	  )
      |	_ ->
	  ()

    method edit_mp3_tags () = 
      match self#selection with
	file :: _ ->
	  (
	   match file.file_format with
             Mp3 tag ->
               Mp3_ui.edit_tag_v1 M.edit_mp3 tag ;
               Gui_com.send (GuiProto.ModifyMp3Tags (file.file_num, tag))
	   | _ ->
	       ()
	  )
      |	_ ->
	  ()

    method menu =
      match self#selection with
	[] -> [ `I (M.save_all, self#save_all) ]
      |	file :: _ ->
	  [ `I (M.save_as, self#save_as) ;
	    `M (M.save, self#save_menu_items file) ;
	  ] @
	  (match file.file_format with
	    Mp3 _ -> [`I (M.edit_mp3, self#edit_mp3_tags)]
	  | _ -> []) @
	  [ `S ;
	    `I (M.save_all, self#save_all) 
	  ]

    (** {2 Handling core messages} *)

    method h_downloaded f =
      try
	ignore (self#find_file f.file_num)
      with
        Not_found ->
          incr ndownloaded;
	  self#update_wl_status ;
	  data <- data @ [f];
	  self#insert ~row: self#wlist#rows f

    method h_removed f =
      try
        let (row, _) = self#find_file f.file_num in
        decr ndownloaded;
	self#update_wl_status ;
        self#remove_file f row
      with
	Not_found ->
	  ()

    initializer
      ignore
	(wtool#insert_button 
	   ~text: M.save
	   ~tooltip: M.save
	   ~icon: (Gui_icons.pixmap M.o_xpm_save)#coerce
	   ~callback: self#save
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: M.save_as
	   ~tooltip: M.save_as
	   ~icon: (Gui_icons.pixmap M.o_xpm_save_as)#coerce
	   ~callback: self#save_as
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: M.save_all
	   ~tooltip: M.save_all
	   ~icon: (Gui_icons.pixmap M.o_xpm_save_all)#coerce
	   ~callback: self#save_all
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: M.edit_mp3
	   ~tooltip: M.edit_mp3
	   ~icon: (Gui_icons.pixmap M.o_xpm_edit_mp3)#coerce
	   ~callback: self#edit_mp3_tags
	   ()
	);
  end

let colorGreen = `NAME "green"
let colorRed   = `NAME "red"
let colorBlue  = `NAME "blue"
let colorWhite =`WHITE
let colorBlack = `BLACK

let drawing = ref (None :   [ `window] GDraw.drawable option)
  
let redraw_chunks draw_avail file =
  let drawing = match !drawing with
      None -> 
        
        let w = draw_avail#misc#window in
        let d = new GDraw.drawable w in
        draw_avail#misc#show ();
        drawing := Some d;
        d        
    | Some d -> d
  in

  let wx, wy = drawing#size in
  drawing#set_foreground colorWhite;
  drawing#rectangle ~filled: true ~x:0 ~y:0 ~width:wx ~height:wy ();
  
  let nchunks = String.length file.file_chunks in
  let dx = min 3 (wx / nchunks) in
  let offset = (wx - nchunks * dx) / 2 in
  let offset = if offset < 0 then 0 else offset in
  for i = 0 to nchunks - 1 do
    drawing#set_foreground (
    if file.file_chunks.[i] = '1' then
      colorGreen
    else
      match  file.file_availability.[i] with
        '0' -> colorRed
      | '1' -> colorBlue
      | _ -> colorBlack);
    drawing#rectangle ~filled: true
      ~x:(offset + i*dx) ~y: 0 
      ~width: dx ~height:wy ()
  done
    
class box_downloads box_locs wl_status () =
  let draw_availability =
    GMisc.drawing_area ~height:20
      ()
  in
  let label_file_info = GMisc.label () in
  object (self)
    inherit box !!O.downloads_columns `EXTENDED ()

    method update_wl_status : unit =
      wl_status#set_text 
	(Gui_messages.downloaded_files !G.ndownloaded !G.ndownloads)

    method cancel () =
      let s = Gui_messages.ask_cancel_download_files
	  (List.map (fun f -> file_first_name f) self#selection)
      in
      match GToolbox.question_box Gui_messages.cancel 
	  [ Gui_messages.yes ; Gui_messages.no] s 
      with
	1 ->
	  List.iter
	    (fun f -> Gui_com.send (RemoveDownload_query f.file_num))
	    self#selection
      |	_ ->
	  ()

    method retry_connect () =
      List.iter
	(fun f -> Gui_com.send (ConnectAll f.file_num))
	self#selection

    method pause_resume () =
      List.iter
        (fun f -> Gui_com.send (SwitchDownload (f.file_num,
              match f.file_state with
                FilePaused -> true
              | _ -> false
            )))
	self#selection

    method verify_chunks () =
      List.iter
	(fun f -> Gui_com.send (VerifyAllChunks f.file_num))
	self#selection

    method preview () =
      List.iter
	(fun f -> Gui_com.send (Preview f.file_num))
	self#selection

    method get_format () =
      List.iter
	(fun f -> Gui_com.send (QueryFormat f.file_num))
	self#selection

    method menu =
      match self#selection with
	[] -> []
      |	_ ->
	  [ `I (M.pause_resume_dl, self#pause_resume) ;
	    `I (M.retry_connect, self#retry_connect) ;
	    `I (M.cancel, self#cancel) ;
	    `I (M.verify_chunks, self#verify_chunks) ;
	    `I (M.preview, self#preview) ;
	    `I (M.get_format, self#get_format) ;
	  ] 

    method on_select file =
      label_file_info#set_text 
	(
         Printf.sprintf "NAME: %s SIZE: %s FORMAT: %s" 
	   (file.file_name)
           (Int32.to_string file.file_size) 
           (string_of_format file.file_format)
           ;
	);
      redraw_chunks draw_availability file;
      begin
        box_locs#update_data_by_file (Some file)
      end

    method on_deselect _ =
      box_locs#update_data_by_file None

    (** {2 Handling core messages} *)

    method update_file f f_new row =
      f.file_md4 <- f_new.file_md4 ;
      f.file_name <- f_new.file_name ;
      f.file_names <- f_new.file_names ;
      f.file_size <- f_new.file_size ;
      f.file_downloaded <- f_new.file_downloaded ;
      f.file_nlocations <- f_new.file_nlocations ;
      f.file_nclients <- f_new.file_nclients ;
      f.file_state <- f_new.file_state ;
      f.file_chunks <- f_new.file_chunks ;
      f.file_availability <- f_new.file_availability ;
      if f_new.file_sources <> None then
        f.file_sources <- f_new.file_sources ;
      f.file_download_rate <- f_new.file_download_rate ;
      f.file_format <- f_new.file_format;
      self#update_row f row;

    method h_file_downloaded num dled rate =
      try
	let (row, f) = self#find_file num in
	self#update_file f
	  { f with file_downloaded = dled ; file_download_rate = rate } 
	  row
      with
	Not_found ->
	  ()

    method h_paused f =
      try
	let (row, fi) = self#find_file f.file_num in
	self#update_file fi f row
      with
        Not_found ->
          incr ndownloads;
	  self#update_wl_status ;
          data <- data @ [f];
          self#insert ~row: self#wlist#rows f;
      
    method h_cancelled f = 
      try
        let (row, fi) = self#find_file f.file_num in
        decr ndownloads;
	self#update_wl_status ;
        self#remove_file fi row
      with
	Not_found ->
	  ()

    method h_downloaded = self#h_cancelled 
    method h_downloading f = self#h_paused f

    method h_file_availability num chunks avail =
      try
	let (row, f) = self#find_file num in
	self#update_file f 
	  { f with file_chunks = chunks ; file_availability = avail } 
	  row
      with Not_found -> ()

    method remove_client c =
      List.iter (fun file ->
          match file.file_sources with
            None -> ()
          | Some sources ->
              if List.memq c sources then
                let (row, file) = self#find_file file.file_num in
(*                Printf.printf "Removing client from sources";
                print_newline (); *)
                self#update_file file { file with file_sources = Some (
                    List.filter (fun cc -> cc != c) sources) } 
                row
      ) data
          
    method h_file_location num src =
      try
(*        Printf.printf "Source %d for %d" src num;  print_newline (); *)
        let (row, f) = self#find_file num in
        self#update_file f 
        { f with 
          file_sources = Some (
            (match f.file_sources with
                None -> [src]
              | Some list -> if not (List.mem src list) then
                    list@[src] else list ))
        } 
          row
      with Not_found -> 
          (* some sources are sent for shared files in eDonkey. have to fix that *)
(*          Printf.printf "No such file %d" num; print_newline () *)
	()

    initializer
      ignore
	(wtool#insert_button 
	   ~text: M.cancel
	   ~tooltip: M.cancel
	   ~icon: (Gui_icons.pixmap M.o_xpm_cancel)#coerce
	   ~callback: self#cancel
	   ()
	);
      ignore
	(wtool#insert_button 
	   ~text: M.retry_connect
	   ~tooltip: M.retry_connect
	   ~icon: (Gui_icons.pixmap M.o_xpm_retry_connect)#coerce
	   ~callback: self#retry_connect
	   ()
	);
      ignore
	(wtool#insert_button 
	   ~text: M.pause_resume_dl
	   ~tooltip: M.pause_resume_dl
	   ~icon: (Gui_icons.pixmap M.o_xpm_pause_resume)#coerce
	   ~callback: self#pause_resume
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: M.verify_chunks
	   ~tooltip: M.verify_chunks
	   ~icon: (Gui_icons.pixmap M.o_xpm_verify_chunks)#coerce
	   ~callback: self#verify_chunks
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: M.preview
	   ~tooltip: M.preview
	   ~icon: (Gui_icons.pixmap M.o_xpm_preview)#coerce
	   ~callback: self#preview
	   ()
	);

      ignore
	(wtool#insert_button 
	   ~text: M.get_format
	   ~tooltip: M.get_format
	   ~icon: (Gui_icons.pixmap M.o_xpm_get_format)#coerce
	   ~callback: self#get_format
	   ()
	);

      self#vbox#pack ~expand: false ~fill: true label_file_info#coerce ;
      self#vbox#pack ~expand: false ~fill: true draw_availability#coerce

  end

class pane_downloads () =
  let wl_status = GMisc.label ~text: "" ~show: true () in
  let locs = new Gui_friends.box_list () in
  let dled = new box_downloaded wl_status () in
  let dls = new box_downloads locs wl_status () in
  object (self)
    inherit Gui_downloads_base.paned ()

    method wl_status = wl_status
    method box_downloads = dls
    method box_downloaded = dled
    method box_locations = locs

    method set_tb_style st =
      locs#set_tb_style st;
      dled#set_tb_style st;
      dls#set_tb_style st

    method clear =
      wl_status#set_text "";
      locs#clear;
      dled#clear;
      dls#clear

    (** {2 Handling core messages} *)

    method h_file_info f = 
      match f.file_state with
        FileNew -> assert false
      | FileCancelled -> 
	  dls#h_cancelled f
      |	FileDownloaded ->
	  dls#h_cancelled f;
	  dled#h_downloaded f
      |	FileShared -> 
          dled#h_removed f
      |	FilePaused -> 
	  dls#h_paused f
      | FileDownloading ->
	  dls#h_downloading f

    method h_file_availability = dls#h_file_availability
    method h_file_downloaded = dls#h_file_downloaded
    method h_file_location = dls#h_file_location

    method h_update_location c_new =
      locs#h_update_location c_new

    method h_remove_client c = dls#remove_client c
      
    method on_entry_return () =
      match entry_ed2k_url#text with
        "" -> ()
      | s ->
          Gui_com.send (GuiProto.Url s);
          entry_ed2k_url#set_text ""

    initializer
      Okey.add entry_ed2k_url
        ~mods: []
        GdkKeysyms._Return
        self#on_entry_return;


      
      hpaned#add2 locs#coerce;
      vpaned#add1 dled#coerce;
      vpaned#add2 dls#coerce ;
  end
