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

(** GUI for graph. *)

open CommonTypes
open GuiProto
open GMain
open GtkBase
open Gtk
open Gdk
open GDraw

module M = Gui_messages
module P = Gpattern
module O = Gui_options

let (!!) = Options.(!!)

let download_timeout = ref 1000
let download_timerID = ref (Timeout.add ~ms:!download_timeout
                              ~callback:(fun _ -> true))
let download_rate = ref 0
let download_counter = ref 0
let download_rate_copy = ref 0
let cumulative_download = ref 0.
let download_cumulative_time = ref 0.
let download_time_range = ref 0
let download_rate_range = ref 0

let upload_timeout = ref 1000
let upload_timerID = ref (Timeout.add ~ms:!upload_timeout
                            ~callback:(fun _ -> true))
let upload_rate = ref 0
let upload_counter = ref 0
let upload_rate_copy = ref 0
let cumulative_upload = ref 0.
let upload_cumulative_time = ref 0.
let upload_time_range = ref 0
let upload_rate_range = ref 0

let width = ref 0
let height = ref 0

let exposed_downloads_pixmap = ref (None : GDraw.pixmap option)
let exposed_uploads_pixmap = ref (None : GDraw.pixmap option)

let downloads_pixmap = ref (None : string array option)
let uploads_pixmap = ref (None : string array option)
let downloads_pixmap_headers = ref (None : string array option)
let uploads_pixmap_headers = ref (None : string array option)

let color_bg_download = ref "#000000"
let color_bg_upload = ref "#000000"
let color_grid_download = ref "#000000"
let color_grid_upload = ref "#000000"

let font = ref (Gdk.Font.load_fontset !!O.font_graphic)

(* the pixmap headers are treated separately to be more convenient.
they will be merged further with the pixmap "body" when we will
call pixmap_from_xpm_d *)
let set_downloads_pixmap_headers w h col1 col2 =
  let header =
    Array.create 1 (Printf.sprintf "%d %d 3 1" w h) in
  let color_none =
    Array.create 1 (Printf.sprintf ". c None") in
  let color1 =
    Array.create 1 ((Printf.sprintf "a c ") ^ col1) in
  let color2 =
    Array.create 1 ((Printf.sprintf "b c ") ^ col2) in
  downloads_pixmap_headers :=
    Some (Array.concat [header;color_none;color1;color2])

let set_uploads_pixmap_headers w h col1 col2 =
  let header =
    Array.create 1 (Printf.sprintf "%d %d 3 1" w h) in
  let color_none =
    Array.create 1 (Printf.sprintf ". c None") in
  let color1 =
    Array.create 1 ((Printf.sprintf "a c ") ^ col1) in
  let color2 =
    Array.create 1 ((Printf.sprintf "b c ") ^ col2) in
  uploads_pixmap_headers :=
    Some (Array.concat [header;color_none;color1;color2])

let string_to_time s =
  let l = String.length s in
  try
    match int_of_string (String.sub s 0 (l - 1)) with
        (a : int) ->
          match (String.sub s (l - 1) 1) with
              "s" ->
                int_of_string (String.sub s 0 (l - 1))
            | "h" ->
                (int_of_string (String.sub s 0 (l - 1)))
                 * 60 * 60
            | "d" ->
                (int_of_string (String.sub s 0 (l - 1)))
                 * 60 * 60 * 24
            | "w" ->
                (int_of_string (String.sub s 0 (l - 1)))
                 * 60 * 60 * 24 * 7
            | _ -> 15 * 60
  with
    | _ -> try
      match int_of_string (String.sub s 0 (l - 2)) with
          (a : int) ->
            match (String.sub s (l - 2) 2) with
                "mn" ->
                  (int_of_string (String.sub s 0 (l - 2))) * 60
              | _ -> 15 * 60
           with
             | _ -> 15 * 60

(* the functions draw_downloads & draw uploads are called to generate the
arrays that will be used to get the pixmaps with pixmap_from_xpm_d *)
let draw_downloads _ =
  let () = match !download_counter with
               0 ->
                 cumulative_download := !cumulative_download
             | _ ->
               cumulative_download :=
                 !cumulative_download +.
                 ((float_of_int (!download_rate) /. 1024.)
                 /. float_of_int (!download_counter)
                 *. (float_of_int (!download_timeout) /. 1000.))
  in
  let downloads_height =
    match !download_counter with
        0 ->
          int_of_float (float_of_int (!height)
          *. (float_of_int (!download_rate_copy) /. 1024.)
          /. (float_of_int (!download_rate_range) *. 1.2))
      | _ ->
        let h = int_of_float (float_of_int (!height)
                *. (float_of_int (!download_rate) /. 1024.)
                /. float_of_int (!download_counter)
                /. (float_of_int (!download_rate_range) *. 1.2))
        in
        download_rate_copy := !download_rate / !download_counter;
        download_rate := 0;
        download_counter := 0;
        h
  in
  let downloads_av_height =
    if !download_cumulative_time = 0.
      then 0
      else int_of_float (float_of_int (!height)
           *. !cumulative_download
           /. (float_of_int (!download_rate_range) *. 1.2)
           /. !download_cumulative_time)
  in
  match !downloads_pixmap with
      None -> ()
    | Some sa ->
        let f i s =
          if (i = (!height - 1 - downloads_av_height)) ||
             (i = (!height - 2 - downloads_av_height))
            then (String.sub s 1 ((String.length s) - 1)) ^ "b"
            else if i < (!height - 1 - downloads_height)
              then (String.sub s 1 ((String.length s) - 1)) ^ "."
              else (String.sub s 1 ((String.length s) - 1)) ^ "a"
        in
        Array.iteri (fun i -> fun s -> sa.(i) <- f i s ) sa


let draw_uploads _ =
  let () = match !upload_counter with
               0 ->
                 cumulative_upload := !cumulative_upload
             | _ ->
               cumulative_upload :=
                 !cumulative_upload +.
                 ((float_of_int (!upload_rate) /. 1024.)
                 /. float_of_int (!upload_counter)
                 *. (float_of_int (!upload_timeout) /. 1000.))
  in
  let uploads_height =
    match !upload_counter with
        0 ->
          int_of_float (float_of_int (!height)
          *. (float_of_int (!upload_rate_copy) /. 1024.)
          /. (float_of_int (!upload_rate_range) *. 1.2))
      | _ ->
        let h = int_of_float (float_of_int (!height)
                *. (float_of_int (!upload_rate) /. 1024.)
                /. float_of_int (!upload_counter)
                /. (float_of_int (!upload_rate_range) *. 1.2))
        in
        upload_rate_copy := !upload_rate / !upload_counter;
        upload_rate := 0;
        upload_counter := 0;
        h
  in
  let uploads_av_height =
    if !upload_cumulative_time = 0.
      then 0
      else int_of_float (float_of_int (!height)
           *. !cumulative_upload
           /. (float_of_int (!upload_rate_range) *. 1.2)
           /. !upload_cumulative_time)
  in
  match !uploads_pixmap with
      None -> ()
    | Some sa ->
        let f i s =
           if i = (!height - 1 - uploads_av_height) ||
              i = (!height - 2 - uploads_av_height)
             then (String.sub s 1 ((String.length s) - 1)) ^ "b"
             else if i < (!height - 1 - uploads_height)
               then (String.sub s 1 ((String.length s) - 1)) ^ "."
               else (String.sub s 1 ((String.length s) - 1)) ^ "a"
        in
        Array.iteri (fun i -> fun s -> sa.(i) <- f i s ) sa

(* draw_downloads_graph & draw_uploads_graph are called to generate the final
pixmaps merging :
1 pixmap for the background
1 pixmap for the grid
1 pixmap for graph
With this structure we can manage all the colors & font without resetting
the whole pixmap which is finally exposed in the graph tab
*)
let draw_downloads_graph _ =
  let grid_pixmap =
    GDraw.pixmap ~width:!width ~height:!height
    ~mask:true ()
  in
  grid_pixmap#set_foreground (`NAME !color_grid_download);
  let x_step = (!width / 50) in
  let y_step = (!height / 50) in
  let x_step =
    if x_step > y_step then
      x_step
      else y_step
  in
  let _ =
    for i = 0 to x_step do
      let a = (i * 50) in
      grid_pixmap#line
        ~x:a ~y:0 ~x:a ~y:!height;
      grid_pixmap#line
        ~x:0 ~y:(!height - a - 1) ~x:!width ~y:(!height - a - 1);
      grid_pixmap#string
        (Date.time_to_string_long (a * !download_timeout / 1000))
         ~font:!font ~x:(a + 2) ~y:(!height - 2);
      if a > 0 then
        grid_pixmap#string
          (Printf.sprintf "%5.1f ko/s"
             (float_of_int (!download_rate_range) *. 1.2
              *. float_of_int (a) /. float_of_int (!height)))
          ~font:!font ~x:2 ~y:(!height - (a + 2))
    done
  in
  let transit_pixmap =
    GDraw.pixmap ~width:!width ~height:!height
    ~mask:false ()
  in
  transit_pixmap#set_foreground
    (`NAME !color_bg_download);
  transit_pixmap#rectangle
    ~filled:true
    ~x:0 ~y:0 
    ~width:!width ~height:!height ();
  let () =
    match (!downloads_pixmap_headers,!downloads_pixmap) with
        (Some sa1, Some sa2) ->
           let pixmap =
             pixmap_from_xpm_d
               ~data:(Array.concat [sa1;sa2])
               ~colormap:(Gdk.Color.get_system_colormap ()) ()
           in
           let () =
             match pixmap#mask with
                 Some m ->
                   transit_pixmap#set_clip_origin ~x:0 ~y:0;
                   transit_pixmap#set_clip_mask m
               | None -> ()
           in
           transit_pixmap#put_pixmap
             ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
             ~width:!width ~height:!height
             pixmap#pixmap
      | _ -> ()
  in
  let _ =
    match grid_pixmap#mask with
        Some m ->
          transit_pixmap#set_clip_origin ~x:0 ~y:0;
          transit_pixmap#set_clip_mask m
      | None -> ()
  in
  transit_pixmap#put_pixmap
    ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
    ~width:!width ~height:!height
    grid_pixmap#pixmap;
  match !exposed_downloads_pixmap with
      Some pixmap ->
        pixmap#put_pixmap
          ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
          ~width:!width ~height:!height
          transit_pixmap#pixmap
    | None ->
       let pixmap =
         GDraw.pixmap ~width:!width ~height:!height
         ~mask:false ()
       in
       pixmap#put_pixmap
         ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
         ~width:!width ~height:!height
         transit_pixmap#pixmap;
       exposed_downloads_pixmap := Some pixmap


let draw_uploads_graph _ =
  let grid_pixmap =
    GDraw.pixmap ~width:!width ~height:!height
    ~mask:true ()
  in
  grid_pixmap#set_foreground
    (`NAME !color_grid_upload);
  let x_step = (!width / 50) in
  let y_step = (!height / 50) in
  let x_step =
    if x_step > y_step then
      x_step
      else y_step
  in
  let _ =
    for i = 0 to x_step do
      let a = (i * 50) in
      grid_pixmap#line
        ~x:a ~y:0 ~x:a ~y:!height;
      grid_pixmap#line
        ~x:0 ~y:(!height - a - 1) ~x:!width ~y:(!height - a - 1);
      grid_pixmap#string
        (Date.time_to_string_long (a * !upload_timeout / 1000))
        ~font:!font ~x:(a + 2) ~y:(!height - 2);
      if a > 0 then
        grid_pixmap#string
          (Printf.sprintf "%5.1f ko/s"
            (float_of_int (!upload_rate_range) *. 1.2
             *. float_of_int (a) /. float_of_int (!height)))
          ~font:!font ~x:2 ~y:(!height - (a + 2))
    done
  in
  let transit_pixmap =
    GDraw.pixmap ~width:!width ~height:!height
    ~mask:false ()
  in
  transit_pixmap#set_foreground
    (`NAME !color_bg_upload);
  transit_pixmap#rectangle
    ~filled:true ~x:0 ~y:0
    ~width:!width ~height:!height ();
  let () =
    match (!uploads_pixmap_headers, !uploads_pixmap) with
       (Some sa1, Some sa2) ->
         let pixmap =
           pixmap_from_xpm_d
             ~data:(Array.concat [sa1; sa2])
             ~colormap:(Gdk.Color.get_system_colormap ()) ()
         in
         let () =
           match pixmap#mask with
               Some m ->
                 transit_pixmap#set_clip_origin ~x:0 ~y:0;
                 transit_pixmap#set_clip_mask m
             | None -> ()
         in
         transit_pixmap#put_pixmap
           ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
           ~width:!width ~height:!height
           pixmap#pixmap
     | _ -> ()
  in
  let _ =
    match grid_pixmap#mask with
        Some m ->
          transit_pixmap#set_clip_origin ~x:0 ~y:0;
          transit_pixmap#set_clip_mask m
      | None -> ()
  in
  transit_pixmap#put_pixmap
    ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
    ~width:!width ~height:!height
    grid_pixmap#pixmap;
  match !exposed_uploads_pixmap with
      Some pixmap ->
        pixmap#put_pixmap
          ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
          ~width:!width ~height:!height
          transit_pixmap#pixmap
    | None ->
        let pixmap =
          GDraw.pixmap ~width:!width ~height:!height
          ~mask:false ()
        in
        pixmap#put_pixmap
          ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
          ~width:!width ~height:!height
          transit_pixmap#pixmap;
        exposed_uploads_pixmap := Some pixmap


let expose_pixmap area pixmap =
  let drawing =
    (area#misc#realize ();
     new GDraw.drawable (area#misc#window))
  in
  match pixmap with
      Some pix ->
        drawing#put_pixmap
          ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
          ~width:!width ~height:!height
          pix#pixmap
    | None -> ()



class box () =
  let hbox_down = GPack.hbox ~border_width:10 ~homogeneous:true () in
  let hbox_up = GPack.hbox ~border_width:10 ~homogeneous:true () in
  let downloads_area = GMisc.drawing_area () in
  let uploads_area = GMisc.drawing_area ()
  in
  object (self)
    inherit Gui_graph_base.box () as box

    method clear =
      download_rate_copy := 0;
      upload_rate_copy := 0

    method set_download_rate r =
      incr (download_counter);
      download_rate := (r  + !download_rate)


    method set_upload_rate r =
      incr (upload_counter);
      upload_rate := (r + !upload_rate)


    method set_graph_properties
      gfont cbd cbu cgd cgu cfd
      cfu cfda cfua dtr mdr utr mur =
      (* Printf.printf "GRAPH PROPERTIES\n%s %s %s %s\n%s %s %s %s\n"
        cbd cbu cgd cgu cfd cfu cfda cfua;
      flush stdout;
      Printf.printf "%s %d - %s %d\n************\n" dtr mdr utr mur;
      flush stdout; *)
      font := Gdk.Font.load_fontset gfont ;
      color_bg_download := cbd;
      color_bg_upload := cbu;
      color_grid_download := cgd;
      color_grid_upload := cgu;
      let drawing = (downloads_area#misc#realize ();
                     new GDraw.drawable (downloads_area#misc#window))
      in
      (* not to mess the drawing areas when the main window is resized
      we fix the width & height of each area once for all. The size will
      be the size when the gui is started *)
      let (w, h) = drawing#size in
      if !width = 0 then width := w ;
      if !height = 0 then height := h ;
      (* Printf.printf "Width : %d Height : %d\n" !width !height;
      flush stdout; *)
      (* to get the possibility to change the colors of the all parts
      of the graph, the pixmaps headers are managed separately (i.e. where
      colors map is defined in the pixmap array ) - Like this it is not
      necessary to reset the graph when such a change is done in the config panel *)
      set_downloads_pixmap_headers !width !height cfd cfda;
      set_uploads_pixmap_headers !width !height cfu cfua;
      let (a, b) = (!download_time_range, !download_rate_range) in
      let (c, d) = ((string_to_time dtr), mdr) in
      (* Printf.printf "Download_time_range : %d Download_rate_range : %d\n%d %d\n"
        a b c d;
      flush stdout; *)
      (* we check if it is necessary to reset the pixmap. Only in 2 cases :
      when the time range is changed (x axis)
      when the max down/upload rate is changed (y axis) *)
      let () = if (a, b) <> (c, d) then begin
          download_time_range := c;
          download_rate_range := d;
          download_timeout := (!download_time_range * 1000 / !width );
          downloads_pixmap :=
            Some (Array.create !height (String.make !width '.')) (*;
          Printf.printf "DOWNLOADS GRAPH PROPERTIES HAVE CHANGED\ndownload_timeout : %d\n"
            !download_timeout;
          flush stdout*)
          end
      in
      let (a, b) = (!upload_time_range, !upload_rate_range) in
      let (c, d) = ((string_to_time utr), mur) in
      (* Printf.printf "Upload_time_range : %d Upload_rate_range : %d\n%d %d\n" a b c d;
      flush stdout;*)
      let () = if (a, b) <> (c, d) then begin
          upload_time_range := c;
          upload_rate_range := d;
          upload_timeout := (!upload_time_range * 1000 / !width );
          uploads_pixmap :=
            Some (Array.create !height (String.make !width '.')) (*;
          Printf.printf "UPLOADS GRAPH PROPERTIES HAVE CHANGED\nupload_timeout : %d\n"
            !upload_timeout;
          flush stdout*)
          end
      in
      (* we check if the pixmaps are existing. If yes we do noting, else
      we create it. In the pixmaps headers '.' is defined as the None color.
      It is necessary because we use this method at the start but also each
      time we use the config panel *)
      let () =
        match !downloads_pixmap with
            None ->
              downloads_pixmap :=
                Some (Array.create !height (String.make !width '.'));
          | _ -> ()
      in
      let () =
        match !uploads_pixmap with
            None ->
              uploads_pixmap :=
                Some (Array.create !height (String.make !width '.'));
          | _ -> ()
      in
      draw_downloads ();
      draw_downloads_graph ();
      draw_uploads ();
      draw_uploads_graph ();
      (* here we need to reset the timers in case the x axis as changed *)
      Timeout.remove (!download_timerID);
      download_timerID :=
        (Timeout.add ~ms:!download_timeout
           ~callback:(fun _ ->
             download_cumulative_time := !download_cumulative_time
               +. ((float_of_int !download_timeout) /. 1000.);
             draw_downloads ();
             draw_downloads_graph ();
             expose_pixmap downloads_area !exposed_downloads_pixmap;
             true));
      Timeout.remove (!upload_timerID);
      upload_timerID :=
        (Timeout.add ~ms:!upload_timeout
           ~callback:(fun _ ->
             upload_cumulative_time := !upload_cumulative_time
               +. ((float_of_int !upload_timeout) /. 1000.);
             draw_uploads ();
             draw_uploads_graph ();
             expose_pixmap uploads_area !exposed_uploads_pixmap;
             true))

    initializer

    hbox_down#add downloads_area#coerce;
    downloads_frame#add hbox_down#coerce;
    hbox_up#add uploads_area#coerce;
    uploads_frame#add hbox_up#coerce;

    ignore (downloads_area#event#connect#expose
      (fun _ ->
                expose_pixmap downloads_area !exposed_downloads_pixmap;
                true));
    ignore (uploads_area#event#connect#expose
      (fun _ ->
                expose_pixmap uploads_area !exposed_uploads_pixmap;
                true))

  end
