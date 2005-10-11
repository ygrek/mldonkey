(* Copyright 2005 beedauchon *)
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

open CommonGlobals
open CommonOptions
open Printf2

(* some thoughts
type gfx_settings = {
    win_x : int;
    win_y : int;
    vtext : string;
 }
*)

(* some defs *)
let reverse lst =
  let rec reverseAux lst acc =
    match lst with
      [] -> acc
    | x::xs -> reverseAux xs (x::acc)
  in reverseAux lst []

let rec sumlist lst =
  match lst with
    [] -> 0
  | x::xs -> if (x > 0) then x + sumlist xs else sumlist xs

let rec maxlist lst = match lst with
  [] -> 0
  | [x] -> x
  | x::xs -> max x (maxlist xs)


(* set some vars *)
let samples_time = 5
let samples_h_time = 720

(* set _x and _y with boundaries *)
let tag_x() = if !!html_mods_vd_gfx_tag_x_size < 130 then 130
  else if !!html_mods_vd_gfx_tag_x_size > 3600 then 3600
  else !!html_mods_vd_gfx_tag_x_size
let tag_y() = if !!html_mods_vd_gfx_tag_y_size < 50 then 50
  else if !!html_mods_vd_gfx_tag_y_size > 1200 then 1200
  else !!html_mods_vd_gfx_tag_y_size
let win_x() = if !!html_mods_vd_gfx_x_size < 320 then 320
  else if !!html_mods_vd_gfx_x_size > 3600 then 3600
  else !!html_mods_vd_gfx_x_size
let win_y() = if !!html_mods_vd_gfx_y_size < 200 then 200
  else if !!html_mods_vd_gfx_y_size > 1200 then 1200
  else !!html_mods_vd_gfx_y_size

let vtext = "0"
let x_divisions() = (win_x()) / 80
let y_divisions() = (win_y()) / 30
let x_fdivisions() = float_of_int (x_divisions())
let y_fdivisions() = float_of_int (y_divisions())

(* todo: limit to 4 max *)
let xmult = 1
let xgmult = 16
(* todo: limit to 4 max *)
let ymult = 1
let xdivisions() = (x_divisions()) * xmult
let fxdivisions() = float_of_int (xdivisions())
let xgdivisions() = (x_divisions()) * xgmult
let fxgdivisions() = float_of_int (xgdivisions())
let ydivisions() = (y_divisions()) * ymult
let fydivisions() = float_of_int (ydivisions())
let vmax_auto() = if detected_downlink_capacity () < detected_uplink_capacity () then
    (detected_uplink_capacity ())
  else
    (detected_downlink_capacity ())

let vmax link = (detected_link_capacity link)

let vx() = (fxgdivisions()) /. (x_fdivisions())
let vy() = (((float_of_int(vmax_auto())) /. 1024.) +. 2.) /. (y_fdivisions())
let vy_m link = (((float_of_int(vmax link)) /. 1024.) +. 2.) /. (y_fdivisions())

(* define margins *)
let left_margin = 20
let right_margin = 45
let top_margin = 16
let bottom_margin = 20

let samples_size = win_x() - (left_margin + right_margin)

let xbl = left_margin
let xbr() = win_x() - right_margin
let xbs() = xbr() - xbl
let ybt = top_margin
let ybb() = win_y() - bottom_margin
let ybs() = ybb() - ybt
let vdt() = float_of_int(ybs()) /. float_of_int(vmax_auto())
let vdt_m link = float_of_int(ybs()) /. float_of_int(vmax link)
let vdt_stack link = float_of_int(ybs() / 2) /. float_of_int(vmax link)

let ttl = ref ""
let vl = ref ""
let hl = ref ""

let graph_length g =
  if (Fifo.length g) < (xgdivisions()) then
    ((Fifo.length g) - 1)
  else
    (xgdivisions())

let datas_length g =
  ((Fifo.length g))

let l_max g =
  (List.fold_left max 0 (Fifo.to_list g))

let datas = Array.create history_size 0


let draw_borders mypic gcolor =
  let my_y = (ybb()) in
  let fx x = int_of_float ((((float_of_int x) /. (fxdivisions())) *. (float_of_int(xbs()))) +. (float_of_int xbl)) in
  let my_x = (xbr()) in
  let fy x = int_of_float ((((float_of_int x) /. (fydivisions())) *. (float_of_int(ybs()))) +. (float_of_int ybt)) in
  mypic#line ~x1:(fx 0) ~y1:ybt ~x2:(fx 0) ~y2:my_y gcolor;
  mypic#line ~x1:(fx (xdivisions())) ~y1:ybt ~x2:(fx (xdivisions())) ~y2:my_y gcolor;
  mypic#line ~x1:xbl ~y1:(fy 0) ~x2:my_x ~y2:(fy 0) gcolor;
  mypic#line ~x1:xbl ~y1:(fy (ydivisions())) ~x2:my_x ~y2:(fy (ydivisions())) gcolor

let draw_stack_borders mypic gcolor =
  let my_y = (ybb()) in
  let fx x = int_of_float ((((float_of_int x) /. (fxdivisions())) *. (float_of_int(xbs()))) +. (float_of_int xbl)) in
  let my_x = (xbr()) in
  let fy x = int_of_float ((((float_of_int x) /. (fydivisions())) *. (float_of_int(ybs()))) +. (float_of_int ybt)) in
  mypic#line ~x1:(fx 0) ~y1:ybt ~x2:(fx 0) ~y2:my_y gcolor;
  mypic#line ~x1:(fx (xdivisions())) ~y1:ybt ~x2:(fx (xdivisions())) ~y2:my_y gcolor;
  mypic#line ~x1:xbl ~y1:(fy 0) ~x2:my_x ~y2:(fy 0) gcolor;
  mypic#line ~x1:xbl ~y1:(fy (ydivisions())) ~x2:my_x ~y2:(fy (ydivisions())) gcolor;
  mypic#line ~x1:xbl ~y1:(my_y - ((my_y - ybt) / 2)) ~x2:my_x ~y2:(my_y - ((my_y - ybt) / 2)) gcolor

let draw_x_grid mypic gcolor =
  let my_y = (ybb()) in
  let fx x = int_of_float ((((float_of_int x) /. (fxdivisions())) *. (float_of_int(xbs()))) +. (float_of_int xbl)) in
    for n = 1 to xdivisions() - 1 do
      mypic#dashed_line ~x1:(fx n) ~y1:ybt ~x2:(fx n) ~y2:my_y gcolor;
    done

let draw_y_grid mypic gcolor =
  let my_x = (xbr()) in
  let fy x = int_of_float ((((float_of_int x) /. (fydivisions())) *. (float_of_int(ybs()))) +. (float_of_int ybt)) in
    for n = 1 to ydivisions() - 1 do
      mypic#dashed_line ~x1:xbl ~y1:(fy n) ~x2:my_x ~y2:(fy n) gcolor;
    done

let draw_arrow mypic gcolor =
  let my_x = (xbr()) in
  let my_y = (ybb()) in
  mypic#line ~x1:(my_x - 4) ~y1:(my_y + 4) ~x2:(my_x + 4) ~y2:(my_y) gcolor;
  mypic#line ~x1:(my_x - 4) ~y1:(my_y - 4) ~x2:(my_x + 4) ~y2:(my_y) gcolor;
  mypic#line ~x1:(my_x - 4) ~y1:(my_y - 4) ~x2:(my_x - 4) ~y2:(my_y + 4) gcolor;
  mypic#fill ~x:(my_x - 1) ~y:(my_y - 1) gcolor;
  mypic#fill ~x:(my_x - 1) ~y:(my_y + 1) gcolor;
  mypic#fill ~x:(my_x + 1) ~y:(my_y) gcolor

let draw_tag mypic title gdown gup gcolor  =
  let my_sum gl = List.fold_left (+) 0 (Fifo.to_list gl) in
  let meanx gl = ((float_of_int (my_sum gl)) /. (float_of_int ((Fifo.length gl) - 1))) in
  let down_bw = (string_of_float (float_of_int(int_of_float((meanx gdown) /. 1024. *. 100.)) /. 100.)) in
  let up_bw = (string_of_float (float_of_int(int_of_float((meanx gup) /. 1024. *. 100.)) /. 100.)) in
  let bw_d = "Dl: " ^ down_bw ^ "KB/s "
  and bw_u = "Ul: " ^ up_bw ^ "KB/s" in
  if !!html_mods_vd_gfx_tag_enable_title then
  mypic#string ~font:Gd.Font.giant ~x:!!html_mods_vd_gfx_tag_title_x_pos ~y:!!html_mods_vd_gfx_tag_title_y_pos   ~s:title gcolor;
  mypic#string ~font:Gd.Font.giant ~x:!!html_mods_vd_gfx_tag_dl_x_pos ~y:!!html_mods_vd_gfx_tag_dl_y_pos  ~s:bw_d gcolor;
  mypic#string ~font:Gd.Font.giant ~x:!!html_mods_vd_gfx_tag_ul_x_pos ~y:!!html_mods_vd_gfx_tag_ul_y_pos  ~s:bw_u gcolor

let draw_title mypic title gcolor win_y =
  mypic#string_up ~font:Gd.Font.giant ~x:2 ~y:((win_y / 2)  + (((String.length title) * 8) / 2)) ~s:title gcolor

let draw_top_legend mypic title tcolor gcolor scolor win_y =
  mypic#line ~x1:(xbl + 1) ~y1:9 ~x2:(xbl + 11) ~y2:9 scolor;
  mypic#line ~x1:xbl ~y1:8 ~x2:(xbl + 10) ~y2:8 gcolor;
  mypic#string ~font:Gd.Font.small ~x:(xbl + 16) ~y:2 ~s:title tcolor

let draw_dual_top_legend mypic titlel tcolorl gcolorl scolorl titler tcolorr gcolorr scolorr win_y =
  let my_x = (xbr()) in
  mypic#line ~x1:(xbl + 1) ~y1:9 ~x2:(xbl + 11) ~y2:9 scolorl;
  mypic#line ~x1:xbl ~y1:8 ~x2:(xbl + 10) ~y2:8 gcolorl;
  mypic#string ~font:Gd.Font.small ~x:(xbl + 16) ~y:2 ~s:titlel tcolorl;
  mypic#line ~x1:(my_x - 1) ~y1:9 ~x2:(my_x - 11) ~y2:9 scolorr;
  mypic#line ~x1:my_x ~y1:8 ~x2:(my_x - 10) ~y2:8 gcolorr;
  mypic#string ~font:Gd.Font.small ~x:(my_x - (((String.length titler) * 8)) - 2) ~y:2 ~s:titler tcolorr

let draw_v_legend mypic g legend_text lcolor gcolor offset =
  let my_x = (xbr()) in
  let my_y = (ybb()) in
  let fy x = int_of_float ((float_of_int my_y) -. (((float_of_int x) /. (y_fdivisions())) *. (float_of_int(ybs())))) in
  let vtext n = (string_of_float (float_of_int(int_of_float((float_of_int (my_y - (fy n))) /. (vdt_m g) /. 1024. *. 100.)) /. 100.)) in
    for n = 1 to ((y_divisions()) - 1) do
      mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy n) - 12 + offset) ~s:(vtext n) gcolor;
    done;
    mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy (y_divisions())) - 5) ~s:(legend_text) lcolor

let draw_stack_v_top_legend mypic g legend_text lcolor gcolor offset =
  let my_x = (xbr()) in
  let my_y = (ybb()  - (ybs() / 2)) in
  let fy x = int_of_float ((float_of_int my_y) -. (((float_of_int x) /. (y_fdivisions())) *. (float_of_int(ybs())))) in
  let vtext n = (string_of_float (float_of_int(int_of_float((float_of_int (my_y - (fy n))) /. (vdt_stack g) /. 1024. *. 100.)) /. 100.)) in
    for n = 1 to ((y_divisions()) - 1) do
      mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy n) - 12 + offset) ~s:(vtext n) gcolor;
    done;
    mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy (y_divisions())) - 5) ~s:(legend_text) lcolor

let draw_stack_v_bottom_legend mypic g legend_text lcolor gcolor offset =
  let my_x = (xbr()) in
  let my_y = (ybb()  - (ybs() / 2)) in
  let fy x = int_of_float ((float_of_int my_y) +. (((float_of_int x) /. (y_fdivisions())) *. (float_of_int(ybs())))) in
  let vtext n = (string_of_float (float_of_int(int_of_float((float_of_int ((fy n) - my_y)) /. (vdt_stack g) /. 1024. *. 100.)) /. 100.)) in
    for n = 1 to ((y_divisions()) - 1) do
      mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy n) - 12 + offset) ~s:(vtext n) gcolor;
    done;
    mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy (y_divisions())) - 5) ~s:(legend_text) lcolor

let draw_h_legend mypic g legend_text gcolor my_time =
  let my_x = (xbr()) in
  let my_x2 = (xbs()) in
  let my_y = (ybb()) in
  let datas g n = List.nth (List.rev (Fifo.to_list g)) n in
  let fx x = int_of_float((float_of_int my_x) -. (((float_of_int x) /. (x_fdivisions())) *. (float_of_int my_x2))) in
  (* and vtext n = (string_of_int (int_of_float((float_of_int(n))*. vx ))) in *)
  let basetime = Unix.gettimeofday () in
  let timer n = Unix.localtime (basetime -. ((float_of_int(n)) *. float_of_int(my_time) *. vx())) in
  let time_string n =
    let time = timer n in
    let h0 = string_of_int(time.Unix.tm_hour ) and    (* H *)
    m0 = string_of_int(time.Unix.tm_min ) and         (* M *)
    s0 = string_of_int(time.Unix.tm_sec ) in          (* S *)
    (if String.length h0 = 2 then h0 else "0"^h0) ^":"^
    (if String.length m0 = 2 then m0 else "0"^m0) ^":"^
    (if String.length s0 = 2 then s0 else "0"^s0) in

    for n = 1 to ((x_divisions()) - 1) do
      mypic#string ~font:Gd.Font.small ~x:((fx n) - ((String.length (time_string n) * 5) / 2)) ~y:(my_y + 5) ~s:(time_string n) gcolor;
    done;
    mypic#string ~font:Gd.Font.small ~x:((fx (x_divisions())) - ((String.length (legend_text) * 4) / 2)) ~y:(my_y + 5) ~s:(legend_text) gcolor

let draw_load mypic g my_color shadow_color =
  let my_x = (xbr()) in
  let my_x2 = (xbs()) in
  let my_y = (ybb()) in
  let my_y2 = (ybs()) in
  let datas g n = List.nth (List.rev (Fifo.to_list g)) n in
  let fx x = int_of_float((float_of_int my_x) -. (((float_of_int x) /. (fxgdivisions())) *. (float_of_int my_x2)))
  and y_c1 n = (my_y - (int_of_float(float_of_int(datas g n) *. (vdt_m g))))
  and y_c2 n = (my_y - (int_of_float(float_of_int(datas g (n+1)) *. (vdt_m g)))) in
  (if !!html_mods_vd_gfx_fill then begin
        mypic#line ~x1:(fx 0) ~y1:my_y
         ~x2:(fx 0) ~y2:(if y_c1 0 >= my_y - 3 then
         ((y_c1 0) - 3) else ((y_c1 0))) shadow_color;
    for n = 0 to ((graph_length(g)) - 1) do
  (* trick to make sure filling will not fail *)
      if n = ((graph_length(g)) - 1) then
        mypic#line ~x1:((fx n)) ~y1:(if y_c1 n >= my_y - 3 then
         ((y_c1 n) - 3) else ((y_c1 n))) ~x2:((fx (n+1))) ~y2:((y_c2 n)) shadow_color
      else begin
        mypic#line ~x1:((fx n)) ~y1:(if y_c1 n >= my_y - 3 then
         ((y_c1 n) - 3) else ((y_c1 n)))
         ~x2:((fx (n+1))) ~y2:(if y_c2 n >= my_y - 3 then
         ((y_c2 n) - 3) else ((y_c2 n))) shadow_color
      end
    done;
  mypic#fill ~x:(my_x - 1) ~y:(my_y - 1) my_color;
  end
  else begin
    for n = 0 to ((graph_length(g)) - 1) do
      mypic#line ~x1:((fx n) + 1) ~y1:((y_c1 n) + 1) ~x2:((fx (n+1)) + 1) ~y2:((y_c2 n) + 1) shadow_color;
      mypic#line ~x1:(fx n) ~y1:(y_c1 n) ~x2:(fx (n+1)) ~y2:(y_c2 n) my_color
    done;
  end
  )

let draw_stack_download mypic g my_color shadow_color =
  let my_x = (xbr()) in
  let my_x2 = (xbs()) in
  let my_y = (ybb() - (ybs() / 2)) in
  let my_y2 = (ybs() / 2) in
  let datas g n = List.nth (List.rev (Fifo.to_list g)) n in
  let fx x = int_of_float((float_of_int my_x) -. (((float_of_int x) /. (fxgdivisions())) *. (float_of_int my_x2)))
  and y_c1 n = (my_y - (int_of_float(float_of_int(datas g n) *. (vdt_stack g))))
  and y_c2 n = (my_y - (int_of_float(float_of_int(datas g (n+1)) *. (vdt_stack g)))) in
  (if !!html_mods_vd_gfx_fill then begin
        mypic#line ~x1:(fx 0) ~y1:my_y
         ~x2:(fx 0) ~y2:(if y_c1 0 >= my_y - 3 then
         ((y_c1 0) - 3) else ((y_c1 0))) shadow_color;
    for n = 0 to ((graph_length(g)) - 1) do
  (* trick to make sure filling will not fail *)
      if n = ((graph_length(g)) - 1) then
        mypic#line ~x1:((fx n)) ~y1:(if y_c1 n >= my_y - 2 then
         ((y_c1 n) - 2) else ((y_c1 n))) ~x2:((fx (n+1))) ~y2:((y_c2 n)) shadow_color
      else begin
        mypic#line ~x1:((fx n)) ~y1:(if y_c1 n >= my_y - 2 then
         ((y_c1 n) - 2) else ((y_c1 n)))
         ~x2:((fx (n+1))) ~y2:(if y_c2 n >= my_y - 2 then
         ((y_c2 n) - 2) else ((y_c2 n))) shadow_color
      end
    done;
  mypic#fill ~x:(my_x - 1) ~y:(my_y - 1) my_color;
  end
  else begin
    for n = 0 to ((graph_length(g)) - 1) do
      mypic#line ~x1:((fx n) + 1) ~y1:((y_c1 n) + 1) ~x2:((fx (n+1)) + 1) ~y2:((y_c2 n) + 1) shadow_color;
      mypic#line ~x1:(fx n) ~y1:(y_c1 n) ~x2:(fx (n+1)) ~y2:(y_c2 n) my_color
    done;
  end
  )

let draw_stack_upload mypic g my_color shadow_color =
  let my_x = (xbr()) in
  let my_x2 = (xbs()) in
  let my_y = (ybb() - (ybs() / 2)) in
  let my_y2 = (ybs() / 2) in
  let datas g n = List.nth (List.rev (Fifo.to_list g)) n in
  let fx x = int_of_float((float_of_int my_x) -. (((float_of_int x) /. (fxgdivisions())) *. (float_of_int my_x2)))
  and y_c1 n = (my_y + (int_of_float(float_of_int(datas g n) *. (vdt_stack g))))
  and y_c2 n = (my_y + (int_of_float(float_of_int(datas g (n+1)) *. (vdt_stack g)))) in
  (if !!html_mods_vd_gfx_fill then begin
        mypic#line ~x1:(fx 0) ~y1:my_y
         ~x2:(fx 0) ~y2:(if y_c1 0 >= my_y - 3 then
         ((y_c1 0) - 3) else ((y_c1 0))) shadow_color;
    for n = 0 to ((graph_length(g)) - 1) do
  (* trick to make sure filling will not fail *)
      if n = ((graph_length(g)) - 1) then
        mypic#line ~x1:((fx n)) ~y1:(if y_c1 n >= my_y + 2 then
         ((y_c1 n) + 2) else ((y_c1 n))) ~x2:((fx (n+1))) ~y2:((y_c2 n)) shadow_color
      else begin
        mypic#line ~x1:((fx n)) ~y1:(if y_c1 n >= my_y + 2 then
         ((y_c1 n) + 2) else ((y_c1 n)))
         ~x2:((fx (n+1))) ~y2:(if y_c2 n >= my_y + 2 then
         ((y_c2 n) + 2) else ((y_c2 n))) shadow_color
      end
    done;
  mypic#fill ~x:(my_x - 1) ~y:(my_y + 1) my_color;
  end
  else begin
    for n = 0 to ((graph_length(g)) - 1) do
      mypic#line ~x1:((fx n) + 1) ~y1:((y_c1 n) + 1) ~x2:((fx (n+1)) + 1) ~y2:((y_c2 n) + 1) shadow_color;
      mypic#line ~x1:(fx n) ~y1:(y_c1 n) ~x2:(fx (n+1)) ~y2:(y_c2 n) my_color
    done;
  end
  )


let draw_mean_line mypic g my_color gcolor =
  let my_x = (xbr())
  and my_x2 = (xbs())
  and my_y = (ybb())
  and my_y2 = (ybs()) in
  let my_sum gl = List.fold_left (+) 0 (Fifo.to_list gl) in
  let meanx() = ((float_of_int (my_sum g)) /. (float_of_int ((Fifo.length g)))) in
  let ypos = (my_y - (int_of_float((meanx()) *. (vdt_m g)))) in
  let vtext = (string_of_float (float_of_int(int_of_float(meanx() /. 1024. *. 100.)) /. 100.)) in
  mypic#line ~x1:xbl ~y1:(ypos - 1) ~x2:(xbl + my_x2 / 10) ~y2:(ypos - 1) my_color;
  mypic#line ~x1:xbl ~y1:(ypos) ~x2:(xbl + my_x2 / 10) ~y2:(ypos) gcolor;
  mypic#string ~font:Gd.Font.small ~x:(xbl + 5) ~y:((ypos) + 2) ~s:(vtext) gcolor

let draw_mygraph mypic ttl top_title vl hl g =
  (* init pic *)
(
  let g_y = win_y() in
  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black red darkgrey g_y;
  draw_v_legend mypic g vl black black 0;
  draw_h_legend mypic g hl black samples_time;
  draw_load mypic g green darkgrey;
  draw_x_grid mypic black;
  draw_y_grid mypic black;
 (* draw_mean_line mypic gdown green black; *)
)

(* end of declarations *)

(* main *)

(* useless

let do_chan_pic ttl vl hl gdown gup =

  (* init pic *)

  let mypic = Gd.create ~x:win_x() ~y:win_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black win_y;
  draw_v_legend mypic vl black black 0;
  draw_h_legend mypic gdown hl black samples_time;
  draw_load mypic gdown green darkgrey;
  draw_load mypic gup red darkgrey;
  draw_x_grid mypic black;
  draw_y_grid mypic black;

(*  draw_mean_line mypic gdown green black;*)
 (* mypic#out_as_jpeg "line.jpg" ~quality:90; *)

*)

(* do_draw_pic "Traffic" "(kb)" "(t)" download_history upload_history *)

let do_draw_pic ttl vl hl gdown gup =
  (* init pic *)
(
  let g_x = win_x() in
  let g_y = win_y() in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_title mypic ttl black g_y;
  draw_dual_top_legend mypic "download" black green darkgrey "upload" black red darkgrey g_y;
  draw_h_legend mypic gdown hl black samples_time;
  (if !!html_mods_vd_gfx_stack then begin
    draw_stack_borders mypic black;
    draw_stack_v_top_legend mypic gdown vl black darkgreen 5;
    draw_stack_v_bottom_legend mypic gup vl black darkred 5;
    (* enable filling for stack graph *)
    if not !!html_mods_vd_gfx_fill then html_mods_vd_gfx_fill =:= true;
    draw_stack_download mypic gdown green darkgrey;
    draw_stack_upload mypic gup red darkgrey;
    draw_x_grid mypic black;
    draw_y_grid mypic black
   end
  else begin
    draw_borders mypic black;
    draw_v_legend mypic gdown vl black darkgreen 0;
    draw_v_legend mypic gup vl black darkred 10;
    if !!html_mods_vd_gfx_fill then html_mods_vd_gfx_fill =:= false;
    draw_load mypic gdown green darkgrey;
    draw_load mypic gup red darkgrey;
    draw_x_grid mypic black;
    draw_y_grid mypic black;
    (if !!html_mods_vd_gfx_mean then
      draw_mean_line mypic gdown green black;
      draw_mean_line mypic gup red black
    );
  end);
  draw_arrow mypic darkred;
  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_updown.png"
  else
    mypic#save_as_jpeg "bw_updown.jpg" ~quality:90
  );
)

let do_draw_down_pic ttl top_title vl hl gdown =
  (* init pic *)
(
  let g_x = win_x() in
  let g_y = win_y() in
  let mypic = Gd.create ~x:g_x ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black green darkgrey g_y;
  draw_v_legend mypic gdown vl black black 6;
  draw_h_legend mypic gdown hl black samples_time;
  draw_load mypic gdown green darkgrey;
  draw_x_grid mypic black;
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gdown green black;

  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_download.png"
  else
    mypic#save_as_jpeg "bw_download.jpg" ~quality:90
  );
)

let do_draw_up_pic ttl top_title vl hl gup =
  (* init pic *)
(
  let g_x = win_x() in
  let g_y = win_y() in
  let mypic = Gd.create ~x:g_x ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black red darkgrey g_y;
  draw_v_legend mypic gup vl black black 6;
  draw_h_legend mypic gup hl black samples_time;
  draw_load mypic gup red darkgrey;
  draw_x_grid mypic black;
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gup red black;

  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_upload.png"
  else
    mypic#save_as_jpeg "bw_upload.jpg" ~quality:90
  );
)

let do_draw_h_pic ttl vl hl gdown gup =
  (* init pic *)
(
  let g_x = win_x() in
  let g_y = win_y() in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_dual_top_legend mypic "download" black green darkgrey "upload" black red darkgrey g_y;
  draw_v_legend mypic gdown vl black darkgreen 0;
  draw_v_legend mypic gup vl black darkred 10;
  draw_h_legend mypic gdown hl black samples_h_time;
  if !!html_mods_vd_gfx_fill then html_mods_vd_gfx_fill =:= false;
  draw_load mypic gdown green darkgrey;
  draw_load mypic gup red darkgrey;
  draw_x_grid mypic black;
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  (if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gdown green black;
    draw_mean_line mypic gup red black
  );
  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_h_updown.png"
  else
    mypic#save_as_jpeg "bw_h_updown.jpg" ~quality:90
  );
)

let do_draw_down_h_pic ttl top_title vl hl gdown =
  (* init pic *)
(
  let g_x = win_x() in
  let g_y = win_y() in
  let mypic = Gd.create ~x:g_x ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black green darkgrey g_y;
  draw_v_legend mypic gdown vl black black 6;
  draw_h_legend mypic gdown hl black samples_h_time;
  draw_load mypic gdown green darkgrey;
  draw_x_grid mypic black;
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gdown green black;

  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_h_download.png"
  else
    mypic#save_as_jpeg "bw_h_download.jpg" ~quality:90
  );
)

let do_draw_up_h_pic ttl top_title vl hl gup =
  (* init pic *)
(
  let g_x = win_x() in
  let g_y = win_y() in
  let mypic = Gd.create ~x:g_x ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black red darkgrey g_y;
  draw_v_legend mypic gup vl black black 6;
  draw_h_legend mypic gup hl black samples_h_time;
  draw_load mypic gup red darkgrey;
  draw_x_grid mypic black;
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gup red black;

  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_h_upload.png"
  else
    mypic#save_as_jpeg "bw_h_upload.jpg" ~quality:90
  );
)

let do_draw_tag title gdown gup =
  (* init pic *)
(
  let g_x = tag_x() in
  let g_y = tag_y() in
  let mypic = if !!html_mods_vd_gfx_tag_use_source then
    (if !!html_mods_vd_gfx_tag_png then
      Gd.open_png (!!html_mods_vd_gfx_tag_source ^ ".png")
    else
      Gd.open_jpeg (!!html_mods_vd_gfx_tag_source ^ ".jpg")
    )
  else
  Gd.create ~x:g_x ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in
  (* draw graph *)
  draw_tag mypic title gdown gup black ;
  (if !!html_mods_vd_gfx_tag_png then
    mypic#save_as_png "tag.png"
  else
    mypic#save_as_jpeg "tag.jpg" ~quality:90
  );
)

let really_remove_files () =
  let files =
    ["bw_updown"; "bw_download"; "bw_upload"; "bw_h_updown"; "bw_h_download"; "bw_h_upload"; "tag"]
  in
  let f file =
    (try Sys.remove (file ^ ".png") with _ -> ());
    (try Sys.remove (file ^ ".jpg") with _ -> ());
  in
  List.iter f files

let remove_files () =
  if !!html_mods_vd_gfx_remove then really_remove_files ()

let png_version_num () =
  begin
    try
      let s = Int32.to_string(Gd.png_version ()) in
        let len = String.length s in
        let major_version = String.sub s 0 1 in
	let minor_version = String.sub s 1 2 in
	let release_version = String.sub s 3 (len-3) in
	  Printf.sprintf "%ld.%ld.%ld"
	    (Int32.of_string(major_version))
	    (Int32.of_string(minor_version))
	    (Int32.of_string(release_version))
    with e -> ""
  end
