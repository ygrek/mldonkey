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
open Gd
open CommonGlobals
open CommonOptions
open Printf2

module type Graphics =
sig
  val do_draw_pic :
    string -> string -> string -> int Fifo.t -> int Fifo.t -> unit
  val do_draw_down_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_up_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_h_pic :
    string -> string -> string -> int Fifo.t -> int Fifo.t -> unit
  val do_draw_down_h_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_up_h_pic :
    string -> string -> string -> string -> int Fifo.t -> unit
  val do_draw_tag : string -> int Fifo.t -> int Fifo.t -> unit
  val really_remove_files : unit -> unit
  val remove_files : unit -> unit
  val png_version_num : unit -> string
end


module Graphics : Graphics = struct

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

let history_time = history_size * history_step
let history_h_time() = history_h_size * !history_h_step

(* define margins *)
let left_margin = 20
let right_margin = 45
let top_margin = 16
let bottom_margin = 24

(* set _x and _y with boundaries *)
let tag_x () = min (max 130 !!html_mods_vd_gfx_tag_x_size) 3600
let tag_y () = min (max 50 !!html_mods_vd_gfx_tag_y_size) 1200
let win_x () = min (max 365 !!html_mods_vd_gfx_x_size) 3665
let win_y () = min (max 200 !!html_mods_vd_gfx_y_size) 1200

let round_down x = 
  let values = [120; 60; 30; 15; 12; 10; 6; 4; 3; 2; 1; 0] in
  max 1 (List.find ((>=) x) values)

let round_up x = 
  let v = min x 120 in
  let values = [0; 1; 2; 3; 4; 5; 10; 15; 30; 60; 120] in
  List.find ((<=) v) values

let round_h_down x = 
  let values = [90720; 80640; 70560; 60480; 50400; 40320; 30240; 20160; 18720; 17280; 15840; 14400; 12960; 11520; 10080; 8640; 7200; 5760; 4320; 2880; 1440; 720; 480; 360; 300; 240; 180; 120; 60; 30; 15; 10; 5; 4; 3; 2; 1; 0] in
  max 1 (List.find ((>=) x) values)
        
let round_h_up x =
  let v = min x 90720 in
  let values = [1; 2; 3; 4; 5; 10; 15; 30; 60; 120; 180; 240; 300; 360; 480; 720; 1440; 2880; 4320; 5760; 7200; 8640; 10080; 11520; 12960; 14400; 15840; 17280; 18720; 20160; 30240; 40320; 50400; 60480; 70560; 80640; 90720] in
  List.find ((<=) v) values
        
(* calculate x-values *)
let x_time_per_grid() = 60 * round_up (history_time / (60 * ((win_x () - left_margin - right_margin) / 60)))
let x_divisions () = history_time / (x_time_per_grid ())
let x_fdivisions () = float_of_int (x_divisions ())

let x_h_time_per_grid g = max !history_h_step (
  if !!html_mods_vd_gfx_h_dynamic then 
    begin
      if !!html_mods_vd_gfx_h_grid_time >= 1 then
        min (round_h_down (!!html_mods_vd_gfx_h_grid_time * 60)) 
                  (min ((round_h_up ((((Fifo.length g) + x_divisions () - 2) / (x_divisions())) * !history_h_step/60 )) * 60)
                       ((round_h_down ( history_h_time() / (60 * x_divisions()) )) * 60))
      else
              min ((round_h_up ((((Fifo.length g) + x_divisions () - 2) / (x_divisions ())) * !history_h_step / 60)) * 60)
            ((round_h_down (history_h_time() / (60 * x_divisions ()) )) * 60)
    end
  else
    if !!html_mods_vd_gfx_h_grid_time >= 1 then
      round_h_down (!!html_mods_vd_gfx_h_grid_time * 60)
  else
          round_h_down (history_h_time() / (60 * x_divisions ())))

let vmax link = (detected_link_capacity link)

let x_h_time g = (x_divisions() * x_h_time_per_grid g)
let x_h_values g = ((x_h_time g) / !history_h_step)

(* calculate y-values *)
let y_divisions () = ((win_y() - top_margin - bottom_margin)) / 50 * 2
let y_fdivisions () = float_of_int (y_divisions())

let vmax_auto () = max (detected_uplink_capacity()) (detected_downlink_capacity())

let samples_size = win_x() - (left_margin + right_margin)

let xbl = left_margin
let xbr() = win_x() - right_margin
let xbs() = xbr() - xbl
let ybt = (top_margin / 2) * 2
let ybb() = ((win_y () - bottom_margin) / 2) * 2 - 1
let ybs() = ybb() - ybt
let vdt() = float_of_int(ybs()) /. float_of_int(vmax_auto())
let vdt_m link = float_of_int(ybs()) /. float_of_int(vmax link)
let vdt_stack link = float_of_int((ybs() / 2)-1) /. float_of_int(vmax link)

let ttl = ref ""
let vl = ref ""
let hl = ref ""

let l_max g = (List.fold_left max 0 (Fifo.to_list g))

let draw_borders mypic gcolor =
  mypic#line ~x1:(xbl-1)   ~y1:(ybt-1) ~x2:(xbl-1)   ~y2:(ybb()+1) gcolor;
  mypic#line ~x1:(xbr()+1) ~y1:(ybt-1) ~x2:(xbr()+1) ~y2:(ybb()+1) gcolor;
  mypic#line ~x1:(xbl-1)   ~y1:(ybt-1) ~x2:(xbr()+1) ~y2:(ybt-1) gcolor;
  mypic#line ~x1:(xbl-1)   ~y1:(ybb()+1) ~x2:(xbr()+1) ~y2:(ybb()+1) gcolor

let draw_stack_borders mypic gcolor =
  draw_borders mypic gcolor;
  mypic#line ~x1:(xbl-1)   ~y1:(ybt+ybs()/2)	~x2:(xbr()+1) ~y2:(ybt+ybs()/2) gcolor

let draw_x_grid mypic gcolor gcolor2 my_xdivisons =
  let my_sdivisions = max 1 !!html_mods_vd_gfx_subgrid in
  let my_xsdivisons = xbs() / my_xdivisons / my_sdivisions in
  let my_y = (ybb()) in
  let fx x = (((x * xbs()) / my_xdivisons) + xbl + 2) in
  let fxs x y = (fx x + my_xsdivisons * y) in
  for n = 1 to my_xdivisons - 1 do
    (*mypic#string ~font:Gd.Font.small ~x:(fx n) ~y:(2) ~s:(string_of_int (fx n)) gcolor;*)
    mypic#dashed_line ~x1:(fx n) ~y1:(ybt) ~x2:(fx n) ~y2:my_y gcolor;
  done;
  if my_sdivisions > 1 then
    for n = 0 to my_xdivisons - 1 do
      for m = 1 to my_sdivisions - 1 do
        mypic#dashed_line ~x1:(fxs n m) ~y1:(ybt) ~x2:(fxs n m) ~y2:my_y gcolor2;
      done;
    done

let draw_y_grid mypic gcolor =
  let fy x = (((x * ybs()) / y_divisions()) + ybt) in
  for n = 1 to  y_divisions() - 1 do
    mypic#dashed_line ~x1:xbl ~y1:(fy n) ~x2:(xbr()) ~y2:(fy n) gcolor;
  done

let draw_arrow mypic gcolor =
  let my_x = (xbr()+1) in
  let my_y = (ybb()+1) in
  mypic#line ~x1:(my_x - 4) ~y1:(my_y + 4) ~x2:(my_x + 4) ~y2:(my_y) gcolor;
  mypic#line ~x1:(my_x - 4) ~y1:(my_y - 4) ~x2:(my_x + 4) ~y2:(my_y) gcolor;
  mypic#line ~x1:(my_x - 4) ~y1:(my_y - 4) ~x2:(my_x - 4) ~y2:(my_y + 4) gcolor;
  mypic#fill ~x:(my_x - 1) ~y:(my_y - 1) gcolor;
  mypic#fill ~x:(my_x - 3) ~y:(my_y - 1) gcolor;
  mypic#fill ~x:(my_x - 1) ~y:(my_y + 1) gcolor;
  mypic#fill ~x:(my_x + 1) ~y:(my_y) gcolor

let draw_tag mypic title gdown gup gcolor  =
  let my_sum gl = List.fold_left (+) 0 (Fifo.to_list gl) in
  let meanx gl = ((float_of_int (my_sum gl)) /. (float_of_int (Fifo.length gl))) in
  let down_bw = (string_of_float (float_of_int(int_of_float((meanx gdown) /. 1024. *. 100.)) /. 100.)) in
  let up_bw = (string_of_float (float_of_int(int_of_float((meanx gup) /. 1024. *. 100.)) /. 100.)) in
  let bw_d = "Dl: " ^ down_bw ^ "KB/s "
  and bw_u = "Ul: " ^ up_bw ^ "KB/s" in
  if !!html_mods_vd_gfx_tag_enable_title then
  mypic#string ~font:Gd.Font.giant ~x:!!html_mods_vd_gfx_tag_title_x_pos ~y:!!html_mods_vd_gfx_tag_title_y_pos ~s:title gcolor;
  mypic#string ~font:Gd.Font.giant ~x:!!html_mods_vd_gfx_tag_dl_x_pos ~y:!!html_mods_vd_gfx_tag_dl_y_pos  ~s:bw_d gcolor;
  mypic#string ~font:Gd.Font.giant ~x:!!html_mods_vd_gfx_tag_ul_x_pos ~y:!!html_mods_vd_gfx_tag_ul_y_pos  ~s:bw_u gcolor

let draw_title mypic title gcolor win_y =
  mypic#string_up ~font:Gd.Font.giant ~x:2 ~y:((win_y / 2)  + (((String.length title) * 8) / 2)) ~s:title gcolor

let draw_top_legend mypic title tcolor gcolor scolor win_y =
  mypic#line ~x1:xbl ~y1:9 ~x2:(xbl + 10) ~y2:9 scolor;
  mypic#line ~x1:xbl ~y1:8 ~x2:(xbl + 10) ~y2:8 gcolor;
  mypic#string ~font:Gd.Font.small ~x:(xbl + 16) ~y:2 ~s:title tcolor

let draw_dual_top_legend mypic titlel tcolorl gcolorl scolorl titler tcolorr gcolorr scolorr win_y =
  let my_x = (xbr()) in
  mypic#line ~x1:xbl ~y1:9 ~x2:(xbl + 10) ~y2:9 scolorl;
  mypic#line ~x1:xbl ~y1:8 ~x2:(xbl + 10) ~y2:8 gcolorl;
  mypic#string ~font:Gd.Font.small ~x:(xbl + 16) ~y:2 ~s:titlel tcolorl;
  mypic#line ~x1:my_x ~y1:9 ~x2:(my_x - 10) ~y2:9 scolorr;
  mypic#line ~x1:my_x ~y1:8 ~x2:(my_x - 10) ~y2:8 gcolorr;
  mypic#string ~font:Gd.Font.small ~x:(my_x - (((String.length titler) * 8)) - 2) ~y:2 ~s:titler tcolorr

let draw_v_legend mypic g vdt legend_text lcolor gcolor offset =
  let my_x = (xbr()) in
  let my_y = (ybb()) in
  let fy x = int_of_float ((float_of_int my_y) -. (((float_of_int x) /. (y_fdivisions())) *. (float_of_int(ybs())))) in
  let vtext n = (string_of_float (float_of_int(int_of_float((float_of_int (my_y - (fy n))) /. vdt /. 1024. *. 100.)) /. 100.)) in
    for n = 1 to ((y_divisions()) - 1) do
      mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy n) - 12 + offset) ~s:(vtext n) gcolor;
    done;
    mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy (y_divisions())) - 5) ~s:(legend_text) lcolor

let draw_stack_v_top_legend mypic g vdt legend_text lcolor gcolor offset =
  let my_x = (xbr()) in
  let my_y = (ybb()  - (ybs() / 2)) in
  let fy x = int_of_float ((float_of_int my_y) -. (((float_of_int x) /. (y_fdivisions())) *. (float_of_int(ybs())))) in
  let vtext n = (string_of_float (float_of_int(int_of_float((float_of_int (my_y - (fy n))) /. vdt /. 1024. *. 100.)) /. 100.)) in
    for n = 1 to ((y_divisions()) / 2) do
      mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy n) - 12 + offset) ~s:(vtext n) gcolor;
    done;
    mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy (y_divisions())) - 5) ~s:(legend_text) lcolor

let draw_stack_v_bottom_legend mypic g vdt legend_text lcolor gcolor offset =
  let my_x = (xbr()) in
  let my_y = (ybb()  - (ybs() / 2)) in
  let fy x = int_of_float ((float_of_int my_y) +. (((float_of_int x) /. (y_fdivisions())) *. (float_of_int(ybs())))) in
  let vtext n = (string_of_float (float_of_int(int_of_float((float_of_int ((fy n) - my_y)) /. vdt /. 1024. *. 100.)) /. 100.)) in
  for n = 1 to ((y_divisions()) / 2) do
    mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy n) - 12 + offset) ~s:(vtext n) gcolor;
  done;
  mypic#string ~font:Gd.Font.small ~x:(my_x + 5) ~y:((fy (y_divisions())) - 5) ~s:(legend_text) lcolor

let draw_h_legend mypic g legend_text gcolor my_time basetime show_days =
  let my_x = (xbr()) in
  let my_y = (ybb()) in
  let fx x = my_x + 4 - (x * (xbs ()) / x_divisions ()) in
  let time_string n =
    let time = Unix.localtime (basetime -. float_of_int(n * my_time / x_divisions ())) in
    Printf.sprintf "%02d:%02d:%02d" time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec
  in
  let day_string n =
    let time = Unix.localtime (basetime -. float_of_int(n * my_time / x_divisions ())) in
    Printf.sprintf "%02d.%02d." time.Unix.tm_mday (time.Unix.tm_mon + 1)
  in
  if show_days then
    begin
      for n = 0 to (x_divisions() - 1) do
        mypic#string ~font:Gd.Font.small ~x:((fx n) - (String.length (time_string n) * 3) ) ~y:(my_y + 1) ~s:(time_string n) gcolor;
        mypic#string ~font:Gd.Font.small ~x:((fx n) - (String.length (day_string n) * 3) ) ~y:(my_y + 11) ~s:(day_string n) gcolor;
      done;
      mypic#string ~font:Gd.Font.small ~x:(4) ~y:(my_y + 2) ~s:(legend_text) gcolor;
      mypic#string ~font:Gd.Font.small ~x:(4) ~y:(my_y + 11) ~s:("t(DD.MM.)") gcolor	
    end
  else
    begin
      for n = 0 to (x_divisions() - 1) do
        mypic#string ~font:Gd.Font.small ~x:((fx n) - (String.length (time_string n) * 3) ) ~y:(my_y + 5) ~s:(time_string n) gcolor;
      done;
      mypic#string ~font:Gd.Font.small ~x:(4) ~y:(my_y + 5) ~s:(legend_text) gcolor
    end

let draw_load mypic g vdt my_color shadow_color my_samples =
  let my_x = (xbr()) in
  let my_y = (ybb()) in
  let my_s = min ((Fifo.length g)-1) my_samples in
  let my_s2 = xbs() / my_samples / 4 in
  let datas g n = List.nth (List.rev (Fifo.to_list g)) n in
  let fx x = my_x - (x * (xbs ()) / my_samples)
  and y_c1 n = (my_y - (int_of_float(float_of_int(datas g n) *. vdt)))
  and y_c2 n = (my_y - (int_of_float(float_of_int(datas g (n+1)) *. vdt))) in
  (if !!html_mods_vd_gfx_fill then begin
      if my_s2 = 0 then	
        for n = 0 to my_s - 1 do
          mypic#line ~x1:(fx n) ~y1:(min (my_y-1)(y_c1 n)) ~x2:(fx(n+1)) ~y2:(min (my_y-2)(y_c2 n)) shadow_color
        done
      else
        for n = 0 to my_s - 1 do
          mypic#line ~x1: (fx n)               ~y1:(min (my_y - 1) (y_c1 n)) ~x2:((fx(n + 1)) + my_s2) ~y2:(min (my_y - 1) (y_c1 n)) shadow_color;
          mypic#line ~x1:((fx(n + 1)) + my_s2) ~y1:(min (my_y - 1) (y_c1 n)) ~x2: (fx(n + 1))          ~y2:(min (my_y - 1) (y_c2 n)) shadow_color
        done;
      if (fx my_s) > (xbl + 1) then
        mypic#line ~x1:(fx my_s) ~y1:(min (my_y - 1) (y_c1 my_s)) ~x2:(fx my_s) ~y2:(my_y) shadow_color;
  mypic#fill ~x:(my_x - 1) ~y:(my_y - 1) my_color;
  end
  else begin
    if my_s2 = 0 then
      for n = 0 to my_s - 1 do
        mypic#line ~x1:((fx n)+1) ~y1:((y_c1 n)+1) ~x2:((fx (n+1))+1) ~y2:((y_c2 n)+1) shadow_color;
        mypic#line ~x1:( fx n) ~y1:(y_c1 n) ~x2:( fx (n+1)) ~y2:(y_c2 n) my_color
      done
    else
      for n = 0 to my_s - 1 do
        mypic#line ~x1:((fx n) + 1)              ~y1:((y_c1 n)+1) ~x2:((fx(n + 1)) + 1 + my_s2) ~y2:((y_c1 n) + 1) shadow_color;
        mypic#line ~x1:((fx(n + 1)) + 1 + my_s2) ~y1:((y_c1 n)+1) ~x2:((fx(n + 1)) + 1)         ~y2:((y_c2 n) + 1) shadow_color;
        mypic#line ~x1: (fx n)                   ~y1: (y_c1 n)    ~x2:((fx(n + 1)) + my_s2)     ~y2: (y_c1 n) my_color;
        mypic#line ~x1:((fx(n + 1)) + my_s2)     ~y1: (y_c1 n)    ~x2:( fx(n + 1))              ~y2: (y_c2 n) my_color
      done;
  end
  )

let draw_stack_download mypic g vdt my_color shadow_color my_samples =
  let my_x = (xbr()) in
  let my_x2 = (xbs()) in
  let my_y = (ybt -1 + (ybs() / 2)) in
  let my_s = min ((Fifo.length g)-1) my_samples in
  let datas g n = List.nth (List.rev (Fifo.to_list g)) n in
  let fx x = my_x - (x * my_x2 / my_samples) and
      y_c1 n = my_y - int_of_float(float_of_int(datas g n) *. vdt) and
      y_c2 n = my_y - int_of_float(float_of_int(datas g (n+1)) *. vdt)
  in
  (if !!html_mods_vd_gfx_fill then begin
    for n = 0 to my_s - 1 do
      mypic#line ~x1:(fx n) ~y1:(min (my_y-1) (y_c1 n)) ~x2:(fx(n+1)) ~y2:(min (my_y-1) (y_c2 n)) shadow_color
    done;
    if (fx my_s) > (xbl+1) then
      mypic#line ~x1:(fx my_s) ~y1:(my_y) ~x2:(fx my_s) ~y2:(min (my_y) (y_c1 my_s)) shadow_color;
  mypic#fill ~x:(my_x) ~y:(my_y) my_color;
  end
  else begin
    for n = 0 to my_s - 1 do
      mypic#line ~x1:((fx n)+1) ~y1:((y_c1 n)+1) ~x2:((fx (n+1))+1) ~y2:((y_c2 n)+1) shadow_color;
      mypic#line ~x1:(fx n) ~y1:(y_c1 n) ~x2:(fx (n+1)) ~y2:(y_c2 n) my_color
    done;
  end
  )

let draw_stack_upload mypic g vdt my_color shadow_color my_samples =
  let my_x = (xbr()) in
  let my_x2 = (xbs()) in
  let my_y = (ybt + 1 + (ybs() / 2)) in
  let my_s = min ((Fifo.length g)-1) my_samples in
  let datas g n = List.nth (List.rev (Fifo.to_list g)) n in
  let fx x = my_x - (x * my_x2 / my_samples)
  and y_c1 n = (my_y + (int_of_float(float_of_int(datas g n) *. vdt)))
  and y_c2 n = (my_y + (int_of_float(float_of_int(datas g (n+1)) *. vdt))) in
  (if !!html_mods_vd_gfx_fill then begin
    for n = 0 to my_s - 1 do
      mypic#line ~x1:(fx n) ~y1:(max (my_y+1) (y_c1 n)) ~x2:(fx(n+1)) ~y2:(max (my_y+1) (y_c2 n)) shadow_color
    done;
    if (fx my_s) > xbl+1 then
      mypic#line ~x1:(fx my_s) ~y1:(my_y) ~x2:(fx my_s) ~y2:(max my_y (y_c1 my_s)) shadow_color;
    mypic#fill ~x:(my_x) ~y:(my_y) my_color
  end
  else begin
    for n = 0 to my_s - 1 do
      mypic#line ~x1:((fx n)+1) ~y1:(max (my_y+1)((y_c1 n)+1)) ~x2:((fx (n+1))+1) ~y2:(max (my_y+1)((y_c2 n)+1)) shadow_color;
      mypic#line ~x1:( fx n) ~y1:(max (my_y+1)(y_c1 n)) ~x2:( fx (n+1)) ~y2:(max (my_y+1)(y_c2 n)) my_color
    done;
  end
  )

let draw_mean_line mypic g vdt my_color shadow_color tcolor =
  let my_sum gl = List.fold_left (+) 0 (Fifo.to_list gl) in
  let meanx() = ((float_of_int (my_sum g)) /. (float_of_int ((Fifo.length g)))) in
  let ypos = (ybb() - int_of_float(meanx() *. vdt)) in
  let vtext = (string_of_float (float_of_int(int_of_float(meanx() /. 1024. *. 100.)) /. 100.)) in
  mypic#line ~x1:(xbl) ~y1:(ypos) ~x2:(xbl+1+ xbs() / x_divisions()) ~y2:(ypos) my_color;
  if ypos+1 < ybb() then
    mypic#line ~x1:(xbl) ~y1:(ypos+1) ~x2:(xbl+1+ xbs() / x_divisions()) ~y2:(ypos+1) shadow_color;	
  mypic#string ~font:Gd.Font.small ~x:(xbl) ~y:(ypos +2 ) ~s:(vtext) tcolor

let draw_mygraph mypic ttl top_title vl hl g =
  (* init pic *)
(
  let g_y = win_y() in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in
  (* set colors *)
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in

  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black red darkgrey g_y;
  draw_v_legend mypic g (vdt_m g) vl black black 0;
  draw_h_legend mypic g hl black history_time !history_timeflag false;
  draw_load mypic g (vdt_m g) green darkgrey history_size;
  draw_x_grid mypic black darkgrey (x_divisions());
  draw_y_grid mypic black;
 (* draw_mean_line mypic gdown green darkgrey black; *)
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
  draw_h_legend mypic gdown hl black history_time !history_timeflag false;
  draw_load mypic gdown green darkgrey history_size;
  draw_load mypic gup red darkgrey history_size;
  draw_x_grid mypic black darkgrey (x_divisions());
  draw_y_grid mypic black;

 (* draw_mean_line mypic gdown green darkgrey  black;*)
 (* mypic#out_as_jpeg "line.jpg" ~quality:90; *)

*)

(* do_draw_pic "Traffic" "(kb)" "(t)" download_history upload_history *)

let do_draw_pic ttl vl hl gdown gup =
  (* init pic *)
(
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
  draw_h_legend mypic gdown hl black history_time !history_timeflag false;
  (if !!html_mods_vd_gfx_stack then begin
    draw_stack_borders mypic black;
    let vdt = min (vdt_stack gdown) (vdt_stack gup) in
    draw_stack_v_top_legend mypic gdown vdt vl black darkgreen 5;
    draw_stack_v_bottom_legend mypic gup vdt vl black darkred 5;
    (* enable filling for stack graph *)
    if not !!html_mods_vd_gfx_fill then html_mods_vd_gfx_fill =:= true;
    draw_stack_download mypic gdown vdt green darkgrey history_size;
    draw_stack_upload mypic gup vdt red darkgrey history_size;
    draw_x_grid mypic black darkgrey (x_divisions());
    draw_y_grid mypic black
   end
  else begin
    draw_borders mypic black;
    let vdt = min (vdt_m gdown) (vdt_m gup) in
    draw_v_legend mypic gdown vdt vl black darkgreen 0;
    draw_v_legend mypic gup vdt vl black darkred 10;
    if !!html_mods_vd_gfx_fill then html_mods_vd_gfx_fill =:= false;
    draw_load mypic gdown vdt green darkgrey history_size;
    draw_load mypic gup vdt red darkgrey history_size;
    draw_x_grid mypic black darkgrey (x_divisions());
    draw_y_grid mypic black; 
    (if !!html_mods_vd_gfx_mean then
      draw_mean_line mypic gdown vdt green darkgrey black;
      draw_mean_line mypic gup vdt red darkgrey black
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
  let g_y = win_y() in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black green darkgrey g_y;
  let vdt = vdt_m gdown in
  draw_v_legend mypic gdown vdt vl black black 6;
  draw_h_legend mypic gdown hl black history_time !history_timeflag false;
  draw_load mypic gdown vdt green darkgrey history_size;
  draw_x_grid mypic black darkgrey (x_divisions());
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gdown vdt green darkgrey black;

  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_download.png"
  else
    mypic#save_as_jpeg "bw_download.jpg" ~quality:90
  );
)

let do_draw_up_pic ttl top_title vl hl gup =
  (* init pic *)
(
  let g_y = win_y() in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black red darkgrey g_y;
  let vdt = vdt_m gup in
  draw_v_legend mypic gup vdt vl black black 6;
  draw_h_legend mypic gup hl black history_time !history_timeflag false;
  draw_load mypic gup vdt red darkgrey history_size;
  draw_x_grid mypic black darkgrey (x_divisions());
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gup vdt red darkgrey black;

  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_upload.png"
  else
    mypic#save_as_jpeg "bw_upload.jpg" ~quality:90
  );
)

let do_draw_h_pic ttl vl hl gdown gup =
  (* init pic *)
(
  let g_y = win_y() in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in
  let x_legend_days = (int_of_float !history_h_timeflag) mod 86400 < (x_h_time gdown) in
        
  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkgreen = mypic#colors#resolve ~red:0 ~blue:0 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  let vdt = min (vdt_m gup) (vdt_m gdown) in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_dual_top_legend mypic "download" black green darkgrey "upload" black red darkgrey g_y;
  draw_v_legend mypic gdown vdt vl black darkgreen 0;
  draw_v_legend mypic gup vdt vl black darkred 10;
  draw_h_legend mypic gup hl black (x_h_time gdown) !history_h_timeflag x_legend_days;
  if !!html_mods_vd_gfx_fill then html_mods_vd_gfx_fill =:= false;
  draw_load mypic gdown vdt green darkgrey (x_h_values gdown);
  draw_load mypic gup vdt red darkgrey (x_h_values gdown);
  draw_x_grid mypic black darkgrey (x_divisions());
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  (if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gdown vdt green darkgrey black;
    draw_mean_line mypic gup vdt red darkgrey black
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
  let g_y = win_y() in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in
  let x_legend_days = (int_of_float !history_h_timeflag) mod 86400 < (x_h_time gdown) in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let green = mypic#colors#resolve ~red:0 ~blue:0 ~green:255 in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black green darkgrey g_y;
  let vdt = vdt_m gdown in
  draw_v_legend mypic gdown vdt vl black black 6;
  draw_h_legend mypic gdown hl black  (x_h_time gdown) !history_h_timeflag x_legend_days;
  draw_load mypic gdown vdt green darkgrey (x_h_values gdown);
  draw_x_grid mypic black darkgrey (x_divisions());
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gdown vdt green darkgrey black;

  (if !!html_mods_vd_gfx_png then
    mypic#save_as_png "bw_h_download.png"
  else
    mypic#save_as_jpeg "bw_h_download.jpg" ~quality:90
  );
)

let do_draw_up_h_pic ttl top_title vl hl gup =
  (* init pic *)
(
  let g_y = win_y() in
  let x_legend_days = (int_of_float !history_h_timeflag) mod 86400 < (x_h_time gup) in
  let mypic = Gd.create ~x:(win_x()) ~y:g_y in

  (* set colors *)
  let white = mypic#colors#white in
  let black = mypic#colors#black in
  let red = mypic#colors#red in
  let darkgrey = mypic#colors#resolve ~red:128 ~blue:128 ~green:128 in
  let darkred = mypic#colors#resolve ~red:128 ~blue:0 ~green:0 in

  if !!html_mods_vd_gfx_transparent then
    mypic#colors#set_transparent white;
  (* draw graph *)
  draw_borders mypic black;
  draw_title mypic ttl black g_y;
  draw_top_legend mypic top_title black red darkgrey g_y;
  let vdt = vdt_m gup in
  draw_v_legend mypic gup vdt vl black black 6;
  draw_h_legend mypic gup hl black (x_h_time gup) !history_h_timeflag x_legend_days;
  draw_load mypic gup vdt red darkgrey (x_h_values gup);
  draw_x_grid mypic black darkgrey (x_divisions());
  draw_y_grid mypic black;
  draw_arrow mypic darkred;
  if !!html_mods_vd_gfx_mean then
    draw_mean_line mypic gup vdt red darkgrey black;

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
  let black = mypic#colors#black in
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

end
