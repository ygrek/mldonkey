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

(** GUI for the networks selection. *)

open Options
open Md4
open GMain

open Gettext
open Gui_global
open CommonTypes
open GuiTypes
open GuiProto

module M = Gui_messages
module O = Gui_options
module G = Gui_global

let (!!) = Options.(!!)

let data =
[|
  (M.o_xpm_bt_net_on, M.o_xpm_bt_net_off, M.nT_lb_display_bt,
    M.nT_lb_net_bt);
  (M.o_xpm_dc_net_on, M.o_xpm_dc_net_off, M.nT_lb_display_dc,
    M.nT_lb_net_dc);
  (M.o_xpm_ftt_net_on, M.o_xpm_ftt_net_off, M.nT_lb_display_ftt,
    M.nT_lb_net_ftt);
  (M.o_xpm_ed2k_net_on, M.o_xpm_ed2k_net_off, M.nT_lb_display_ed2k,
    M.nT_lb_net_ed2k);
  (M.o_xpm_gnut_net_on, M.o_xpm_gnut_net_off, M.nT_lb_display_gnut,
    M.nT_lb_net_gnut);
  (M.o_xpm_nap_net_on, M.o_xpm_nap_net_off, M.nT_lb_display_nap,
    M.nT_lb_net_nap);
  (M.o_xpm_slsk_net_on, M.o_xpm_slsk_net_off, M.nT_lb_display_slsk,
    M.nT_lb_net_slsk);
|]


let fill_box box i net_enabled net_displayed net_exist =
  List.iter box#wtog_net#remove box#wtog_net#children;
  let (wpix_on, wpix_off, text_display, text_net) = data.(i) in
  let pix =
    if net_enabled then
      Gui_options.gdk_pix wpix_on
      else Gui_options.gdk_pix wpix_off
  in
  ignore (GMisc.pixmap pix ~packing:box#wtog_net#add ());
  box#wl_net#set_text text_net;
  let style = box#wl_net#misc#style#copy in
  style#set_font (Gdk.Font.load_fontset (!!O.font_networks));
  let fg_col =
    if net_enabled then
      [(`NORMAL, `NAME !!O.color_network_enabled)]
      else [(`NORMAL, `NAME !!O.color_network_disabled)]
  in
  style#set_fg fg_col;
  box#wl_net#misc#set_style style;
  box#wl_display#set_text text_display;
  let style = box#wl_display#misc#style#copy in
  let fg_col =
    if net_displayed then
      [(`NORMAL, `NAME !!O.color_network_enabled)]
      else [(`NORMAL, `NAME !!O.color_network_disabled)]
  in
  style#set_fg fg_col;
  box#wl_display#misc#set_style style;
  box#wtog_net#set_active net_enabled;
  box#wchk_net#set_active net_displayed;
  if net_exist then
    begin
      box#wtog_net#misc#set_sensitive true;
      box#wchk_net#misc#set_sensitive true
    end
    else begin
      box#wtog_net#misc#set_sensitive false;
      box#wchk_net#misc#set_sensitive false
    end

let retrieve_net_box name box =
  match name with
      "BitTorrent" -> (box#box_bt, 0)
    | "Direct Connect" -> (box#box_dc, 1)
    | "Fasttrack" -> (box#box_ftt, 2)
    | "Donkey" -> (box#box_ed2k, 3)
    | "Gnutella" -> (box#box_gnut, 4)
    | "Open Napster" -> (box#box_nap, 5)
    | "Soulseek" -> (box#box_slsk, 6)
    | _ -> raise Not_found

class net_left () =
  let _hbox_1 = GPack.hbox ~homogeneous:false () in
  let _hbox_2 =
    GPack.hbox ~homogeneous:false
      ~packing:(_hbox_1#pack ~expand:true ~fill:false) ()
  in
  let _vbox_1 =
    GPack.vbox ~homogeneous:false ~spacing:5
      ~packing:(_hbox_1#pack ~expand:false ~fill:false) ()
  in
  let wl_net =
    GMisc.label ~justify:`RIGHT ~line_wrap:true
      ~packing:(_vbox_1#pack ~expand:false ~fill:false) ()
  in
  let _hbox_3 =
    GPack.hbox ~homogeneous:false
      ~packing:(_vbox_1#pack ~expand:false ~fill:true) ()
  in
  let _hbox_4 =
    GPack.hbox ~homogeneous:false
      ~packing:(_hbox_3#pack ~expand:true ~fill:false) ()
  in
  let wl_display =
    GMisc.label ~justify:`RIGHT ~line_wrap:true
      ~packing:(_hbox_3#pack ~expand:false ~fill:false ~padding:10) ()
  in
  let wchk_net =
    GButton.check_button ~active:false ~draw_indicator:true
      ~packing:(_hbox_3#pack ~expand:false ~fill:false) ()
  in
  let wtog_net =
    GButton.toggle_button ~active:false ~draw_indicator:false
      ~packing:(_hbox_1#pack ~expand:false ~fill:false  ~padding:10) ()
  in
  object
    val wl_net = wl_net
    val wl_display = wl_display
    val wchk_net = wchk_net
    val wtog_net = wtog_net
    method wl_net = wl_net
    method wl_display = wl_display
    method wchk_net = wchk_net
    method wtog_net = wtog_net
    method coerce = _hbox_1#coerce
  end

class net_right () =
  let _box_1 = GPack.hbox ~homogeneous:false () in
  let wtog_net =
    GButton.toggle_button ~active:false ~draw_indicator:false
      ~packing:(_box_1#pack ~expand:false ~fill:false ~padding:10) ()
  in
  let _box_2 =
    GPack.vbox ~homogeneous:false ~spacing:5
      ~packing:(_box_1#pack ~expand:false ~fill:false) ()
  in
  let wl_net =
    GMisc.label ~justify:`LEFT ~line_wrap:true
      ~packing:(_box_2#pack ~expand:false ~fill:false) ()
  in
  let _box_3 =
    GPack.hbox ~homogeneous:false
      ~packing:(_box_2#pack ~expand:false ~fill:false) ()
  in
  let wchk_net =
    GButton.check_button ~active:false ~draw_indicator:true
      ~packing:(_box_3#pack ~expand:false ~fill:false) ()
  in
  let wl_display =
    GMisc.label ~justify:`LEFT ~line_wrap:true
      ~packing:(_box_3#pack ~expand:false ~fill:false ~padding:10) ()
  in
  object
    val wtog_net = wtog_net
    val wl_net = wl_net
    val wchk_net = wchk_net
    val wl_display = wl_display
    method wtog_net = wtog_net
    method wl_net = wl_net
    method wchk_net = wchk_net
    method wl_display = wl_display
    method coerce = _box_1#coerce
  end

class net_top () =
  let _vbox_1 = GPack.vbox ~homogeneous:false ~border_width:50 ~spacing:10 () in
  let _hbox_2 = GPack.hbox ~homogeneous:true
      ~packing:(_vbox_1#pack ~expand:false ~fill:false) ()
  in
  let wl_net =
    GMisc.label ~justify:`CENTER ~line_wrap:true
      ~packing:(_hbox_2#pack ~expand:false ~fill:false) ()
  in
  let _hbox_3 = GPack.hbox ~homogeneous:true
      ~packing:(_vbox_1#pack ~expand:false ~fill:false) ()
  in
  let _hbox_4 = GPack.hbox ~homogeneous:false
      ~packing:(_hbox_3#pack ~expand:false ~fill:false) ()
  in
  let wchk_net =
    GButton.check_button ~active:false ~draw_indicator:true
      ~packing:(_hbox_4#pack ~expand:false ~fill:false) ()
  in
  let wl_display =
    GMisc.label ~justify:`CENTER ~line_wrap:true
      ~packing:(_hbox_4#pack ~expand:false ~fill:false) ()
  in
  let _hbox_5 = GPack.hbox ~homogeneous:true
      ~packing:(_vbox_1#pack ~expand:false ~fill:false) ()
  in
  let wtog_net =
    GButton.toggle_button ~active:false ~draw_indicator:false
      ~packing:(_hbox_5#pack ~expand:false ~fill:false) ()
  in
  object
    val wl_net = wl_net
    val wchk_net = wchk_net
    val wl_display = wl_display
    val wtog_net = wtog_net
    method wl_net = wl_net
    method wchk_net = wchk_net
    method wl_display = wl_display
    method wtog_net = wtog_net
    method coerce = _vbox_1#coerce
  end


class net_animation () =
  let wf_net = GBin.frame ~width:256 ~height:256 ~shadow_type:`OUT () in
  let wdraw_net = GMisc.drawing_area ~packing:(wf_net#add) () in
  object
    val wf_net = wf_net
    val wdraw_net = wdraw_net
    method wf_net = wf_net
    method wdraw_net = wdraw_net
    method coerce = wf_net#coerce
  end

class box () =
  let vbox = GPack.vbox ~homogeneous:false () in
  let scroll =
    GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
      ~placement:`TOP_LEFT
      ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  let vbox_1 =
    GPack.vbox ~homogeneous:false
      ~packing:(scroll#add_with_viewport) ()
  in
  let hbox = GPack.hbox ~homogeneous:true
      ~packing:(vbox_1#pack ~expand:false ~fill:false) ()
  in
  let hbox_1 = GPack.hbox ~homogeneous:true
      ~packing:(vbox_1#pack ~expand:false ~fill:false) ()
  in
  let hbox_2 =
    GPack.hbox ~homogeneous:false
      ~packing:(hbox_1#pack ~expand:false ~fill:false) ()
  in
  let vbox_left =
    GPack.vbox ~homogeneous:false ~spacing:20
      ~packing:(hbox_2#pack ~expand:false ~fill:false) ()
  in
  let vbox_center =
    GPack.vbox ~homogeneous:false
      ~packing:(hbox_2#pack ~expand:false ~fill:false ~padding:10) ()
  in
  let vbox_right =
    GPack.vbox ~homogeneous:false ~spacing:20
      ~packing:(hbox_2#pack ~expand:false ~fill:false) ()
  in
  object
    val hbox = hbox
    val vbox_left = vbox_left
    val vbox_center = vbox_center
    val vbox_right = vbox_right
    method hbox = hbox
    method vbox_left = vbox_left
    method vbox_center = vbox_center
    method vbox_right = vbox_right
    method coerce = vbox#coerce
  end

let animation_timerID =
  ref (Timeout.add ~ms:500
        ~callback:(fun _ -> true))

type anim_image =
  {
   mutable x : int;
   mutable y : int;
   width : int;
   height : int;
   mutable pixmaps : GDraw.pixmap list;
  }

let pixmap_to_anim_image pixmaps_list =
  {
   x = 0;
   y = 0;
   width = fst ((List.nth pixmaps_list 0)#size);
   height = snd ((List.nth pixmaps_list 0)#size);
   pixmaps = pixmaps_list;
  }

let img_gallery =
  let l = ref [] in
  let li = Array.to_list data in
  List.iter (fun (d_on, d_off, _, _) ->
    let pix1 = O.resize (O.gdk_pix d_on) 0.75 in
    let pix2 = O.resize (O.gdk_pix d_off) 0.75 in
    let d = pixmap_to_anim_image [pix1; pix2] in
    l := d::!l
  ) li;
  let pix1 = O.gdk_pix M.o_xpm_mld_tux_on in
  let pix2 = O.gdk_pix M.o_xpm_mld_tux_off in
  let d = pixmap_to_anim_image [pix1; pix2] in
  l := d::!l;
  Array.of_list !l

let asserted col =
  if (col > 0) && (col < 256 * 255) then
    col
    else if col < 0 then
      0
      else 256 * 255

let gradual (rf, gf, bf) (rt, gt, bt) width pos =
  let w = float_of_int width in
  let p = float_of_int pos in
  let r_step = if w = 0. then 0. else float_of_int (abs (rt - rf)) /. w in
  let r_sign = if (rt - rf) = 0 then 1. else float_of_int (abs (rt - rf) / (rt - rf)) in
  let g_step = if w = 0. then 0. else float_of_int (abs (gt - gf)) /. w in
  let g_sign = if (gt - gf) = 0 then 1. else float_of_int (abs (gt - gf) / (gt - gf)) in
  let b_step = if w = 0. then 0. else float_of_int (abs (bt - bf)) /. w in
  let b_sign = if (bt - bf) = 0 then 1. else float_of_int (abs (bt - bf) / (bt - bf)) in
  let r_of_pos = 256 * int_of_float (float_of_int rf +. r_sign *. r_step *. p) in
  let g_of_pos = 256 * int_of_float (float_of_int gf +. g_sign *. g_step *. p) in
  let b_of_pos = 256 * int_of_float (float_of_int bf +. b_sign *. b_step *. p) in
  (`RGB (asserted r_of_pos, asserted g_of_pos, asserted b_of_pos))

class networks_box () =
  let bt_box = new net_left () in
  let dc_box = new net_right () in
  let nap_box = new net_left () in
  let ed2k_box = new net_top () in
  let ftt_box = new net_left () in
  let gnut_box = new net_right () in
  let slsk_box = new net_right () in
  let animation_frame = new net_animation () in
  let pix1 =
    GDraw.pixmap ~width:256 ~height:256
      ~colormap:(Gdk.Color.get_system_colormap ()) ()
  in
  let background = pixmap_to_anim_image [pix1] in
  object (self)
    inherit box () as box

    val background = (background : anim_image)
    val dwg = (animation_frame#wdraw_net : GMisc.drawing_area)

    method render_to_drawing pixmap =
      let drawing = (dwg#misc#realize (); new GDraw.drawable (dwg#misc#window)) in
      drawing#put_pixmap
        ~x:0 ~y:0
        ~width:256 ~height:256
        pixmap#pixmap

    method fade prog image pixmap reverse =
      if reverse then
        begin
          let p = (100 - prog) in
          if p > 0 then
           for i = 0 to (p * (image.height - 1) / 100) do
             let y = image.y + i * 100 / p in
             pixmap#set_foreground `WHITE;
             pixmap#line ~x:image.x ~y ~x:(image.x + image.width) ~y
           done
        end else
          begin
            let p = prog in
            if p > 0 then
              for i = 0 to (p * (image.height - 1) / 100) do
                let y = image.y + i * 100 / p in
                pixmap#set_foreground `WHITE;
                pixmap#line ~x:image.x ~y ~x:(image.x + image.width) ~y
              done
          end

    (* blank drawing *)
    method anim_1 =
      let pixmap = List.nth background.pixmaps 0 in
      pixmap#set_foreground `WHITE;
      pixmap#rectangle ~filled:true ~x:0 ~y:0 ~width:256 ~height:256 ();
      self#render_to_drawing pixmap;
      background.pixmaps <- [pixmap];
      self#anim_2

    (* mld_tux appears *)
    method anim_2 =
      let tux = img_gallery.(0) in
      tux.x <- (background.width / 2 - tux.width / 2);
      tux.y <- (background.height / 2 - tux.height / 2 - 5);
      let colored_tux = List.nth tux.pixmaps 0 in
      let grey_tux = List.nth tux.pixmaps 1 in
      let progress = ref 0 in
      let render_tux _ = (
        let pixmap = List.nth background.pixmaps 0 in
        if !progress >= 110 then
          begin
            Timeout.remove (!animation_timerID);
            pixmap#put_pixmap
              ~x:tux.x ~y:tux.y
              ~width:tux.width ~height:tux.height
              colored_tux#pixmap;
            self#render_to_drawing pixmap;
            background.pixmaps <- [pixmap];
            self#anim_3
          end
          else begin
            pixmap#put_pixmap
              ~x:tux.x ~y:tux.y
              ~width:tux.width ~height:tux.height
              grey_tux#pixmap;
            self#fade !progress tux pixmap true;
            self#render_to_drawing pixmap;
            background.pixmaps <- [pixmap];
          end
      ) in
      animation_timerID :=
        (Timeout.add ~ms:500
           ~callback:(fun _ ->
                        render_tux ();
                        progress := !progress + 20;
                        true))

    (* net icons appears *)
    method anim_3 =
      let pixmap = List.nth background.pixmaps 0 in
      pixmap#set_foreground `WHITE;
      pixmap#rectangle ~filled:true ~x:0 ~y:0 ~width:256 ~height:256 ();
      for i = 0 to 6 do
        let col =
          if i <= 3 then
            gradual (0, 0, 0) (200, 200, 200) 3 i
            else gradual (200, 200, 200) (0, 0, 0) 4 (i - 4)
        in
        pixmap#set_foreground col;
        pixmap#arc
          ~x:(background.width / 2 - 103 + i) ~y:(background.height / 2 - 103 + i)
          ~width:(2 * (103 - i)) ~height:(2 * (103 - i)) ~filled:true ()
      done;
      pixmap#set_foreground `WHITE;
      pixmap#arc ~x:(background.width / 2 - 96) ~y:(background.height / 2 - 96) ~width:192 ~height:192 ~filled:true ();
      let tux = img_gallery.(0) in
      let colored_tux = List.nth tux.pixmaps 0 in
      pixmap#put_pixmap
        ~x:tux.x ~y:tux.y
        ~width:tux.width ~height:tux.height
        colored_tux#pixmap;
      self#render_to_drawing pixmap;
      background.pixmaps <- [pixmap];
      let li = List.tl (Array.to_list img_gallery) in
      let len = List.length li in
      let pi = 3.14159 in
      let alpha = 2. *. pi /. float_of_int len in
      let n = ref 0 in
      List.iter (fun image ->
        image.x <- (background.width / 2) - (image.width / 2) + int_of_float (100. *. cos (float_of_int !n *. alpha));
        image.y <- (background.height / 2) - (image.height / 2) - int_of_float (100. *. sin (float_of_int !n *. alpha));
        incr (n)
      ) li;
      let progress = ref 0 in
      n := 0;
      let ind = ref 1 in
      let render_net net_image _ = (
        let pixmap = List.nth background.pixmaps 0 in
        let image = List.nth net_image.pixmaps !ind in
        if !progress >= 110 then
          if (!n = len - 1) && (!ind = 0) then
            begin
              Timeout.remove (!animation_timerID);
              pixmap#put_pixmap
                ~x:net_image.x ~y:net_image.y
                ~width:net_image.width ~height:net_image.height
                image#pixmap;
              self#render_to_drawing pixmap;
              background.pixmaps <- [pixmap];
              self#anim_4
            end else
              begin
                if (!n = len - 1) then
                  begin
                    pixmap#put_pixmap
                      ~x:net_image.x ~y:net_image.y
                      ~width:net_image.width ~height:net_image.height
                      image#pixmap;
                    self#render_to_drawing pixmap;
                    background.pixmaps <- [pixmap];
                    ind := 0;
                    n := 0
                  end else
                    begin
                      pixmap#put_pixmap
                        ~x:net_image.x ~y:net_image.y
                        ~width:net_image.width ~height:net_image.height
                        image#pixmap;
                      self#render_to_drawing pixmap;
                      background.pixmaps <- [pixmap];
                      incr (n);
                      progress := 0
                    end
              end
          else begin
            pixmap#put_pixmap
              ~x:net_image.x ~y:net_image.y
              ~width:net_image.width ~height:net_image.height
              image#pixmap;
            self#fade !progress net_image pixmap true;
            self#render_to_drawing pixmap;
            background.pixmaps <- [pixmap]
          end
      ) in
      animation_timerID :=
        (Timeout.add ~ms:500
           ~callback:(fun _ ->
                        let net_image = List.nth li !n in
                        render_net net_image ();
                        progress := if !ind = 0 then 110 else (!progress + 20);
                        true))

    method anim_4 =
      let pixmap = List.nth background.pixmaps 0 in
      pixmap#set_foreground `WHITE;
      pixmap#rectangle ~filled:true ~x:0 ~y:0 ~width:256 ~height:256 ();
      for i = 0 to 6 do
        let col =
          if i <= 3 then
            gradual (0, 0, 255) (200, 200, 255) 3 i
            else gradual (200, 200, 255) (0, 0, 255) 4 (i - 4)
        in
        pixmap#set_foreground col;
        pixmap#arc
          ~x:(background.width / 2 - 103 + i) ~y:(background.height / 2 - 103 + i)
          ~width:(2 * (103 - i)) ~height:(2 * (103 - i)) ~filled:true ()
      done;
      pixmap#set_foreground `WHITE;
      pixmap#arc ~x:(background.width / 2 - 96) ~y:(background.height / 2 - 96) ~width:192 ~height:192 ~filled:true ();
      let li = Array.to_list img_gallery in
      List.iter (fun image ->
        let pix = List.nth image.pixmaps 0 in
        pixmap#put_pixmap
          ~x:image.x ~y:image.y
          ~width:image.width ~height:image.height
          pix#pixmap;
      ) li;
      self#render_to_drawing pixmap;
      background.pixmaps <- [pixmap];
      let li = List.tl li in
      let c = ref 0 in
      let swap_icons _ =
        if !c = 6 then
          begin
            Timeout.remove (!animation_timerID);
            List.iter (fun image ->
              let pix = List.nth image.pixmaps 0 in
              pixmap#put_pixmap
                ~x:image.x ~y:image.y
                ~width:image.width ~height:image.height
                pix#pixmap;
            ) li;
            self#render_to_drawing pixmap;
            background.pixmaps <- [pixmap];
            self#anim_5
          end else
            begin
              let ind = ref [] in
              for n = 0 to !c do
                ind := (Random.int 6)::!ind
              done;
              let n = ref 0 in
              List.iter (fun i ->
                let image1 = List.nth li i in
                let image2 = List.nth li !n in
                let (x, y) = (image2.x, image2.y) in
                image2.x <- image1.x;
                image2.y <- image1.y;
                image1.x <- x;
                image1.y <- y;
                incr (n)
              ) !ind;
              List.iter (fun image ->
                let pix = List.nth image.pixmaps 0 in
                pixmap#put_pixmap
                  ~x:image.x ~y:image.y
                  ~width:image.width ~height:image.height
                  pix#pixmap;
              ) li;
             self#render_to_drawing pixmap;
             background.pixmaps <- [pixmap];
           end
      in
      animation_timerID :=
        (Timeout.add ~ms:500
           ~callback:(fun _ ->
                        swap_icons ();
                        incr (c);
                        true))

    method anim_5 =
      let progress = ref 0 in
      let fade_all _ = (
        let pixmap = List.nth background.pixmaps 0 in
        if !progress >= 110 then
          begin
            Timeout.remove (!animation_timerID);
            self#render_to_drawing pixmap;
            background.pixmaps <- [pixmap];
            self#anim_1
          end
          else begin
            self#fade !progress background pixmap false;
            self#render_to_drawing pixmap;
            background.pixmaps <- [pixmap];
          end
      ) in
      animation_timerID :=
        (Timeout.add ~ms:500
           ~callback:(fun _ ->
                        fade_all ();
                        progress := !progress + 20;
                        true))



    method is_visible b =
      if b then
        self#anim_1
        else Timeout.remove (!animation_timerID)

    method coerce = box#coerce

    method box_bt = bt_box
    method box_dc = dc_box
    method box_nap = nap_box
    method box_ed2k = ed2k_box
    method box_ftt = ftt_box
    method box_gnut = gnut_box
    method box_slsk = slsk_box

    method update_style =
      Hashtbl.iter (fun num n ->
        try
          let (box, i) = retrieve_net_box n.net_name self in
          fill_box box i n.net_enabled n.net_displayed true
        with _ -> ()
      ) G.networks


    method clear =
      fill_box bt_box 0 false false false;
      fill_box dc_box 1 false false false;
      fill_box ftt_box 2 false false false;
      fill_box ed2k_box 3 false false false;
      fill_box gnut_box 4 false false false;
      fill_box nap_box 5 false false false;
      fill_box slsk_box 6 false false false
 
    initializer

    box#vbox_left#pack ftt_box#coerce;
    box#vbox_left#pack dc_box#coerce;
    box#vbox_left#pack bt_box#coerce;
    box#hbox#pack ed2k_box#coerce;
    box#vbox_right#pack gnut_box#coerce;
    box#vbox_right#pack nap_box#coerce;
    box#vbox_right#pack slsk_box#coerce;
    box#vbox_center#pack animation_frame#coerce ;


    ignore (dwg#event#connect#expose
      (
       fun _ ->
           let pixmap = List.nth background.pixmaps 0 in
           self#render_to_drawing pixmap;
           true
      )
    );

    ignore (fill_box bt_box 0 false false false);
    ignore (fill_box dc_box 1 false false false);
    ignore (fill_box ftt_box 2 false false false);
    ignore (fill_box ed2k_box 3 false false false);
    ignore (fill_box gnut_box 4 false false false);
    ignore (fill_box nap_box 5 false false false);
    ignore (fill_box slsk_box 6 false false false)

end

