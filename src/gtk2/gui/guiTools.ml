(* Copyright 2004 b8_bavard, INRIA *)
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

(* Tools of the GUI. *)

open GMain

module M = GuiMessages
module O = GuiOptions
module U = GuiUtf8
module A = GuiArt

let (!!) = Options.(!!)
let (=:=) = Options.(=:=)


let (<:>) w = (w :> GContainer.container)

let verbose = O.gtk_verbose_tools

let lprintf' fmt =
  Printf2.lprintf ("GuiTools: " ^^ fmt)

(**********************************************************************************)
(*                                                                                *)
(*                  Miscellaneous functions for a paned area                      *)
(*                                                                                *)
(**********************************************************************************)

let set_hpaned (hpaned : GPack.paned) prop =
  let w = float_of_int (Gdk.Screen.width ()) in
  let width = int_of_float (w *. !!prop) in
  hpaned#child1#misc#set_size_request ~width ()

let set_vpaned (vpaned : GPack.paned) prop =
  let h = float_of_int (Gdk.Screen.height ()) in
  let height = int_of_float (h *. !!prop) in
  vpaned#child1#misc#set_size_request ~height ()

let get_hpaned (hpaned: GPack.paned) prop =
  ignore (hpaned#child1#coerce#misc#connect#size_allocate
      ~callback: (fun r ->
        let w = float_of_int (Gdk.Screen.width ()) in
        prop =:= float_of_int r.Gtk.width /. w
    ))

let get_vpaned (vpaned: GPack.paned) prop =
  ignore (vpaned#child1#coerce#misc#connect#size_allocate
      ~callback: (fun r ->
        let h = float_of_int (Gdk.Screen.height ()) in
        prop =:= float_of_int r.Gtk.height /. h
    ))

let remove_key ~target ~sign =
    ignore (target#misc#disconnect sign)

let add_key ~key ~target ~f ?(mods = [`MOD1]) ?source () =
  let sign =
    target#event#connect#key_press
      ~callback:(fun ev ->
        if GdkEvent.Key.keyval ev = key && GdkEvent.Key.state ev = mods
          then begin
            f (); 
            true 
          end else false
    )
  in
  Gaux.may ~f:(fun w -> 
    ignore (w#misc#connect#destroy 
      ~callback:(fun _ -> remove_key ~target ~sign)
  )) source


(* **
   [get_access_key_from ~markup] parses the string [markup] and
   an underline in [markup] indicates the next character should be
   used for the mnemonic accelerator key.

   @param char, specifies which character shall used when parsing [markup]
   instead of an underline.

*)

let get_access_key_from ~markup ?(char=0x05f) () =
  try
    let chr = Char.chr char in
    let pos = String.index markup chr in
    let c = String.get markup (pos + 1) in
    let access_key = Char.code (Char.lowercase c) in
    Some (access_key : Gdk.keysym)
  with
    Not_found -> None


(* **
   [add_label ~cont ~markup] add a label to the widget [cont].
   Parses [markup] which is marked up with the Pango text markup 
   language, setting the label's text and attribute list based on
   the parse results.
   Especially using an underline in [markup] indicates the next
   character should be used for the mnemonic accelerator key. The 
   mnemonic accelerator key is automatically associated with the
   widget [cont]. Returns the label.

   @param mnemonic_widget, specifies the given widget [mnemonic_widget]
   that will be accelerated by mnemonic accelerator key of the label
   instead of [cont].
   Can be usefull when the target is a GtkEntry next to the label.

*)

let add_label ~cont ~markup ?mnemonic_widget () =
  let label = 
    GMisc.label ~markup ~use_underline:true 
      ?mnemonic_widget ()
  in
  cont#add label#coerce;
  label



(* **
   [add_pixbuf ~cont ~pixpuf] add an image to the widget [cont].
   Returns the image.

*)

let add_pixbuf ~cont ~pixbuf =
  let image = GMisc.image ~pixbuf () in
  cont#add image#coerce;
  image



(* **
   [add_complex_box ~cont ~style] add a GPack.vbox or a GPack.hbox
   according to [style] to the widget [cont].

   @param markup, add a label to the widget [cont] which is
   marked up with the Pango text markup language, setting the
   label's text and attribute list based on the parse results.
   Especially using an underline in [markup] indicates the next
   character should be used for the mnemonic accelerator key. The 
   mnemonic accelerator key is automatically associated with the
   widget [cont].
   The default handler for this signal will activate the widget
   [cont] if there are no mnemonic collisions and toggle focus
   between the colliding widgets otherwise.

   @param mnemonic_widget, specifies the given widget [mnemonic_widget]
   that will be accelerated by the mnemonic accelerator key of 
   the label instead of [cont].

   @param icon, add an image based on the [icon] to the widget [cont].

*)

let fs cont = (fun m -> add_label ~cont ~markup:m ())

let fp cont = (fun i -> add_pixbuf ~cont ~pixbuf:i)

let get_parent_window ~widget =
  GWindow.toplevel widget

let accel_ ~source ~markup ~f ~target =
  let key = get_access_key_from ~markup () in
  Gaux.may ~f:(fun k -> add_key ~key:k ~target ~f ~source ()) key

let _accel ~source ~markup ~f =
  let target = get_parent_window ~widget:source in
  Gaux.may ~f:(fun t -> accel_ ~source ~markup ~f ~target:t) target


let add_complex_box ~cont ~style ?markup ?icon () =
  match style with
      `ICONS ->
        begin
          Gaux.may ~f:(fp cont) icon;
          let default_callback = fun _ -> ignore (cont#misc#activate ()) in
          Gaux.may ~f:(fun m -> _accel ~source:(cont#coerce) ~markup:m ~f:default_callback) markup
        end
    | `TEXT ->
        begin
          Gaux.may ~f:(fs cont) markup
        end
    | `BOTH ->
        begin
          let box =
            GPack.vbox ~homogeneous:false 
              ~spacing:2 ~packing:cont#add ()
          in
          Gaux.may ~f:(fp box) icon;
          Gaux.may ~f:(fs box) markup
        end
    | `BOTH_HORIZ ->
        begin
          let box =
            GPack.hbox ~homogeneous:false 
              ~packing:cont#add ()
          in
          Gaux.may ~f:(fp box) icon;
          Gaux.may ~f:(fs box) markup
        end

(* **
   [tool_bar dir] add a vertical or horiontal toolbar
   according to [dir].

   tool_bar inherit GPack.button_box with 3 new methods
     add_toggle_button
     add_button
     empty

*)

type b_children =
  {
   oid : int;
   button : GButton.toggle_button;
   mutable active : bool;
 }

class tool_bar obj =
  object (self)
    inherit GPack.box obj

    val mutable bb_children = ([] : b_children list)

    method add_toggle_button ~(style:Gtk.Tags.toolbar_style) 
      ?(markup:string option) ?(icon:GdkPixbuf.pixbuf option) ?(f = fun () -> ()) () =
      let t_button = GButton.toggle_button ~packing:self#add () in
      bb_children <- {oid = t_button#get_oid; button = t_button; active = t_button#active} :: bb_children;
      add_complex_box ~cont:((<:>) t_button) ~style ?markup ?icon ();
      ignore (t_button#connect#toggled
        (fun _ ->
          let button = List.find (fun w -> w.oid = t_button#get_oid) bb_children in
          if t_button#active then
            begin
              if not button.active
                then begin
                  let buttons = List.filter (fun w  -> w.oid <> t_button#get_oid) bb_children in
                  List.iter (fun w -> (w.active <- false; w.button#set_active false)) buttons;
                  button.active <- true;
                  f ()
                end
            end else begin
              if button.active then t_button#set_active true
            end
      ));
      ignore (t_button#misc#connect#destroy ~callback:
        (fun _ ->
          bb_children <- List.filter (fun w -> w.oid <> t_button#get_oid) bb_children
      ));
      t_button

    method add_button ~(style:Gtk.Tags.toolbar_style) 
      ?(markup:string option) ?(icon:GdkPixbuf.pixbuf option) ?(f = fun () -> ()) () =
      let button = GButton.button ~packing:self#add () in
      add_complex_box ~cont:((<:>) button) ~style ?markup ?icon ();
      ignore (button#connect#clicked
        (fun _ -> f ()
      ));
      button

    method empty () =
      List.iter (fun w -> w#destroy ()) self#children


  end

let tool_bar dir ?spacing ?child_width ?child_height ?child_ipadx ?child_ipady
                 ?layout ?width ?height ?border_width ?packing ?show () =
  let bbox = GPack.button_box dir ?spacing ?child_width ?child_height
                              ?child_ipadx ?child_ipady ?layout ?width ?height
                              ?border_width ?packing ?show ()
  in
  let w = Gobject.try_cast bbox#as_widget  "GtkButtonBox" in
  new tool_bar w


(* **
   [popup_progress ~label ~items ~f ~step] displays a popup window
   with a label [label] and a progress bar.
   the progress displayed corresponds to the progress of the
   function [f] applied to items list [items].
   
*)

let timerID = ref (Timeout.add ~ms:1000 ~callback:(fun _ -> true))

let spercent p =
  let v = int_of_float (p *. 100.) in
  U.simple_utf8_of (Printf.sprintf "%d%%" v)

let set_progress p pbar =
  let p = 
    if p >= 1. 
      then 1. 
      else p
  in
  pbar#set_fraction p;
  pbar#set_text (spercent p)

let rec iter (step : int) (l : 'a list) (li : 'a list list) =
  let le = List.length l in
  if le <= step then
    begin
      let li = l::li in
      li
    end
    else begin
      let lis = Array.to_list (Array.sub (Array.of_list l) 0 step) in
      let li = lis::li in
      iter step (Array.to_list (Array.sub (Array.of_list l) step (le - step))) li
    end

let popup_progress ~label ~items ~f ~step =
  let window = 
    GWindow.window ~border_width:10 ~kind:`POPUP
      ~position:`CENTER_ALWAYS ()
  in
  let vbox =
    GPack.vbox ~homogeneous:true ~packing:window#add ()
  in
  let wlabel =
    GMisc.label ~packing:(vbox#pack ~fill:true ~expand:false) ()
  in
  let pbar =
    GRange.progress_bar ~pulse_step:0.1
      ~packing:(vbox#pack ~fill:true ~expand:true) ()
  in

  wlabel#set_text (U.simple_utf8_of label);
  set_progress 0. pbar;
  window#show ();

  let len = List.length items in
  let new_l = iter step items [] in
  let c = ref 0 in
  let count = ref 0 in
  let flush_list _ =
    let li = List.nth new_l !c in
    if !c = (List.length new_l - 1) then
      begin
        List.iter f li;
        Timeout.remove (!timerID);
        window#destroy ()
      end else
      begin
        List.iter f li;
        incr (c)
      end
  in
  timerID :=
    (Timeout.add ~ms:50
           ~callback:(fun _ ->
             count := !count + List.length (List.nth new_l !c);
             flush_list ();
             set_progress ((float_of_int !count) /. (float_of_int len +. 1.)) pbar;
             true))

(* **
   [create_bold_markup s] returns a string which is
   marked up with the Pango text markup language, setting the
   label's text as bold with a blue color, and converted
   to the utf8 format.

*)

let create_bold_markup s =
  let str = U.utf8_of s in
  Printf.sprintf "<span foreground=\"%s\" weight=\"bold\">%s</span>" 
    "#0000FF" str

let create_default_bold_markup s = 
  Printf.sprintf "<span foreground=\"%s\" weight=\"bold\">%s</span>" 
      !!O.gtk_color_default s

(* **
   [create_markup s] returns a string which is
   marked up with the Pango text markup language, setting the
   label's text with the default color as set in the options,
   and converted to the utf8 format.

*)

let create_markup s =
  let str = U.utf8_of s in
  Printf.sprintf "<span foreground=\"%s\">%s</span>" 
    !!O.gtk_color_default str

(* **
   [color_of_name s] returns a string of type GDraw.color.

*)

let color_of_name name =
  let name = Glib.Utf8.to_unistring name in
  let len = Array.length name in
  let accs = [| ref 0 ; ref 0 ; ref 0 |] in
  for i = 0 to (len - 1) do
    let m = i mod 3 in
    accs.(m) := !(accs.(m)) + name.(i)
  done;
  let r = !(accs.(0)) mod 210 in
  let g = !(accs.(1)) mod 210 in
  let b = !(accs.(2)) mod 210 in
  let s = Printf.sprintf "#%02X%02X%02X" r g b in
  s

(* **
   [fit_string_to_pixels s ~context ~pixels] returns a new string
   from [s] that fits the given width [pixels] for the given pango [context].

*)

let fit_string_to_pixels s ~context ~pixels =
  let pango_layout = Pango.Layout.create context#as_context in
  Pango.Layout.set_text pango_layout s;
  let s' =
    if fst (Pango.Layout.get_pixel_size pango_layout) <= pixels
    then s
    else begin
      let len = String.length s in
      let ext = "..." ^ (String.sub s (len - 7) 7) in
      let rec adjust_string str n len =
        if n = len
          then str
          else begin
            let str' = U.utf8_of ((String.sub str 0 n) ^ ext) in
            Pango.Layout.set_text pango_layout str';
            let str_pixels = fst (Pango.Layout.get_pixel_size pango_layout) in
            (* Printf.printf "width %d - Pango.Layout.get_pixel_size %d Pango.Layout.get_text %s\n" 
               pixels s_width (Pango.Layout.get_text pango_layout);
            flush stdout; *)
            if str_pixels < pixels
              then adjust_string str (n + 1) len
              else str'
          end
      in
      adjust_string s 0 len
    end
  in s'



let warning_box
  ~text
  ?(title = !M.mW_lb_warning)
  ?(icon = (A.get_icon ~icon:M.icon_stock_warning ~size:A.SMALL ()))
  ?(on_ok = fun () -> ()) () =
  let window =
    GWindow.window
      ~title
      ~icon
      ~position:`CENTER_ALWAYS
      ~kind:`TOPLEVEL
      ~resizable:true ~modal:false ()
  in
  window#set_skip_taskbar_hint false;
  window#set_skip_pager_hint false;
  let vbox =
    GPack.vbox ~homogeneous:false
      ~packing:window#add ()
  in
  let hbox =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let pixbuf = A.get_icon ~icon:M.icon_stock_warning ~size:A.LARGE () in
  let image =
    GMisc.image ~pixbuf 
      ~packing:(hbox#pack ~expand:false ~fill:true) ()
  in
  let w_vbox =
    GPack.hbox ~homogeneous:false ~border_width:3
       ~packing:(hbox#pack ~expand:true ~fill:true) ()
  in
  let w_label =
    GMisc.label ~xalign:0.5 ~yalign:0.5 ~text
      ~packing:(w_vbox#pack ~expand:true ~fill:true) ()
  in
  let separator =
    GMisc.separator `HORIZONTAL
    ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let hbox_button =
    GPack.hbox ~homogeneous:false ~border_width:6
      ~packing:(vbox#pack ~expand:false ~fill:true) ()
  in
  let bbox = 
    tool_bar `HORIZONTAL ~border_width:6 ~spacing:6
      ~layout:`END ~packing:hbox_button#add ()
  in
  ignore (bbox#add_button
    ~markup:!M.pW_lb_cancel
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_close ~size:A.SMALL ())
    ~f:(window#destroy) ()
  );
  let f () =
    on_ok ();
    window#destroy ()
  in
  ignore (bbox#add_button 
    ~markup:!M.pW_lb_ok
    ~style:`BOTH_HORIZ
    ~icon:(A.get_icon ~icon:M.icon_stock_ok ~size:A.SMALL ())
    ~f ()
  );
  window#show ()
