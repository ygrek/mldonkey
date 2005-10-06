
open Gettext
open GMain

let _s x = _s "Gui_starter" x
let _b x = _b "Gui_starter" x

let home_dir = (try Sys.getenv "HOME" with _ -> ".")
let config_dir_basename =
  if Autoconf.windows then "mldonkey" else ".mldonkey"
let config_dir = Filename.concat home_dir config_dir_basename
  
let _ = 
  let filename = Filename.concat config_dir "gui_starter_strings" in
  Unix2.safe_mkdir (Filename.dirname filename);
  set_strings_file filename

let main () =
  ignore (GMain.Main.init ());
  let window = GWindow.window ~title:(_s "MLdonkey GUI starter") ~border_width:10
    ~width:300 ~height:150 () in
  let vbox_2 = GPack.vbox ~homogeneous:false ~packing: window#add ()in
  let wl_3 =
    GMisc.label ~text: (_s "\n     Start the GTK gui ?     \n\nChange the 'ask_for_gui' option to false to remove this message.\n\n") ~justify:`CENTER ~line_wrap:true
      ~xalign:(-1.0) ~yalign:(-1.0)
      ~packing:(vbox_2#pack ~expand:true ~fill:true) ()
  in
  let hbox_4 =
    GPack.hbox ~homogeneous:true
      ~packing:(vbox_2#pack ~expand:false ~fill:true) ()
  in
  let wb_5 =
    GButton.button ~packing:(hbox_4#pack ~expand:true ~fill:true) ()
  in
  let wl_7 =
    GMisc.label ~text:(_s "YES") ~justify:`CENTER ~line_wrap:true ~xalign:(-1.0)
      ~yalign:(-1.0) ~packing:(wb_5#add) ()
  in
  let wb_6 =
    GButton.button ~packing:(hbox_4#pack ~expand:true ~fill:true) ()
  in
  let wl_8 =
    GMisc.label ~text:(_s "NO") ~justify:`CENTER ~line_wrap:true ~xalign:(-1.0)
      ~yalign:(-1.0) ~packing:(wb_6#add) ()
  in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  ignore (wb_5#connect#clicked ~callback:
  (fun () -> 
      ignore (Sys.command (Printf.sprintf "%s &" Sys.argv.(1)));
      GMain.Main.quit ();
      ));
  ignore (wb_6#connect#clicked ~callback:
  (fun () -> 
      GMain.Main.quit ();
  ));
  window#show ();
  GMain.Main.main ()

let _ = main ()
  