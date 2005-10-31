
open Gettext
open GMain
open GuiTypes2

module M = GuiMessages
module A = GuiArt

let message_file_name =
  try
    Sys.getenv "MLGUI_STRINGS"
  with _ -> 
     let filename = 
       Filename.concat M.lang_dir "mlgui_strings"
     in
     Unix2.safe_mkdir (Filename.dirname filename);
     filename

let _s x = GuiUtf8.simple_utf8_of (_s "GuiStarter" x)
let _b x = _b "GuiStarter" x  

let _s_ x = (_s x) ^ ":"

let main () =
  ignore (GMain.Main.init ());
  let window = GWindow.window 
    ~title:(_s "MLdonkey GUI starter")
    ~icon:(A.get_icon ~icon:M.icon_type_source_normal ~size:A.SMALL ())
    ~border_width:10
    ~width:300 ~height:150 ()
  in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  let vbox_2 = GPack.vbox ~homogeneous:false ~packing: window#add ()in
  let wl_3 =
    GMisc.label ~text:(_s "\n     Start the GTK gui ?     \n\nChange the 'ask_for_gui' option to false to remove this message.\n\n") ~justify:`CENTER ~line_wrap:true
      ~xalign:0.5 ~yalign:0.5
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
    GMisc.label ~text:(_s "YES") ~justify:`CENTER ~line_wrap:true ~xalign:0.5
      ~yalign:0.5 ~packing:(wb_5#add) ()
  in
  let wb_6 =
    GButton.button ~packing:(hbox_4#pack ~expand:true ~fill:true) ()
  in
  let wl_8 =
    GMisc.label ~text:(_s "NO") ~justify:`CENTER ~line_wrap:true ~xalign:0.5
      ~yalign:0.5 ~packing:(wb_6#add) ()
  in
  ignore (wb_5#connect#clicked ~callback:
  (fun () -> 
      window#destroy ();
      ignore (Sys.command (Printf.sprintf "%s &" Sys.argv.(1)));
      ));
  ignore (wb_6#connect#clicked ~callback:
  (fun () -> 
      window#destroy ();
  ));
  window#show ();
  GMain.Main.main ()

let _ = main ()
  
