
open GMain

let main () =
  ignore (GMain.Main.init ());
  let window = GWindow.window ~title:"MLdonkey GUI starter" ~border_width:10
    ~width:300 ~height:150 () in
  let vbox_2 = GPack.vbox ~homogeneous:false ~packing: window#add ()in
  let wl_3 =
    GMisc.label ~text:"\n     Start the GTK gui ?     \n\nChange the 'ask_for_gui' option to false to remove this message.\n\n" ~justify:`CENTER ~line_wrap:true
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
    GMisc.label ~text:"YES" ~justify:`CENTER ~line_wrap:true ~xalign:(-1.0)
      ~yalign:(-1.0) ~packing:(wb_5#add) ()
  in
  let wb_6 =
    GButton.button ~packing:(hbox_4#pack ~expand:true ~fill:true) ()
  in
  let wl_8 =
    GMisc.label ~text:"NO" ~justify:`CENTER ~line_wrap:true ~xalign:(-1.0)
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
  