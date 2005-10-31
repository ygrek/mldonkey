open GuiTypes2
open Printf2

type f = {mutable f : button_types -> unit}

external systray_create : GdkPixbuf.pixbuf -> string -> unit = "caml_systray_create"
external systray_set_icon : GdkPixbuf.pixbuf -> unit = "caml_systray_modify_icon"
external systray_set_tooltip : string -> unit = "caml_systray_modify_tooltip"
external systray_destroy : unit -> unit = "caml_systray_destroy"

let tray_create pixb s =
  systray_create pixb s;
  GuiGlobal.is_docked := true

let tray_set_icon = systray_set_icon
let tray_set_tooltip = systray_set_tooltip

let tray_destroy () =
  systray_destroy ();
  GuiGlobal.is_docked := false

let callback = {f = (fun ev -> 
             match ev with
               DOUBLE_CLICKED ->
               begin
                   lprintf "tray double clicked\n";
                   flush stdout;
                 end
             | RBUTTON_CLICKED ->
                 begin
                   lprintf "tray right clicked\n";
                   flush stdout;
                 end
             | _ -> ()
)}

let f ev = callback.f ev

let _ =
  GuiGlobal.set_systray_callback := (fun g -> callback.f <- g);
  Callback.register "on_systray_clicked" f;
  GuiGlobal.tray.create_tray <- tray_create;
  GuiGlobal.tray.set_icon_tray <- tray_set_icon;
  GuiGlobal.tray.set_tooltip_tray <- tray_set_tooltip;
  GuiGlobal.tray.destroy_tray <- tray_destroy
