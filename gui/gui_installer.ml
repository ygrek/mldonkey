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

open Options
open GMain

(* What we want ? 

* In which directory should the .ini files be stored ?
* Which directories should the temp/ and incoming/ be ?
* Which other directories are shared ?
* Which connection link is used ?
* Security option ?

All these replies should be stored in a basic file ~/.mldonkey_install.ini
that should also be loaded at startup by mldonkey.
  
*)

  
  
let home_dir = (try Sys.getenv "HOME" with _ -> ".")
  
let installer_name = Filename.concat home_dir ".mldonkey_installer.ini"
  
let installer_ini = create_options_file installer_name
  
let mldonkey_directory = 
  define_option installer_ini ["mldonkey_directory"] 
    "The directory where mldonkey's option files are" string_option
    (Filename.concat home_dir ".mldonkey")

let _ =
  (try Options.load installer_ini with _ -> ())
  
module Config = struct
    open Configwin_types
    open Configwin_ihm
    
    
    let box param_list =
      let main_box = GPack.vbox  () in
      let f parameter =
        match parameter with
          String_param p ->
            let box = new string_param_box p in
            let _ = main_box#pack ~expand: false ~padding: 2 box#box in
            box
        | Combo_param p ->
            let box = new combo_param_box p in
            let _ = main_box#pack ~expand: false ~padding: 2 box#box in
            box
        | Text_param p ->
            let box = new text_param_box p in
            let _ = main_box#pack ~expand: p.string_expand ~padding: 2 box#box in
            box
        | Bool_param p ->
            let box = new bool_param_box p in
            let _ = main_box#pack ~expand: false ~padding: 2 box#box in
            box
        | Filename_param p ->
            let box = new filename_param_box p in
            let _ = main_box#pack ~expand: false ~padding: 2 box#box in
            box
        | List_param f ->
            let box = f () in
            let _ = main_box#pack ~expand: true ~padding: 2 box#box in
            box
        | Custom_param p ->
            let box = new custom_param_box p in
            let _ = main_box#pack ~expand: p.custom_expand ~padding: 2 box#box in
            box
        | Color_param p ->
            let box = new color_param_box p in
            let _ = main_box#pack ~expand: false ~padding: 2 box#box in
            box
        | Font_param p ->
            let box = new font_param_box p in
            let _ = main_box#pack ~expand: false ~padding: 2 box#box in
            box
        | Date_param p ->
            let box = new date_param_box p in
            let _ = main_box#pack ~expand: false ~padding: 2 box#box in
            box
      in
      let list_param_box = List.map f param_list in
      let f_apply () = 
        List.iter (fun param_box -> param_box#apply) list_param_box 
      in
      
      main_box, f_apply
  end
  
  
class tab (notebook : GPack.notebook) label =
  let tab_label =  GMisc.label ~text: label () in
  let vbox = GPack.vbox () in
  let main_hbox = GPack.hbox ~packing: 
    (vbox#pack ~fill: true ~expand: true) () in
  let main_vbox = GPack.vbox ~packing: 
    (main_hbox#pack ~fill: true ~expand: true) () in

  let button_hbox = GPack.hbox ~packing: 
    (vbox#pack ~expand: false ~padding: 10) () in

  
  let previous_slide = GButton.button ~width: 100 ~label: "Previous" () 
    ~packing: (button_hbox#pack ~padding: 10) in

  let cancel = GButton.button ~label: "Cancel" () 
    ~packing: (button_hbox#pack ~padding: 10) in
  
  let next_slide = GButton.button ~width: 100 ~label: "Next" () 
    ~packing: (button_hbox#pack ~padding: 10) in
  
  object(self)
  
    method vbox = main_vbox
    
    initializer
      notebook#append_page  ~tab_label: tab_label#coerce vbox#coerce ;
      ignore (next_slide#connect#clicked
          (fun () ->  notebook#next_page ()));
      ignore (previous_slide#connect#clicked
          (fun () ->  notebook#previous_page ()));
      ignore (cancel#connect#clicked
          (fun () ->  exit 2));

end


let create_downloads_ini () =
  
  let filename = Filename.concat !!mldonkey_directory "downloads.ini" in
  let downloads_ini = create_options_file filename in
  
  let shared_directories = 
    define_option downloads_ini ["shared_directories" ] 
      "Directories where files will be shared"
      (list_option string_option) []
  in
        
  let gui_port = 
    define_option downloads_ini ["gui_port"] "port for user interaction" int_option 4001
    in 
  
  let http_port = 
    define_option downloads_ini ["http_port"] "The port used to connect to your client with a WEB browser" int_option 4080
    in
  
  let telnet_port = define_option downloads_ini ["telnet_port"] "port for user interaction" int_option 4000
    in
  
  let http_login = 
    define_option downloads_ini ["http_login"] "Your login when using a WEB browser" string_option ""
    in
  
  let http_password = 
    define_option downloads_ini ["http_password"] "Your password when using a WEB browser" string_option ""
    in
  
  let max_hard_upload_rate = define_option downloads_ini ["max_hard_upload_rate"] 
    "The maximal upload rate you can tolerate on your link in kBytes/s (0 = no limit)
    The limit will apply on all your connections (clients and servers) and both
    control and data messages." int_option 0
    in
  
  let max_hard_download_rate = define_option downloads_ini ["max_hard_download_rate"] 
    "The maximal download rate you can tolerate on your link in kBytes/s (0 = no limit)
    The limit will apply on all your connections (clients and servers) and both
    control and data messages." int_option 0
    in
  
  let password = define_option downloads_ini ["password"] 
    "The password to access your client from the GUI (setting it disables
    the command-line client)" string_option ""
    in
  
  let allowed_ips = define_option downloads_ini ["allowed_ips"]
    "list of IP address allowed to control the client via telnet/GUI/WEB"
    (list_option Ip.option) [Ip.of_string "127.0.0.1"]
    in
  
  Options.save_with_help downloads_ini;
  lprintf "%s created successfully" filename; lprint_newline () 

let main () =
  let gui = new Gui_installer_base.window () in
  let window = gui#window in
  let notebook = gui#notebook in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  
  let directories_tab = new tab notebook "Directories" in
  
  
  let param = Configwin_ihm.filename "MLdonkey directory:" !!mldonkey_directory
  in
  let directories_box, directories_apply = Config.box [param] in
  directories_tab#vbox#pack ~expand: true ~fill: true directories_box#coerce;
  
  let connection_tab = new tab notebook "Connection" in
  let security_tab = new tab notebook "Interfaces" in
  
  let save_tab = new tab notebook "Save" in

  
  let label = GMisc.label ~text:
    "Click on the Save button to save this configuration.\n Be careful: it will erase any previous configuration."
    ~justify:`CENTER
      ~packing:(save_tab#vbox#pack  ~expand: true ~fill: true ~padding: 100) () in
  
  let save_button = GButton.button ~height: 30 ~width: 100 ~label: "Save" () 
    ~packing: (save_tab#vbox#pack ~expand: true ~fill: true ~padding: 10) in
  ignore (save_button#connect#clicked
      (fun () -> 
        directories_apply ();        

        
        Unix2.safe_mkdir !!mldonkey_directory;
        create_downloads_ini ();
        Options.save_with_help installer_ini;
        lprintf "%s created successfully" installer_name; lprint_newline ();
        
        exit 0));

  
  window#show ();
  GMain.Main.main ()

let _ = 
  main ()
  