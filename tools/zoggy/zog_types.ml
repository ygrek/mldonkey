(***********************************************************************)
(*                               Zoggy                                 *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Types for gui description. *)

(** {2 Properties} *)

(** Different kinds of properties. *)
type prop_kind =
  | Function
  | Expand
  | Fill
  | Width
  | Height
  | Border_width
  | Title
  | Allow_shrink
  | Allow_grow
  | Auto_shrink
  | X_pos
  | Y_pos
  | PLabel
  | Padding
  | Group
  | Orientation
  | Toolbar_style
  | Toolbar_space_size
  | Toolbar_space_style
  | Tooltips
  | Button_relief_style
  | Spacing
  | Homogeneous
  | Button_box_style
  | Child_width 
  | Child_height
  | Child_ipadx
  | Child_ipady
  | Label_xalign
  | Label_yalign
  | Shadow_type
  | Obey_child
  | Ratio
  | Hscrollbar_policy
  | Vscrollbar_policy
  | Handle_position
  | Snap_edge
  | Column_titles
  | Show_titles
  | X_align
  | Y_align
  | X_pad
  | Y_pad
  | PText
  | Line_wrap
  | Tab_pos
  | Show_tabs
  | Homogeneous_tabs
  | Show_border
  | Scrollable
  | Tab_border
  | Popup
  | SBUpdate_policy
  | Visibility
  | Editable
  | Use_arrows
  | Case_sensitive
  | Word_wrap
  | Column_number
  | Draw_indicator
  | Active
  | Placement
  | Selection_mode
  | Justification
  | Max_length
  | View_mode
  | View_lines
  | Handle_size
  | Modal
  | Tab_label

  | Accel_group_name
  | Accel_modifier
  | Accel_flags
  | Accel_keysym
  | Show_toggle
  | Show_indicator
  | Right_justify

  | Arrow_type
  | Calendar_options
  | Popdown_strings
  | Value_in_list
  | Ok_if_empty
  | Update_policy

  | PPixmap_file
  | PPixmap_data
  | PPixmap_code

(*
open Gtk.Tags

type tag = [ 
    orientation | toolbar_style | relief_style | button_box_style
  | shadow_type | policy_type | position | update_type | corner_type
  | selection_mode | justification | Gdk.Tags.modifier | accel_flag
  | arrow_type
  | calendar_display_options
  | `EMPTY | `LINE | `ITEM | `NEVER | `DEFAULT | `ALWAYS |`IF_VALID
  ]
*)

type tag =
  [ `HORIZONTAL | `VERTICAL | `ICONS | `TEXT | `BOTH | `NORMAL | `HALF
  | `NONE | `DEFAULT_STYLE | `SPREAD | `EDGE | `START | (*`END | `IN |*) `OUT
  | `ETCHED_IN | `ETCHED_OUT | `ALWAYS | `AUTOMATIC | `NEVER | `LEFT
  | `RIGHT | `TOP | `BOTTOM | `CONTINUOUS | `DISCONTINUOUS | `DELAYED
  | `TOP_LEFT | `BOTTOM_LEFT | `TOP_RIGHT | `BOTTOM_RIGHT | `SINGLE
  | `BROWSE | `MULTIPLE | `EXTENDED | `CENTER | `FILL | `SHIFT | `LOCK
  | `CONTROL | `MOD1 | `MOD2 | `MOD3 | `MOD4 | `MOD5 | `BUTTON1 | `BUTTON2
  | `BUTTON3 | `BUTTON4 | `BUTTON5 | `VISIBLE | `SIGNAL_VISIBLE | `LOCKED
  | `UP | `DOWN | `SHOW_HEADING | `SHOW_DAY_NAMES | `NO_MONTH_CHANGE
  | `SHOW_WEEK_NUMBERS | `WEEK_START_MONDAY | `EMPTY | `LINE | `ITEM
  | `DEFAULT | `IF_VALID ]

(** Different kinds of values for a property. *)
type prop_values =
  | Bool (** true, false or some code which evaluates to a boolean *)
  | PosInt (** a positive int, empty string means no value *)
  | Float (** a float, empty string means no value *)
  | Code (** empty string means no value *)
  | Enum of (string * tag) list (** one value among predefined values *)
  | Enum_list of (string * tag list) list (** values among predefined values *)
  | Code_list
  | Keysym 

(** A widget property. *)
type property = {
    prop_kind : prop_kind ; (** property kind *)
    mutable prop_value : string ; (** ocaml code of the value *)
    prop_value_loc : Camlp4.PreCast.Loc.t ; (** source location of the value *)
  } 

let gt s = "Gtk.Tags."^s
let gd s = "Gdk.Tags."^s


let orientation_values = [ "`HORIZONTAL", `HORIZONTAL; "`VERTICAL", `VERTICAL ]
let toolbar_style_values = [ "`ICONS", `ICONS; "`TEXT", `TEXT; "`BOTH", `BOTH ]
let toolbar_space_values = [ "`EMPTY", `EMPTY; "`LINE", `LINE ]
let button_relief_values = [ "`NORMAL", `NORMAL; "`HALF", `HALF; "`NONE", `NONE ]
let button_box_style_values = [ "`DEFAULT_STYLE", `DEFAULT_STYLE; 
                                "`SPREAD", `SPREAD; "`EDGE", `EDGE;
                                "`START", `START; (*"`END", `END*) ]
let shadow_type_values = [ "`NONE", `NONE; (*"`IN", `IN;*) "`OUT", `OUT;
                           "`ETCHED_IN", `ETCHED_IN; "`ETCHED_OUT", `ETCHED_OUT ]
let policy_type_values = [ "`AUTOMATIC", `AUTOMATIC ; "`ALWAYS", `ALWAYS ]
let position_values = [ "`LEFT", `LEFT; "`RIGHT", `RIGHT; "`TOP", `TOP; "`BOTTOM", `BOTTOM ]
let update_type_values =  [ "`CONTINUOUS", `CONTINUOUS; "`DISCONTINUOUS", `DISCONTINUOUS;
                            "`DELAYED", `DELAYED ]
let corner_type_values = [ "`TOP_LEFT", `TOP_LEFT ; "`BOTTOM_LEFT" , `BOTTOM_LEFT ;
                           "`TOP_RIGHT", `TOP_RIGHT ; "`BOTTOM_RIGHT", `BOTTOM_RIGHT ]

let selection_mode_values = [ "`SINGLE", `SINGLE ; "`BROWSE", `BROWSE ;
                              "`MULTIPLE", `MULTIPLE ; "`EXTENDED", `EXTENDED ]

let justification_values = [ "`LEFT", `LEFT ; "`RIGHT", `RIGHT ;
                             "`CENTER", `CENTER ; "`FILL", `FILL ]

let view_mode_values = [ "`LINE", `LINE ; "`ITEM", `ITEM ]
let use_arrows_values = [ "`DEFAULT", `DEFAULT ; "`NEVER",`NEVER ; "`ALWAYS" ,`ALWAYS]
let sb_update_type_values = [ "`IF_VALID", `IF_VALID ; "`ALWAYS", `ALWAYS ]
let modifier_values = [ "`SHIFT", `SHIFT ; "`LOCK", `LOCK ;
                        "`CONTROL", `CONTROL ;"`MOD1", `MOD1 ;
                        "`MOD2", `MOD2 ; "`MOD3", `MOD3 ; "`MOD4", `MOD4 ;
                        "`MOD5", `MOD5 ; "`BUTTON1", `BUTTON1 ; "`BUTTON2" ,`BUTTON2 ;
                        "`BUTTON3", `BUTTON3 ; "`BUTTON4", `BUTTON4 ; "`BUTTON5", `BUTTON5 ]
let accel_flag_values = [ "`VISIBLE", `VISIBLE ; "`SIGNAL_VISIBLE", `SIGNAL_VISIBLE ;
                          "`LOCKED", `LOCKED ]
let arrow_type_values = ["`UP", `UP; "`DOWN", `DOWN ; 
                          "`LEFT", `LEFT ; "`RIGHT", `RIGHT ]
let calendar_options_values = 
  [
    "[`SHOW_HEADING;`SHOW_DAY_NAMES;`NO_MONTH_CHANGE;`SHOW_WEEK_NUMBERS;`WEEK_START_MONDAY]",
    [`SHOW_HEADING;`SHOW_DAY_NAMES;`NO_MONTH_CHANGE;`SHOW_WEEK_NUMBERS;`WEEK_START_MONDAY] ;

    "`SHOW_HEADING", [`SHOW_HEADING] ;
    "`SHOW_DAY_NAMES", [`SHOW_DAY_NAMES] ;
    "`NO_MONTH_CHANGE", [`NO_MONTH_CHANGE] ;
    "`SHOW_WEEK_NUMBERS", [`SHOW_WEEK_NUMBERS] ;
    "`WEEK_START_MONDAY", [`WEEK_START_MONDAY] ;

  ] 
let accel_flag_values =
  [ 
    "[`VISIBLE ; `LOCKED]", [`VISIBLE ; `LOCKED] ;
    "[]", [] ;
    "[`VISIBLE]", [`VISIBLE] ;
    "[`SIGNAL_VISIBLE]", [`SIGNAL_VISIBLE] ;
    "[`LOCKED]", [`LOCKED] ;
    "[`VISIBLE ; `SIGNAL_VISIBLE; `LOCKED]", [`VISIBLE ; `SIGNAL_VISIBLE ; `LOCKED]
  ]
let modifier_values = [
  "[`CONTROL]", [`CONTROL] ;
  "[`SHIFT]", [`SHIFT] ;
  "[`LOCK]", [`LOCK] ;
  "[`MOD1]", [`MOD1] ;
  "[`MOD2]", [`MOD2] ;
  "[`MOD3]", [`MOD3] ;
  "[`MOD4]", [`MOD4] ;
  "[`MOD5]", [`MOD5] ;
  "[`BUTTON1]", [`BUTTON1] ;
  "[`BUTTON2]", [`BUTTON2] ;
  "[`BUTTON3]", [`BUTTON3] ;
  "[`BUTTON4]", [`BUTTON4] ;
  "[`BUTTON5]", [`BUTTON5] ]

let bool_values = [ "true", true ; "false", false ]

(** Definitions of properties *)
let properties = [
  (Function, "function", Code, "unit -> < coerce : GObj.widget ; .. >") ;
  (Expand, "expand", Bool, "bool") ;
  (Fill, "fill", Bool, "bool") ;
  (Width, "width", PosInt, "int") ;
  (Height, "height", PosInt, "int") ;
  (Border_width, "border_width", PosInt, "int") ;
  (Title, "title", Code, "string") ;
  (Allow_shrink, "allow_shrink", Bool, "bool") ;
  (Allow_grow, "allow_grow", Bool, "bool") ;
  (Auto_shrink, "auto_grow", Bool, "bool") ;
  (X_pos, "x", PosInt, "int") ;
  (Y_pos, "y", PosInt, "int") ;
  (PLabel, "label", Code, "string") ;
  (Padding, "padding", PosInt, "int") ;
  (Group, "group", Code, "Gtk.radio_button group") ;
  (Orientation, "orientation", Enum orientation_values, gt "orientation") ;
  (Toolbar_style, "style", Enum toolbar_style_values, gt "toolbar_style") ;
  (Toolbar_space_size, "space_size", PosInt, "int") ;
  (Toolbar_space_style, "space_style", Enum toolbar_space_values, "[`EMPTY | `LINE]") ;
  (Tooltips, "tooltips", Bool, "bool") ;
  (Button_relief_style, "button_relief", Enum button_relief_values, gt "relief_style") ;
  (Spacing, "spacing", PosInt, "int") ;
  (Homogeneous, "homogeneous", Bool, "bool") ;
  (Button_box_style, "layout", Enum button_box_style_values, gt "button_box_style") ;
  (Child_width, "child_width", PosInt, "int") ;
  (Child_height, "child_height", PosInt, "int") ;
  (Child_ipadx, "child_ipadx", PosInt, "int") ;
  (Child_ipady, "child_ipady", PosInt, "int") ;
  (Label_xalign, "label_xalign", Float, "Gtk.clampf" ) ;
  (Label_yalign, "label_yalign", Float, "Gtk.clampf" ) ;
  (Shadow_type, "shadow_type", Enum shadow_type_values, gt "shadow_type" ) ;
  (Obey_child, "obey_child", Bool, "bool") ;
  (Ratio, "ratio", Float, "float") ;
  (Hscrollbar_policy, "hscrollbar_policy", Enum policy_type_values, gt "policy_type") ;
  (Vscrollbar_policy, "vscrollbar_policy", Enum policy_type_values, gt "policy_type") ;
  (Handle_position, "handle_position", Enum position_values, gt "position") ;
  (Snap_edge, "snap_edge", Enum position_values, gt "position") ;
  (Column_titles, "columns_titles", Code_list, "string list") ;
  (Show_titles, "show_titles", Bool, "bool") ;
  (X_align, "x_alignment", Float, "Gtk.clampf") ;
  (Y_align, "y_alignment", Float, "Gtk.clampf") ;
  (X_pad, "x_padding", PosInt, "int") ;
  (Y_pad, "y_padding", PosInt, "int") ;
  (PText, "text", Code, "string");
  (Line_wrap, "line_wrap", Bool, "bool") ;
  (Tab_pos, "tab_position", Enum position_values, gt "position") ;
  (Show_tabs, "show_tabs", Bool, "bool") ;
  (Homogeneous_tabs, "homogeneous_tabs", Bool, "bool") ;
  (Show_border, "show_border", Bool, "bool") ;
  (Scrollable, "scrollable", Bool, "bool") ;
  (Tab_border, "tab_border", PosInt, "int" );
  (Popup, "popup_enable", Bool, "bool") ;
  (SBUpdate_policy, "update_policy", Enum sb_update_type_values, gt "update_type" ) ;
  (Visibility, "visibility", Bool, "bool") ;
  (Editable, "editable", Bool, "bool") ;
  (Use_arrows, "use_arrows", Enum use_arrows_values, "[`NEVER|`DEFAULT|`ALWAYS]" );
  (Case_sensitive, "case_sensitive", Bool, "bool") ;
  (Word_wrap, "word_wrap", Bool, "bool" ) ;
  (Column_number, "number_of_columns", PosInt, "int") ;
  (Draw_indicator, "draw_indicator", Bool, "bool") ;
  (Active, "active", Bool, "bool") ;
  (Placement, "placement", Enum corner_type_values, gt "corner_type") ;
  (Selection_mode, "selection_mode", Enum selection_mode_values, gt "selection_mode") ;
  (Justification, "justification", Enum justification_values, gt "justification") ;
  (Max_length, "max_length", PosInt, "int") ;
  (View_mode, "view_mode", Enum view_mode_values, "[`LINE | `ITEM") ;
  (View_lines, "view_lines", Bool, "bool" );
  (Handle_size, "handle_size", PosInt, "int" );
  (Modal, "modal", Bool, "bool") ;
  (Tab_label, "tab_label", Code, "string") ;
  (Accel_group_name, "accel_group_name", Code, "") ;
  (Accel_modifier, "accel_modifier", Enum_list modifier_values, gd "modifier list") ;
  (Accel_flags, "accel_flags", Enum_list accel_flag_values, gt "accel_flag") ;
  (Accel_keysym, "accel_keysym", Keysym , "Gdk.keysym") ;
  (Show_toggle, "show_toggle", Bool, "bool") ;
  (Show_indicator, "show_indicator", Bool, "bool") ;
  (Right_justify, "right_justify", Bool, "bool") ;
  (Arrow_type, "arrow_type", Enum arrow_type_values, gt "arrow_type") ;
  (Calendar_options, "calendar_options", Enum_list calendar_options_values, gt "calendar_display_options") ;
  (Popdown_strings, "popdown_strings", Code, "string list") ;
  (Value_in_list, "value_in_list", Bool, "bool");
  (Ok_if_empty, "ok_if_empty", Bool, "bool") ;
  (Update_policy, "update_policy", Enum update_type_values, gt "policy_type") ;
  (PPixmap_file, "pixmap_file", Code, "string");
  (PPixmap_data, "pixmap_data", Code, "string array");
  (PPixmap_code, "pixmap_code", Code, "pixmap");
] 

(** Get information on property kind. *)
let get_prop_info pk =
  try
    List.find (fun (pkind,_,_,_) -> pk = pkind) properties
  with
    Not_found ->
      raise (Failure "a property was not found in Zog_types.properties")

(** {2 Gui elements} *)

(** The different kinds of elements. *)
type ele_class =
  | Custom_box
  | Button
  | Toggle_button
  | Check_button
  | Radio_button
  | Toolbar
  | Hbox
  | Vbox
  | Hbutton_box
  | Vbutton_box
  | Fixed
  | Frame
  | Aspect_frame
  | Scrolled_window
  | Event_box
  | Handle_box
  | Viewport
  | Hseparator
  | Vseparator
  | Clist
  | Gui2_Clist
  | Label
  | Statusbar
  | Notebook
  | Color_selection
  | Pixmap
  | Pixmap_file
  | Pixmap_data
  | Pixmap_code
  | Entry
  | Spin_button
  | Combo
  | Tree
  | Text
  | Hpaned
  | Vpaned
  | Window

  | Menubar
  | Menu_item
  | Tearoff_menu_item
  | Check_menu_item
  | Radio_menu_item
  | Menu_separator
  | Menu

  | Table
  | Progress_bar
  | HRuler
  | VRuler

  | Arrow
  | Calendar
  | Drawing_area
  | Font_selection

(** A GUI element. Some fields won't be used, depending on the element class. *)
type gui_element = {
    mutable name : string ;
    name_loc : Camlp4.PreCast.Loc.t;
    classe : ele_class ;
    mutable props : property list ;
    mutable children : gui_element list ;
    mutable expanded : bool ; (* true if the tree node is expanded *)
  } 
    
(** An entity will become a class in the generated ocaml code. *)
type entity = {
    mutable en_name : string ;
    mutable en_ele : gui_element option ;
    mutable en_params : string list ;
  } 

(** This list can be used to iter on classes. *)
let class_names_and_strings = 
  List.sort
    (fun (_,s1,_,_) (_,s2,_,_) -> compare s1 s2)
    [
      Custom_box, "custom", "box_", "" ;
      Button, "button", "wb_", "GButton.button" ;
      Toggle_button, "toggle_button", "wtog_", "GButton.toggle_button" ;
      Check_button, "check_button", "wchk_", "GButton.check_button" ;
      Radio_button, "radio_button", "wradio_", "GButton.radio_button" ;
      Toolbar, "toolbar", "wtool_", "GButton.toolbar" ;
      Hbox, "hbox", "hbox_", "GPack.hbox" ;
      Vbox, "vbox", "vbox_", "GPack.vbox" ;
      Hbutton_box, "button_box", "wbb_", "GPack.button_box `HORIZONTAL" ;
      Vbutton_box, "vbutton_box", "wbb_", "GPack.button_box `VERTICAL" ;
      Fixed, "fixed", "wfix_", "GPack.fixed" ;
      Frame, "frame", "wf_", "GBin.frame" ;
      Aspect_frame, "aspect_frame", "waf_", "GBin.aspect_frame" ;
      Scrolled_window, "scrolled_window", "wscroll_", "GBin.scrolled_window" ;
      Event_box, "event_box", "wevt_", "GBin.event_box" ;
      Handle_box, "handle_box", "whandle_", "GBin.handle_box" ;
      Viewport, "viewport", "wview_", "GBin.viewport" ;
      Hseparator, "hseparator", "whsep", "GMisc.separator `HORIZONTAL" ;
      Vseparator, "vseparator", "wvsep", "GMisc.separator `VERTICAL" ;
      Clist, "clist", "wlist_", "GList.clist" ;
      Gui2_Clist, "gui2_clist", "wlist_", "Gui2_GList.clist" ;
      Label, "label", "wl_", "GMisc.label" ;
      Statusbar, "statusbar", "wstat_", "GMisc.statusbar" ;
      Notebook, "notebook", "wnote_", "GPack.notebook" ;
      Color_selection, "color_selection", "wcolorsel", "GMisc.color_selection" ;
      Pixmap, "pixmap", "wpix_", "GMisc.pixmap" ;
      Entry, "entry", "we_", "GEdit.entry" ;
      Spin_button, "spin_button", "wsb_", "GEdit.spin_button" ;
      Combo, "combo", "wcb_", "GEdit.combo" ;
      Tree, "tree", "wtree_", "GTree.tree" ;
      Text, "text", "wt_", "GEdit.text" ;
      Hpaned, "hpaned", "wpane_", "GPack.paned `HORIZONTAL" ;
      Vpaned, "vpaned", "wpane_", "GPack.paned `VERTICAL" ;
      Window, "window", "win_", "GWindow.window" ;
      Menubar, "menubar", "mbar_", "GMenu.menu_bar" ;
      Menu_item, "menu_item", "item", "GMenu.menu_item" ;
      Tearoff_menu_item, "tearoff_menu_item", "item", "GMenu.tearoff_item" ;
      Check_menu_item, "check_menu_item", "item", "GMenu.check_menu_item" ;
      Radio_menu_item, "radio_menu_item", "item", "GMenu.radio_menu_item" ;
      Menu_separator, "menu_separator", "sep", "GMenu.menu_item" ;
      Menu, "menu", "menu", "GMenu.menu" ;
      Table, "table", "wtab_", "GPack.table ~rows: 1 ~columns: 1" ;
      Progress_bar, "progress_bar", "wpb_", "GRange.progress_bar" ;
      HRuler, "hruler", "wrul_", "GRange.ruler `HORIZONTAL" ;
      VRuler, "vruler", "wrul_", "GRange.ruler `VERTICAL" ;
      Arrow, "arrow", "wa_", "GMisc.arrow" ;
      Calendar, "calendar", "wcal_", "GMisc.calendar" ;
      Drawing_area, "drawing_area", "wdraw_", "GMisc.drawing_area" ;
      Font_selection, "font_selection", "wfontsel", "GMisc.font_selection" ;
    ] 

let class_names_and_strings_opt = 
  [
    Pixmap_data, "pixmap_data", "wpix_", "GMisc.pixmap" ;
    Pixmap_file, "pixmap_file", "wpix_", "GMisc.pixmap" ;
    Pixmap_code, "pixmap_code", "wpix_", "GMisc.pixmap" ;
  ] 

let class_names_and_strings_complete =
  class_names_and_strings @ class_names_and_strings_opt

(** This list can be used to iter on classes used to define menus. *)
let menu_classes_names_and_strings = List.filter
    (fun (c,_,_,_) -> 
      List.mem c  
        [ Menu_item ; Tearoff_menu_item ; Menu_separator ;
          Check_menu_item ; Radio_menu_item ; Menu])
    class_names_and_strings

(** Get information on a class kind. *)
let get_class_info cl =
  try
    List.find (fun (c,_,_,_) -> c = cl) class_names_and_strings_complete
  with
    Not_found ->
     raise (Failure "a constructor has no string in Zog_types.class_names_and_strings")

(** Return the value of a property kind in a list of properties. *)
let get_prop_value props kind =
  try (List.find (fun p -> p.prop_kind = kind) props).prop_value
  with Not_found -> ""

(** Set the value of a given property, for the given element. *)
let set_prop_value e kind v =
  try (List.find (fun p -> p.prop_kind = kind) e.props).prop_value <- v
  with Not_found -> ()

(** Get a string to represent a kind of gui element. *)
let string_of_gui_element_class c =
  let (_, s, _, _) = get_class_info c in
  s
  
(** A type to know when we must use add, pack, add1/add2 or add_with_viewport. *)
type pack_method = Pack | Add | Add1 | Add2 | Add_with_viewport | Insert_page | No_pack | Set_submenu

(** Get the pack method from the class of a gui element and its parent. *)
let pack_method_of_ele parent ele =
  match parent.classe, ele.classe with
  | Scrolled_window, Tree -> Add_with_viewport
  | Hpaned, _ when (match parent.children with c1 :: _ when c1 == ele -> true | _ -> false) -> Add1
  | Hpaned, _ when (match parent.children with _ :: c2 :: _ when c2 == ele -> true | _ -> false) -> Add2
  | Hpaned, _ -> Add1 (* we should not reach this case *)

  | Vpaned, _ when (match parent.children with c1 :: _ when c1 == ele -> true | _ -> false) -> Add1
  | Vpaned, _ when (match parent.children with _ :: c2 :: _ when c2 == ele -> true | _ -> false) -> Add2
  | Vpaned, _ -> Add1 (* we should not reach this case *)

  | Custom_box, _ -> No_pack

  | Clist, _
  | Gui2_Clist, _
  | Label, _
  | Hseparator, _
  | Vseparator, _
  | Color_selection, _
  | Pixmap, _
  | Pixmap_file, _
  | Pixmap_data, _
  | Pixmap_code, _
  | Entry, _
  | Spin_button, _
  | Combo, _
  | Statusbar, _
  | Tree, _
  | Text, _ 
  | Table, _ (* Table : A VOIR ? *)
  | Progress_bar, _
  | HRuler, _
  | VRuler, _
  | Arrow, _ 
  | Calendar, _
  | Drawing_area, _
  | Font_selection, _ 
    -> raise (Failure ("Nothing can be packed in "^parent.name))

  | Notebook, _ -> Insert_page

  | Button, _
  | Toggle_button, _
  | Check_button, _
  | Radio_button, _
  | Frame, _
  | Aspect_frame, _
  | Scrolled_window, _
  | Event_box, _
  | Handle_box, _
  | Viewport, _
  | Fixed, _
  | Toolbar, _
  | Window, _ -> Add

  | Hbox, _
  | Vbox, _
  | Vbutton_box, _
  | Hbutton_box, _ -> Pack
          
  | Menu_item, _ -> Set_submenu
  | Tearoff_menu_item, _
  | Check_menu_item, _
  | Radio_menu_item, _
  | Menu_separator, _ -> No_pack
  | Menubar, _
  | Menu, _ -> Add
