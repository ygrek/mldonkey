# 3 "gui/gui_header.ml"

open Options

module Old_GList = GList
  
module GList = struct
    include Old_GList
      
    let clist = (clist_poly : ?columns:int ->
  ?titles:string list ->
  ?hadjustment:GData.adjustment ->
  ?vadjustment:GData.adjustment ->
  ?shadow_type:Gtk.Tags.shadow_type ->
  ?button_actions:(int * Gtk.Tags.button_action list) list ->
  ?selection_mode:Gtk.Tags.selection_mode ->
  ?reorderable:bool ->
  ?use_drag_icons:bool ->
  ?row_height:int ->
  ?titles_show:bool ->
  ?titles_active:bool ->
  ?auto_sort:bool ->
  ?sort_column:int ->
  ?sort_type:Gtk.Tags.sort_type ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> int clist)
  end

module Mes = Gui_messages 
 
let failed = ()
  
# 1 "gui/gui_zog.ml"
