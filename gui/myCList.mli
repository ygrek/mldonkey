open Gui_options
open Gui_types
open Gui_proto
open Gui
  
type ('a, 'b) t
type ('a, 'b) node

val create : gui -> int GList.clist -> ?color:('b -> string option) -> ('b -> string) list -> ('c, 'b) t

val set_context_menu : 
  ('c, 'b)t -> (('c, 'b) t ->  GToolbox.menu_entry list) -> unit
val set_selected_callback :
  ('c, 'b)t -> (('c, 'b) t -> 'b -> unit) -> unit
val set_replace_value : 
  ('c, 'b)t -> ('b -> 'b -> 'b) -> unit
val set_size_callback :
  ('c, 'b)t -> (int -> unit) -> unit

val clear : ('a, 'b) t -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val update : ('a, 'b) t -> 'a -> 'b -> unit
val selection : ('a, 'b) t -> 'b list
val set_value : ('a, 'b) t -> 'a -> 'b -> unit
val remove : ('a, 'b) t -> 'a -> unit
val unselect_all : ('a,'b) t -> unit
val select_all : ('a,'b) t -> unit
val find : ('a,'b) t -> 'a -> 'b
val size : ('a,'b) t -> int
val set_can_select_all: ('a,'b) t -> unit

val set_multiple_select :  ('a,'b) t -> bool -> unit
val get_multiple_select :  ('a,'b) t -> bool
val set_auto_resize :  ('a,'b) t -> bool -> unit
val get_auto_resize :  ('a,'b) t -> bool

val iter :  ('a,'b) t -> ('a -> 'b -> unit) -> unit
  
val update_sizes : BasicSocket.timer -> unit