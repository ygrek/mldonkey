type 'a client_impl = {
    mutable impl_client_type : CommonTypes.client_type;
    mutable impl_client_state : CommonTypes.host_state;
    mutable impl_client_update : int;
    mutable impl_client_has_slot : bool;
    mutable impl_client_upload : CommonTypes.shared option;
    mutable impl_client_num : int;
    mutable impl_client_val : 'a;
    mutable impl_client_ops : 'a client_ops;
  
  } 
and 'a client_ops = {
    mutable op_client_network : CommonTypes.network;
    mutable op_client_connect : 'a -> unit;
    mutable op_client_to_option : 'a -> (string * Options.option_value) list;
    mutable op_client_info : 'a -> GuiTypes.client_info;
    mutable op_client_say : 'a -> string -> unit;
    mutable op_client_browse : 'a -> bool -> unit;
    mutable op_client_files : 'a -> (string * CommonTypes.result) list;
    mutable op_client_clear_files : 'a -> unit;
    mutable op_client_bprint : 'a -> Buffer.t -> unit;
    mutable op_client_bprint_html : 'a -> Buffer.t -> CommonTypes.file -> unit;
    mutable op_client_dprint :
    'a -> CommonTypes.ui_conn -> CommonTypes.file -> unit;
    mutable op_client_dprint_html :
    'a -> CommonTypes.ui_conn -> CommonTypes.file -> string -> bool;
    mutable op_client_debug : 'a -> bool -> unit;
    mutable op_client_can_upload : 'a -> int -> unit;
    mutable op_client_enter_upload_queue : 'a -> unit;
  } 
val client_print_html : CommonTypes.client -> CommonTypes.ui_conn -> unit
val client_print : CommonTypes.client -> CommonTypes.ui_conn -> unit
val client_must_update : CommonTypes.client -> unit
val client_info : CommonTypes.client -> GuiTypes.client_info
val client_say : CommonTypes.client -> string -> unit
val client_debug : CommonTypes.client -> bool -> unit
val client_files : CommonTypes.client -> (string * CommonTypes.result) list
val client_bprint : CommonTypes.client -> Buffer.t -> unit
val client_bprint_html :
  CommonTypes.client -> Buffer.t -> CommonTypes.file -> unit
val client_dprint :
  CommonTypes.client -> CommonTypes.ui_conn -> CommonTypes.file -> unit
val client_dprint_html :
  CommonTypes.client ->
  CommonTypes.ui_conn -> CommonTypes.file -> string -> bool
val client_connect : CommonTypes.client -> unit
val client_clear_files : CommonTypes.client -> unit
val client_browse : CommonTypes.client -> bool -> unit
val as_client : 'a client_impl -> CommonTypes.client
val as_client_impl : CommonTypes.client -> 'a client_impl
val client_num : CommonTypes.client -> int
val client_state : CommonTypes.client -> CommonTypes.host_state
val set_client_state : CommonTypes.client -> CommonTypes.host_state -> unit
val set_client_disconnected : CommonTypes.client -> unit
val new_client : 'a client_impl -> unit
val book_client_num : unit -> int
val new_client_with_num : 'a client_impl -> int -> unit
val client_network : CommonTypes.client -> CommonTypes.network
val client_to_option :
  CommonTypes.client -> (string * Options.option_value) list
val new_client_ops : CommonTypes.network -> 'a client_ops
val dummy_client_impl : int client_impl
val client_remove : CommonTypes.client -> unit
val client_type : CommonTypes.client -> CommonTypes.client_type
val set_client_type : CommonTypes.client -> CommonTypes.client_type -> unit
val client_new_file :
  CommonTypes.client -> string -> CommonTypes.result -> unit
val client_find : int -> CommonTypes.client
val clients_get_all : unit -> int list
val check_client_implementations : unit -> unit
val client_can_upload : CommonTypes.client -> int -> unit
val client_enter_upload_queue : CommonTypes.client -> unit
val client_upload : CommonTypes.client -> CommonTypes.shared option
val set_client_upload : CommonTypes.client -> CommonTypes.shared option -> unit
val client_has_a_slot : CommonTypes.client -> bool
val set_client_has_a_slot : CommonTypes.client -> bool -> unit
  
val uploaders : CommonTypes.client Intmap.t ref
  
