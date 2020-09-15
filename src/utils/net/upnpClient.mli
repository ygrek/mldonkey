(* simples pure c api now
(type ml_port_map  
val upnp_map_list_add    : ml_port_map -> unit
val upnp_map_list_remove : ml_port_map -> unit
val create_ml_port_map   : int -> int -> int -> int -> string -> ml_port_map
*)

type ml_port_map = {
        mutable enabled : int;
        mutable intPort : int;
        mutable extPort : int;
        mutable isTcp   : int;
        mutable natpmpStatus : int;
        mutable upnpStatus : int;
        mutable notes   : string;
}

val string_of_map_status : int -> string
val strings_port_map : ml_port_map -> string
        
external init_maps : unit -> unit = "ml_upnpInitMaps" "noalloc"
(*external set_maps  : unit -> unit = "ml_upnpSetMaps" *)
external dump_maps : unit -> unit = "ml_upnpDumpMaps" "noalloc"
external job_start : unit -> unit = "ml_upnp_job_start" "noalloc"
external job_stop  : int -> unit = "ml_upnp_job_stop"
external maps_add_item     : int -> int -> int -> int -> string -> unit  = "ml_upnpAddMap" 
external maps_remove_item  : int -> int -> int -> int -> string -> unit  = "ml_upnpRemoveMap" 
external remove_all_maps   : int -> unit = "ml_upnpRemoveAllMaps"
external maps_get  : unit -> ml_port_map list = "ml_upnpGetMaps"