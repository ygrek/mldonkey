val ni : string -> string -> string
val fni : string -> string -> 'a
val ni_ok : string -> string -> unit
val network_uid : unit -> int
val networks : CommonTypes.network list ref
val networks_by_name : (string, CommonTypes.network) Hashtbl.t
val networks_by_num : (int, CommonTypes.network) Hashtbl.t
val networks_ops : (CommonTypes.network * CommonTypes.network) list ref
val new_network :
    string ->  string ->   CommonTypes.network_flag list -> CommonTypes.network

val check_network_implementations : unit -> unit
val network_clean_exit : CommonTypes.network -> bool
val network_reset : CommonTypes.network -> unit
val network_connected_servers :
  CommonTypes.network -> CommonTypes.server list
val network_is_enabled : CommonTypes.network -> bool
val network_save_complex_options : CommonTypes.network -> unit
val network_save_sources : CommonTypes.network -> unit
val network_load_complex_options : CommonTypes.network -> unit
val network_enable : CommonTypes.network -> unit
val network_update_options : CommonTypes.network -> unit
val network_disable : CommonTypes.network -> unit
val network_recover_temp : CommonTypes.network -> unit
val network_share : CommonTypes.network -> string -> string -> int64 -> unit
val network_add_server :
  CommonTypes.network ->
  Ip.addr -> int -> CommonTypes.server
val network_server_of_option :
  CommonTypes.network ->
  (string * Options.option_value) list -> CommonTypes.server
val network_file_of_option :
  CommonTypes.network ->
    int64 ->
    CommonTypes.file_state ->
    (string * Options.option_value) list -> CommonTypes.file
val network_client_of_option :
  CommonTypes.network ->
  bool -> (string * Options.option_value) list -> CommonTypes.client
val networks_iter : (CommonTypes.network -> unit) -> unit
val networks_iter_until_true : (CommonTypes.network -> bool) -> bool
val networks_iter_all : (CommonTypes.network -> unit) -> unit
val networks_iter_all_until_true : (CommonTypes.network -> bool) -> bool
val networks_for_all : (CommonTypes.network -> bool) -> bool
val network_find_by_name : string -> CommonTypes.network
val network_find_by_num : int -> CommonTypes.network
val network_commands : (string * string * CommonTypes.arg_kind * string) list ref
val register_commands : (string * string * CommonTypes.arg_kind * string) list -> unit
val network_connect_servers : CommonTypes.network -> unit
val network_ports : CommonTypes.network -> (int * string) list
val network_porttest_start : CommonTypes.network -> unit
val network_porttest_result : CommonTypes.network -> CommonTypes.network_porttest
val network_forget_search : CommonTypes.network -> CommonTypes.search -> unit
val network_close_search : CommonTypes.network -> CommonTypes.search -> unit
val network_private_message : CommonTypes.network -> string -> string -> unit
val network_extend_search :
  CommonTypes.network ->
  CommonTypes.search -> CommonTypes.extend_search -> unit
val network_connected : CommonTypes.network -> bool
val network_clean_servers : CommonTypes.network -> unit
val network_parse_url : CommonTypes.network -> string -> CommonTypes.userdb -> string * bool
val network_info : CommonTypes.network -> CommonTypes.network_info
val commands_by_kind : (string, (string * string) list ref) Hashtbl.t
