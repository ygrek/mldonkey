(** Generic network services *)

(*
(** log and return a "method not implemented" message
    @param n network name
    @param m name of the method that's not implemented
    @return the "not implemented" message *)
val ni : string -> string -> string
*)

(** log and raise a Failure exception because of a "method not implemented"
    @param n network name
    @param m name of the method that's not implemented
    @raise Failure *)
val fni : string -> string -> 'a

(** log a "method not implemented" message 
    @param n network name
    @param m name of the method that's not implemented *)
val ni_ok : string -> string -> unit

(** define a new instance of network class, to be filled later with
    functions defining the specialized methods for this network.
    @param shortname short name for the network
    @param name standard name for the network
    @param ?comment comment to add to network name in networks list
    @param flags network properties
    @return the new network instance *)
val new_network :
  string ->  string -> ?comment:string -> CommonTypes.network_flag list -> 
  CommonTypes.network

(** Find a network by its name
    @param network name
    @return network instance
    @raise Not_found *)
val network_find_by_name : string -> CommonTypes.network

(** Find a network by its internal number
    @param network identifier
    @return network instance
    @raise Not_found *)
val network_find_by_num : int -> CommonTypes.network

(** List all networks, and for each, which methods are defined 
    Useful for debugging only *)
val check_network_implementations : unit -> unit

(** {6 Network methods} *)

(** call the op_network_clean_exit method of a network. Can require
    more than one try to succeed, if cleaning up needs some time.
    @param network instance
    @return true if successful *)
val network_clean_exit : CommonTypes.network -> bool

(** call the op_network_reset method of a network.
    (called when SIGHUP is received)
    @param network instance *)
val network_reset : CommonTypes.network -> unit

(** call the op_network_connected_servers method of a network.
    @param network instance
    @return the list of servers currently connected to *)
val network_connected_servers :
  CommonTypes.network -> CommonTypes.server list

(** call the op_network_is_enabled method of a network.
    @param network instance
    @return true if the network is enabled *)
val network_is_enabled : CommonTypes.network -> bool

(** call the op_network_save_complex_options method of a network.
    @param network instance *)
val network_save_complex_options : CommonTypes.network -> unit

(** call the op_network_save_sources method of a network.
    @param network instance *)
val network_save_sources : CommonTypes.network -> unit

(** call the op_network_load_complex_options method of a network.
    @param network instance *)
val network_load_complex_options : CommonTypes.network -> unit

(** call the op_network_enable method of a network, to activate it.
    @param network instance *)
val network_enable : CommonTypes.network -> unit

(** call the op_network_update_options method of a network.
    @param network instance *)
val network_update_options : CommonTypes.network -> unit

(** call the op_network_disable method of a network, to deactivate it.
    @param network instance *)
val network_disable : CommonTypes.network -> unit

(** call the op_network_recover_temp method of a network.
    @param network instance *)
val network_recover_temp : CommonTypes.network -> unit

(** call the op_network_share method of a network.
    @param network instance
    @param fullname
    @param codedname
    @param size *)
val network_share : CommonTypes.network -> string -> string -> int64 -> unit

(** call the op_network_add_server method of a network.
    @param network instance
    @param ip of server
    @param port of server 
    @return the added server *)
val network_add_server : 
  CommonTypes.network -> Ip.addr -> int -> CommonTypes.server

(** call the op_network_server_of_option method of a network.
    @param network instance
    @param assoc list of server properties
    @return the created server *)
val network_server_of_option :
  CommonTypes.network ->
  (string * Options.option_value) list -> CommonTypes.server

(** call the op_network_file_of_option method of a network.
    @param network instance
    @param file size
    @param file state
    @param assoc list of file properties
    @return the created file *)
val network_file_of_option :
  CommonTypes.network ->
  int64 -> CommonTypes.file_state -> CommonTypes.userdb -> CommonTypes.groupdb option ->
  (string * Options.option_value) list -> CommonTypes.file

(** call the op_network_client_of_option method of a network.
    @param network instance
    @param true if client is a friend 
    @param assoc list of client properties
    @return the created client *)
val network_client_of_option :
  CommonTypes.network ->
  bool -> (string * Options.option_value) list -> CommonTypes.client

(** call the op_network_connect_servers method of a network.
    Try to connect to one more server.
    @param network instance *)
val network_connect_servers : CommonTypes.network -> unit

(** call the op_network_search method of a network.
    Initiate a new search.
    @param network instance
    @param search to start
    @param buffer to send any feedback messages to *)
val network_search : 
  CommonTypes.network -> CommonTypes.search -> Buffer.t -> unit

(** call the op_network_forget_search method of a network.
    On donkey, it stops extended searches. 
    On others, it does the same as network_close_search.
    @param network instance
    @param the search to forget *)
val network_forget_search : CommonTypes.network -> CommonTypes.search -> unit

(*
val network_private_message : CommonTypes.network -> string -> string -> unit
*)

(** call the op_network_extend_search method of a network.
    @param network instance
    @param the search to extend 
    @param the kind of extension (local or remote) *)
val network_extend_search :
  CommonTypes.network ->
  CommonTypes.search -> CommonTypes.extend_search -> unit

(** call the op_network_close_search method of a network.
    Stop for search procedure, and release associated resources.
    @param network instance
    @param the search to close *)
val network_close_search : CommonTypes.network -> CommonTypes.search -> unit

(*
val network_connected : CommonTypes.network -> bool
*)

(** call the op_network_clean_servers method of a network.
    Remove dead servers from the list.
    @param network instance *)
val network_clean_servers : CommonTypes.network -> unit

(** call the op_network_parse_url method of a network.
    Try to recognize a download URL, and if successful, to actually
    start that download.
    @param network instance
    @param URL
    @param user to start the download for
    @return status message
    @return true if successful *)
val network_parse_url : 
  CommonTypes.network -> string -> CommonTypes.userdb -> CommonTypes.groupdb option -> string * bool

(** call the op_network_download method of a network.
    Start a download from the result of a search.
    @param network instance
    @param search result
    @param user to start the download for
    @return the newly started download *)
val network_download :
  CommonTypes.network -> CommonTypes.result_info -> CommonTypes.userdb -> CommonTypes.groupdb option -> 
  CommonTypes.file

(** call the op_network_display_stats method of a network.
    Output statistics about a network
    @param network instance
    @param interface context *)
val network_display_stats : 
  CommonTypes.network -> CommonTypes.ui_conn -> unit

(** call the op_network_info method of a network.
    Return the current status of a network.
    @param network instance 
    @return status of this network *)
val network_info : CommonTypes.network -> CommonTypes.network_info

(** call the op_network_stat_info_list method of a network.
    @param network instance 
    @return list of label, duration, network_stat_info tuples *)
val network_stat_info_list : CommonTypes.network -> 
  (string * int * CommonTypes.network_stat_info list) list

(** call the op_network_gui_message method of a network.
    Handle a message received from a GUI.
    @param network instance
    @param message
    @param user *)
val network_gui_message : 
  CommonTypes.network -> string -> CommonTypes.userdb -> unit

(** call the op_network_ports method of a network.
    @param network instance 
    @return list of port numbers, with their descriptions *)
val network_ports : CommonTypes.network -> (int * string) list

(** call the op_network_porttest_start method of a network.
    Start an asynchronous test of ports well-behaving,
    if one is available for that network.
    @param network instance *)
val network_porttest_start : CommonTypes.network -> unit

(** call the op_network_porttest_result method of a network.
    Return the result of the asynchronous ports test, 
    if some are available.
    @param network instance 
    @return port test result *)
val network_porttest_result : CommonTypes.network -> CommonTypes.network_porttest

(** call the op_network_check_upload_slots method of a network.
    To be used in option_hook max_upload_slots to avoid an
    indefinite loop when having more than one of this option_hook
    @param network instance *)
val network_check_upload_slots : CommonTypes.network -> unit

(** {6 Iterators} *)

(** Call a function for each enabled network 
    @param function to call *)
val networks_iter : (CommonTypes.network -> unit) -> unit

(** Call a predicate for each enabled network, until it returns true 
    @param predicate to call
    @return true if predicate was verified for a network *)
val networks_iter_until_true : (CommonTypes.network -> bool) -> bool

(** Call a function for each known network 
    @param function to call *)
val networks_iter_all : (CommonTypes.network -> unit) -> unit

(** Call a predicate for each known network, until it returns true 
    @param predicate to call
    @return true if predicate was verified for a network *)
val networks_iter_all_until_true : (CommonTypes.network -> bool) -> bool

(** Call a predicate for each enabled network, until one returns false 
    @param predicate to call
    @return true if predicate was verified for all enabled networks *)
val networks_for_all : (CommonTypes.network -> bool) -> bool

(** {6 Internal command interpreter} *)

(** For each command, one must provide:
    - a name
    - a section name
    - the handler function
    - a textual description *)

(** List of registered commands in the internal command interpreter *)
val network_commands : (string * string * CommonTypes.arg_kind * string) list ref

(** Hashtable of registered commands names and descriptions in the
    internal command interpreter, grouped by section *)
val commands_by_kind : (string, (string * string) list ref) Hashtbl.t

(** Add commands to the registry *)
val register_commands : (string * string * CommonTypes.arg_kind * string) list -> unit


