val search_num : int ref
val searches_by_num : (int, CommonTypes.search) Hashtbl.t
val new_search :
  CommonTypes.ui_user ->
  CommonTypes.query GuiTypes.search_request -> CommonTypes.search
val search_find : int -> CommonTypes.search
val search_add_result_in :
  CommonTypes.search -> CommonTypes.result -> unit
val search_end_reply : CommonTypes.search -> unit
val search_nresults : CommonTypes.search -> int
val search_of_args : string list -> CommonTypes.query * int
type englob_op = IN_NOOP | IN_AND | IN_OR
val custom_query : Buffer.t -> string -> unit
val complex_search : Buffer.t -> unit
val search_forget : CommonTypes.ui_user -> CommonTypes.search -> unit
val search_close : CommonTypes.search -> unit
val search_media_list : (string * string) list
val or_comb : CommonTypes.query -> CommonTypes.query -> CommonTypes.query
val and_comb : CommonTypes.query -> CommonTypes.query -> CommonTypes.query
val andnot : CommonTypes.query -> CommonTypes.query -> CommonTypes.query
val mftp_query_of_query_entry : CommonTypes.query_entry -> CommonTypes.query
module Filter :
  sig
    val add : CommonTypes.result -> unit
    val find : CommonTypes.search -> unit
    val clear : unit -> unit
    val stats : unit -> int
  end
module Local :
  sig
    val add : CommonTypes.result -> unit
    val find : CommonTypes.search -> unit
    val clear : unit -> unit
    val stats : unit -> int
  end
val clean_local_search : int ref
val local_search : CommonTypes.search -> unit
  
val result_format_of_name : string -> string
val result_media_of_name : string -> string
  