val update_result_num : CommonTypes.result_info -> CommonTypes.result
val update_result : CommonTypes.result_info -> unit
val find_result : int -> CommonTypes.result
val get_result : CommonTypes.result -> CommonTypes.result_info
val result_download : CommonTypes.result -> string list -> bool -> 
  CommonTypes.file list
val dummy_result : CommonTypes.result_info
val results_iter : (int -> CommonTypes.result -> unit) -> unit
val known_uids : (CommonTypes.uid_type, int) Hashtbl.t
val set_result_name : CommonTypes.result_info -> string -> unit
val set_result_tag : CommonTypes.result_info -> CommonTypes.tag -> unit
  
module Document : sig 
    type t = Store.index
    val num : Store.index -> int
    val filter : Store.index -> bool -> unit
    val filtered : Store.index -> bool
    val doc_value : Store.index -> CommonTypes.result_info
  end
  