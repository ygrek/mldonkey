module StoredResult :
  sig
    type result = CommonTypes.result_info
    type stored_result = CommonTypes.result
    type search = CommonTypes.search
    val store_name : string
    val result_names : CommonTypes.result_info -> string list
    val result_size : CommonTypes.result_info -> int64
    val result_uids : CommonTypes.result_info -> CommonTypes.Uid.t list
    val result_tags : CommonTypes.result_info -> CommonTypes.tag list
    val result_index : CommonTypes.result -> Store.index
    val search_query : CommonTypes.search -> CommonTypes.query
  end
module IndexedResults :
  sig
    val store : StoredResult.result Store.t
    module Document :
      sig
        type t = Store.index
        val num : Store.index -> int
        val filtered : Store.index -> bool
        val filter : Store.index -> bool -> unit
        val doc_value : Store.index -> StoredResult.result
      end
    val name_bit : int
    val artist_bit : int
    val title_bit : int
    val album_bit : int
    val media_bit : int
    val format_bit : int
    val uid_bit : int
    val index_result : (string -> int -> unit) -> StoredResult.result -> unit
    val query_to_indexer :
      ('a -> StoredResult.result) -> CommonTypes.query -> 'a Indexer.query
    module DocIndexer :
      sig
        type index = Indexer2.FullMake(Document).index
        val create : unit -> index
        val add : index -> string -> Document.t -> int -> unit
        val clear : index -> unit
        val filter_words : index -> string list -> unit
        val clear_filter : index -> unit
        val filtered : Document.t -> bool
        val query : index -> Document.t Indexer.query -> Document.t array
        val query_map :
          index -> Document.t Indexer.query -> Document.t Intmap.t
        val stats : index -> int
      end
    val get_result : StoredResult.stored_result -> StoredResult.result
    val update_result :
      StoredResult.stored_result -> StoredResult.result -> unit
    val add : StoredResult.result -> Store.index
    module MakeIndex :
      functor (FilterResult : sig
          val add_search_result : StoredResult.search -> StoredResult.result -> unit
            end) ->
        sig
          val index : DocIndexer.index
          val index_string : Document.t -> string -> int -> unit
          val add : StoredResult.stored_result -> unit
          val find : StoredResult.search -> unit
          val clear : unit -> unit
          val stats : unit -> int
        end
    val stats : unit -> int * int
  end
val next_result_num : int ref
val results_by_uid :
  (CommonTypes.uid_type, StoredResult.stored_result) Hashtbl.t
val known_uids : (CommonTypes.uid_type, int) Hashtbl.t
val results_by_num : (int, StoredResult.stored_result) Hashtbl.t
val set_result_name : CommonTypes.result_info -> string -> unit
val set_result_tag : CommonTypes.result_info -> CommonTypes.tag -> unit
val declare_result :
  StoredResult.stored_result ->
  StoredResult.result -> CommonTypes.uid_type -> unit
val update_result_num : StoredResult.result -> StoredResult.stored_result
val find_result : int -> StoredResult.stored_result
val dummy_result : CommonTypes.result_info
val result_download :
  StoredResult.stored_result -> 'a -> 'b -> CommonTypes.file list
val results_iter : (int -> StoredResult.stored_result -> unit) -> unit
val update_result : StoredResult.result -> unit
val update_result2 :
  StoredResult.stored_result -> StoredResult.result -> unit
val increment_avail : StoredResult.stored_result -> StoredResult.stored_result
val update_or_create_avail : CommonTypes.tag list -> CommonTypes.tag list
