type shared_file = {
  shared_fullname : string;
  shared_codedname : string;
  shared_size : int64;
  shared_fd : Unix32.t;
  shared_id : int;
  shared_format : CommonTypes.format;
} 
and shared_tree = {
  shared_dirname : string;
  mutable shared_files : shared_file list;
  mutable shared_dirs : (string * shared_tree) list;
} 
and upload = {
  upload_file : CommonTypes.file;
  upload_client : CommonTypes.client;
  mutable upload_pos : int64;
  mutable upload_end : int64;
  mutable upload_sock : TcpBufferedSocket.t option;
  mutable upload_on_close : upload -> unit;
  upload_subdir : string Options.option_record;
  upload_on_finish : upload -> unit;
} 

  (*
  val shareds_counter : int ref
val shared_counter : int64 ref
val shared_files : (string, shared_file) Hashtbl.t
module Document :
  sig
    type t = shared_file
    val num : shared_file -> int
    val filtered : 'a -> bool
    val filter : 'a -> 'b -> unit
  end
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
    val query_map : index -> Document.t Indexer.query -> Document.t Intmap.t
  end
val index : DocIndexer.index
val table : (int, shared_file) Hashtbl.t
module Indexer :
  sig
    val stem : string -> string list
    val name_bit : int
    val artist_bit : int
    val title_bit : int
    val album_bit : int
    val media_bit : int
    val format_bit : int
    val index_string : Document.t -> string -> int -> unit
    val index_name : Document.t -> string -> unit
    val index_file : Document.t -> CommonTypes.tagged_file -> unit
    val bit_of_field : string -> int
    val query_to_query : CommonTypes.query -> 'a Indexer.query
    val find : Document.t Indexer.query -> Document.t array
    val find_map : Document.t Indexer.query -> Document.t Intmap.t
  end
  
    *)

val new_shared_dir : string -> shared_tree
val shared_tree : shared_tree
val add_shared_file : shared_tree -> shared_file -> string list -> unit
val add_shared : string -> string -> int64 -> unit
val query: CommonTypes.query -> shared_file array
val find : string -> shared_file
  
val filesize_field : string