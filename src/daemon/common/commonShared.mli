type 'a shared_impl = {
  impl_shared_fullname : string;
  impl_shared_codedname : string;
  mutable impl_shared_val : 'a;
  mutable impl_shared_update : int;
  mutable impl_shared_num : int;
  mutable impl_shared_ops : 'a shared_ops;
  mutable impl_shared_uploaded : int64;
  mutable impl_shared_size : int64;
  mutable impl_shared_id : Md4.Md4.t;
  mutable impl_shared_requests : int;
  mutable impl_shared_file : CommonTypes.file option;
  mutable impl_shared_servers : CommonTypes.server list;
} 
and 'a shared_ops = {
  mutable op_shared_info : 'a -> GuiTypes.shared_info;
  mutable op_shared_unshare : 'a -> unit;
  mutable op_shared_state : CommonTypes.file -> CommonTypes.ui_conn -> string;
} 

val dirnames_prio : (string * int) list ref

val as_shared : 'a shared_impl -> CommonTypes.shared
val as_shared_impl : CommonTypes.shared -> 'a shared_impl
val shared_num : CommonTypes.shared -> int
val shared_calculate_total_bytes : unit -> unit
val shared_counter : int ref
val shared_must_update : CommonTypes.shared -> unit
val shared_must_update_downloaded : CommonTypes.shared -> unit
val update_shared_num : 'a shared_impl -> unit
val replace_shared : 'a shared_impl -> 'b shared_impl -> unit
val shared_remove : 'a shared_impl -> unit
val dirnames : (string, string) Hashtbl.t
val dirname_counter : int ref
val files_scanned : int ref
val files_scanned_size : int64 ref
val new_shared : string -> int -> string -> string -> unit
val shared_num : CommonTypes.shared -> int
val shared_fullname : CommonTypes.shared -> string
val shared_size : CommonTypes.shared -> Int64.t
val shared_codedname : CommonTypes.shared -> string
val shared_unshare : CommonTypes.shared -> unit
val shared_dir : CommonTypes.shared option -> string
val shared_prio : CommonTypes.shared option -> int
val new_shared_ops : CommonTypes.network -> 'a shared_ops
val dummy_shared : int shared_impl
val shared_find : int -> CommonTypes.shared
val shared_iter : (CommonTypes.shared -> unit) -> unit
val can_share : string -> bool
val shared_add_directory : CommonTypes.shared_directory -> unit
val shared_check_files : unit -> unit
val impl_shared_info : 'a shared_impl -> GuiTypes.shared_info
val shared_info : CommonTypes.shared -> GuiTypes.shared_info
val shared_state : CommonTypes.shared -> CommonTypes.ui_conn -> string
