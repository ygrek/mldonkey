val dummy_result : CommonTypes.result
type 'a result_impl = {
  mutable impl_result_update : int;
  mutable impl_result_num : int;
  mutable impl_result_val : 'a;
  mutable impl_result_ops : 'a result_ops;
} 
and 'a result_ops = {
  mutable op_result_network : CommonTypes.network;
  mutable op_result_download : 'a -> string list -> bool -> unit;
  mutable op_result_info : 'a -> CommonTypes.result_info;
} 
val result_counter : int ref
val as_result : 'a result_impl -> CommonTypes.result
val as_result_impl : CommonTypes.result -> 'a result_impl
val result_num : CommonTypes.result -> int
val dummy_result_impl : int result_impl
val dummy_result : CommonTypes.result
val new_result : 'a result_impl -> unit
val ni : CommonTypes.network -> string -> string
val fni : CommonTypes.network -> string -> 'a
val ni_ok : CommonTypes.network -> string -> unit
val result_info : CommonTypes.result -> CommonTypes.result_info
val result_download : CommonTypes.result -> string list -> bool -> unit
val results_ops : (int result_ops * int result_ops) list ref
val new_result_ops : CommonTypes.network -> 'a result_ops
val check_result_implementations : unit -> unit
val result_find : int -> CommonTypes.result
val result_print : CommonTypes.result -> 'a -> 'b -> unit
val results_iter : (int -> CommonTypes.result -> unit) -> unit
