val simple_bind: int -> Unix.file_descr
val simple_server : int -> (Unix.file_descr * Unix.sockaddr -> 'a) -> unit
val channel_relay : in_channel -> out_channel -> unit
val simple_relay : Unix.file_descr -> Unix.file_descr -> unit
val spy_relay :
  (string -> int -> int -> 'a) -> Unix.file_descr -> Unix.file_descr -> unit
val simple_connect : string -> int -> Unix.file_descr

val channel_connect : Unix.inet_addr -> int -> in_channel * out_channel
  