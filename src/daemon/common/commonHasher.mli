type hash_method = MD4 | MD5 | SHA1 | TIGER
and 'a job = {
  job_name : string;
  job_begin : int64;
  job_len : int64;
  job_method : hash_method;
  job_result : 'a;
  job_handler : 'a job -> unit;
  job_error : bool;
} 
val fifo : string job Fifo.t
val current_job : (string job * Unix.file_descr) option ref
external job_done : 'a job -> bool = "ml_job_done"
external job_start : 'a job -> Unix.file_descr -> unit = "ml_job_start"
val compute_md4 : string -> int64 -> int64 -> (Md4.Md4.t job -> unit) -> unit
val compute_sha1 :
  string -> int64 -> int64 -> (Md4.Sha1.t job -> unit) -> unit
val compute_md5 : string -> int64 -> int64 -> (Md4.Md5.t job -> unit) -> unit
val compute_tiger :
  string -> int64 -> int64 -> (Md4.TigerTree.t job -> unit) -> unit
