exception Error of string * string

let _ =
  Callback.register_exception "Zlib.Error" (Error("",""))

type stream

type flush_command =
    Z_NO_FLUSH
  | Z_SYNC_FLUSH
  | Z_FULL_FLUSH
  | Z_FINISH

external deflate_init: int -> bool -> stream = "camlzip_deflateInit"
external deflate:
  stream -> string -> int -> int -> string -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_deflate_bytecode" "camlzip_deflate"
external deflate_end: stream -> unit = "camlzip_deflateEnd"

external inflate_init: bool -> stream = "camlzip_inflateInit"
external inflate:
  stream -> string -> int -> int -> string -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_inflate_bytecode" "camlzip_inflate"
external inflate_end: stream -> unit = "camlzip_inflateEnd"

external update_crc: int32 -> string -> int -> int -> int32
                   = "camlzip_update_crc32"

external zlib_version : unit -> string = "camlzip_zlibversion"

let zlib_version_num () =
  begin
    try
      zlib_version ()
    with _ -> ""
  end

let buffer_size = 1024

let compress ?(level = 6) ?(header = true) refill flush =
  let inbuf = String.create buffer_size
  and outbuf = String.create buffer_size in
  let zs = deflate_init level header in
  let rec compr inpos inavail =
    if inavail = 0 then begin
      let incount = refill inbuf in
      if incount = 0 then compr_finish() else compr 0 incount
    end else begin
      let (_, used_in, used_out) =
        deflate zs inbuf inpos inavail outbuf 0 buffer_size Z_NO_FLUSH in
      flush outbuf used_out;
      compr (inpos + used_in) (inavail - used_in)
    end
  and compr_finish () =
    let (finished, _, used_out) =
       deflate zs inbuf 0 0 outbuf 0 buffer_size Z_FINISH in
    flush outbuf used_out;
    if not finished then compr_finish()
  in
    compr 0 0;
    deflate_end zs

let grow_buffer s =
  let s' = String.create (2 * String.length s) in
  String.blit s 0 s' 0 (String.length s);
  s'

let compress_string ?(level = 6) inbuf =
  let zs = deflate_init level true in
  let rec compr inpos outbuf outpos =
    let inavail = String.length inbuf - inpos in
    let outavail = String.length outbuf - outpos in
    if outavail = 0
    then compr inpos (grow_buffer outbuf) outpos
    else begin
      let (finished, used_in, used_out) =
        deflate zs inbuf inpos inavail outbuf outpos outavail
                   (if inavail = 0 then Z_FINISH else Z_NO_FLUSH) in
      if finished then 
        String.sub outbuf 0 (outpos + used_out)
      else
        compr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = compr 0 (String.create (String.length inbuf)) 0 in
  deflate_end zs;
  res

(* header info from camlzip/gpl *)
let gzip_string ?(level = 6) inbuf =
  if String.length inbuf <= 0 then "" else
  begin
  let zs = deflate_init level false in
  let out_crc = ref Int32.zero in
  let rec compr inpos outbuf outpos =
    let inavail = String.length inbuf - inpos in
    let outavail = String.length outbuf - outpos in
    if outavail = 0
    then compr inpos (grow_buffer outbuf) outpos
    else begin 
      let (finished, used_in, used_out) =
        deflate zs inbuf inpos inavail outbuf outpos outavail
                   (if inavail = 0 then Z_FINISH else Z_NO_FLUSH) in
         out_crc := update_crc !out_crc inbuf inpos used_in;
      if finished then 
        String.sub outbuf 0 (outpos + used_out)
      else
        compr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = compr 0 (String.create (String.length inbuf)) 0 in
  deflate_end zs; 
  let buf = Buffer.create (18 + String.length res) in
  let write_int wbuf n =
    Buffer.add_char wbuf (char_of_int n)
  in
  let write_int32 wbuf n =
    let r = ref n in
    for i = 1 to 4 do
      Buffer.add_char wbuf (char_of_int (Int32.to_int (Int32.logand !r 0xffl)));
      r := Int32.shift_right_logical !r 8
    done
  in
  write_int buf 0x1F;
  write_int buf 0x8B;
  write_int buf 8;
  write_int buf 0;
  for i = 1 to 4 do write_int buf 0 done;
  write_int buf 0;
  write_int buf 0xFF;
  Buffer.add_string buf res;
  write_int32 buf !out_crc;
  write_int32 buf (Int32.of_int (String.length inbuf));
  Buffer.contents buf
  end

let uncompress ?(header = true) refill flush =
  let inbuf = String.create buffer_size
  and outbuf = String.create buffer_size in
  let zs = inflate_init header in
  let rec uncompr inpos inavail =
    if inavail = 0 then begin
      let incount = refill inbuf in
      if incount = 0 then uncompr_finish true else uncompr 0 incount
    end else begin
      let (finished, used_in, used_out) =
        inflate zs inbuf inpos inavail outbuf 0 buffer_size Z_SYNC_FLUSH in
      flush outbuf used_out;
      if not finished then uncompr (inpos + used_in) (inavail - used_in)
    end
  and uncompr_finish first_finish =
    (* Gotcha: if there is no header, inflate requires an extra "dummy" byte
       after the compressed stream in order to complete decompression
       and return finished = true. *)
    let dummy_byte = if first_finish && not header then 1 else 0 in
    let (finished, _, used_out) =
       inflate zs inbuf 0 dummy_byte outbuf 0 buffer_size Z_SYNC_FLUSH in
    flush outbuf used_out;
    if not finished then uncompr_finish false
  in
    uncompr 0 0;
    inflate_end zs

let uncompress_string2 inbuf =
  let zs = inflate_init true in
  let rec uncompr inpos outbuf outpos =
    let inavail = String.length inbuf - inpos in
    let outavail = String.length outbuf - outpos in
    if outavail = 0
    then uncompr inpos (grow_buffer outbuf) outpos
    else begin
      let (finished, used_in, used_out) =
        inflate zs inbuf inpos inavail outbuf outpos outavail Z_SYNC_FLUSH in
      if finished then 
        String.sub outbuf 0 (outpos + used_out)
      else
        uncompr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = uncompr 0 (String.create (2 * String.length inbuf)) 0 in
  inflate_end zs;
  res

let uncompress_string s =
  let buf = Buffer.create (2 * String.length s) in
  let pos = ref 0 in
  let len = String.length s in
  uncompress ~header: true (fun b ->
      let n = min (String.length b) (len - !pos) in
      if n < 1 then 0 else begin
      String.blit s !pos b 0 n;
      pos := !pos + n;
      n end
  ) (fun s len -> Buffer.add_string buf (String.sub s 0 len));
  Buffer.contents buf
  
