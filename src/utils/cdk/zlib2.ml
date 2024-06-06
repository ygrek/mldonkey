open Zlib

external zlib_version : unit -> string = "camlzip_zlibversion"

let zlib_version_num () =
  begin
    try
      zlib_version ()
    with _ -> ""
  end

let grow_buffer s =
  let s' = String.create (2 * Bytes.length s) in
  Bytes.blit s 0 s' 0 (Bytes.length s);
  s'

let compress_string ?(level = 6) inbuf =
  let zs = deflate_init level true in
  let rec compr inpos outbuf outpos =
    let inavail = Bytes.length inbuf - inpos in
    let outavail = Bytes.length outbuf - outpos in
    if outavail = 0
    then compr inpos (grow_buffer outbuf) outpos
    else begin
      let (finished, used_in, used_out) =
        deflate zs inbuf inpos inavail outbuf outpos outavail
                   (if inavail = 0 then Z_FINISH else Z_NO_FLUSH) in
      if finished then 
        Bytes.sub outbuf 0 (outpos + used_out)
      else
        compr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = compr 0 (String.create (Bytes.length inbuf)) 0 in
  deflate_end zs;
  res

(* header info from camlzip/gpl *)
let gzip_bytes ?(level = 6) inbuf =
  if Bytes.length inbuf <= 0 then Bytes.empty else
  begin
  let zs = deflate_init level false in
  let out_crc = ref Int32.zero in
  let rec compr inpos outbuf outpos =
    let inavail = Bytes.length inbuf - inpos in
    let outavail = Bytes.length outbuf - outpos in
    if outavail = 0
    then compr inpos (grow_buffer outbuf) outpos
    else begin 
      let (finished, used_in, used_out) =
        deflate zs inbuf inpos inavail outbuf outpos outavail
                   (if inavail = 0 then Z_FINISH else Z_NO_FLUSH) in
         out_crc := update_crc !out_crc inbuf inpos used_in;
      if finished then 
        Bytes.sub outbuf 0 (outpos + used_out)
      else
        compr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = compr 0 (String.create (Bytes.length inbuf)) 0 in
  deflate_end zs; 
  let buf = Buffer.create (18 + Bytes.length res) in
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
  Buffer.add_bytes buf res;
  write_int32 buf !out_crc;
  write_int32 buf (Int32.of_int (Bytes.length inbuf));
  Buffer.to_bytes buf
  end

let gzip_string ?(level = 6) instr =
  gzip_bytes ~level:level (Bytes.of_string instr)

let uncompress_string2 inbuf =
  let zs = inflate_init true in
  let rec uncompr inpos outbuf outpos =
    let inavail = Bytes.length inbuf - inpos in
    let outavail = Bytes.length outbuf - outpos in
    if outavail = 0
    then uncompr inpos (grow_buffer outbuf) outpos
    else begin
      let (finished, used_in, used_out) =
        inflate zs inbuf inpos inavail outbuf outpos outavail Z_SYNC_FLUSH in
      if finished then 
        Bytes.sub outbuf 0 (outpos + used_out)
      else
        uncompr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = uncompr 0 (String.create (2 * Bytes.length inbuf)) 0 in
  inflate_end zs;
  res

let uncompress_string s =
  let buf = Buffer.create (2 * String.length s) in
  let pos = ref 0 in
  let len = String.length s in
  uncompress ~header: true (fun b ->
      let n = min (Bytes.length b) (len - !pos) in
      if n < 1 then 0 else begin
      String.blit s !pos b 0 n;
      pos := !pos + n;
      n end
  ) (fun s len -> Buffer.add_bytes buf (Bytes.sub s 0 len));
  Buffer.contents buf
  

