(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)


open Printf2
open Unix
open Md4
 
type mail = {
    mail_to : string list;
    mail_from : string;
    mail_subject : string;
    mail_body : string;
    smtp_login : string;
    smtp_password : string;
  }

let rfc2047_encode h encoding s =
  let beginning = "=?" ^ encoding ^"?q?" in
  let ending = "?=" in
  let space = " " in
  let crlf = "\r\n" in
  let maxlen = 75 in (* max lenght of a line *)
  let buf = Buffer.create 1500 in
  let pos = ref 0 in
  let rl = ref 1 in
  let hexa_digit x =
    if x >= 10 then Char.chr (Char.code 'A' + x - 10)
    else Char.chr (Char.code '0' + x) in
  let copy tanga = begin
        Buffer.add_string buf tanga;
        pos := !pos + String.length tanga;
    end;
  in    
  copy h; 
  copy beginning;
  let newline () = 
    incr rl;
    copy ending;
    copy crlf;
    copy space;
    copy beginning;
  in
  for i=0 to (String.length s)-1 do
    let l = (!rl * (maxlen-String.length ending)) - 1 in
    if l < !pos then newline ();
    match s.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' ->
          Buffer.add_char buf s.[i]; incr pos
      | ' ' -> Buffer.add_char buf '_'; incr pos 
      | c ->
          Buffer.add_char buf '=';
          Buffer.add_char buf (hexa_digit (Char.code c / 16));
          Buffer.add_char buf (hexa_digit (Char.code c mod 16));
          pos := !pos + 3;	
  done;
  copy ending;
  Buffer.contents buf

let send_bytes oc s = Printf.fprintf oc "%a\r\n" output_bytes s; flush oc
let send_string oc s = Printf.fprintf oc "%s\r\n" s; flush oc
let send1_bytes oc s p = Printf.fprintf oc "%a %a\r\n" output_bytes s output_bytes p; flush oc
let send1_string oc s p = Printf.fprintf oc "%s %s\r\n" s p; flush oc
 
let simple_connect hostname port =
  let s = socket PF_INET SOCK_STREAM 0 in
  let h = Ip.from_name  hostname in
  let addr = Ip.to_inet_addr h in
  try
    Unix.connect s (ADDR_INET(addr,port));
    s
  with e -> close s; raise e

let last_response = ref ""

let bad_response () =
  failwith (Printf.sprintf "Bad response [%s]"
      (String.escaped !last_response))

type response = int * bool * string list

let get_response ic =
  last_response := input_line ic;
  if String.length !last_response <= 3 then bad_response ();
  if !last_response.[String.length !last_response - 1] <> '\r' then bad_response ();
  let final = match !last_response.[3] with ' ' -> true | '-' -> false | _ -> bad_response () in
  let code = int_of_string (String.sub !last_response 0 3) in
  let text = String.sub !last_response 4 (String.length !last_response - 5) in
  (code,final,text)

let read_response ic =
  let rec iter () =
    match get_response ic with
    | (n,true,_) -> n
    | _ -> iter ()
  in
  iter ()

let mail_address new_style s = if new_style then "<"^s^">" else s

let make_mail mail new_style =
  let (mail_to,mail_cc) =
    match mail.mail_to with
    | [] -> failwith "no recipients specified"
    | x::xs -> x,xs
  in
  let mail_date = Date.mail_string (Unix.time ()) in
  let addr = mail_address new_style in
        Printf.sprintf 
        "From: mldonkey %s\r\nTo: %s\r\n%s%s\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nDate: %s\r\n\r\n%s"
        (addr mail.mail_from)
        (addr mail_to)
        (match mail_cc with [] -> "" | l -> Printf.sprintf "Cc: %s\r\n" (String.concat ", " (List.map addr l)))
        (rfc2047_encode "Subject: " "utf-8" mail.mail_subject)
        mail_date
        mail.mail_body

let canon_addr s = 
  let len = String.length s in
  let rec iter_end s pos =
    if pos = -1 then s else
    if s.[pos] = ' ' then iter_end s (pos-1) else
      iter_begin s (pos-1) pos
  and iter_begin s pos last =
    if pos = -1 || s.[pos] = ' ' then
      String.sub s (pos+1) (last - pos)
    else iter_begin s (pos-1) last
  in
  iter_end s (len - 1)

let string_xor s1 s2 =
  assert (Bytes.length s1 = Bytes.length s2);
  let s = Bytes.create (Bytes.length s1) in
  for i = 0 to Bytes.length s - 1 do
    s.[i] <- Char.chr (Char.code (Bytes.get s1 i) lxor Char.code (Bytes.get s2 i));
  done;
  s

(* HMAC-MD5, RFC 2104 *)
let hmac_md5 =
  let ipad = Bytes.make 64 '\x36' in
  let opad = Bytes.make 64 '\x5C' in
  let md5 s = Md5.direct_to_string (Md5.string s) in
  fun secret challenge ->
    let secret = if String.length secret > 64 then md5 secret else secret in
    let k = Bytes.make 64 '\x00' in
    String.blit secret 0 k 0 (String.length secret);
    md5 (Bytes.to_string (string_xor k opad) ^ md5 (Bytes.to_string(string_xor k ipad) ^ challenge))

let sendmail smtp_server smtp_port new_style mail =
(* a completely synchronous function (BUG) *)
  try
    let s = simple_connect smtp_server smtp_port in
    (try Unix.setsockopt_float s Unix.SO_RCVTIMEO 30. with _ -> ());
    (try Unix.setsockopt_float s Unix.SO_SNDTIMEO 30. with _ -> ());
    let ic = in_channel_of_descr s in
    let oc = out_channel_of_descr s in
    let auth_login_enabled = ref false in
    let auth_plain_enabled = ref false in
    let auth_cram_enabled = ref false in
    let read_response_auth ic =
      let rec loop () =
        let (n,final,text) = get_response ic in
        begin match String2.split_simplify (String.uppercase text) ' ' with
        | ("AUTH"::methods) ->
          List.iter (function
          | "LOGIN" -> auth_login_enabled := true
          | "PLAIN" -> auth_plain_enabled := true
          | "CRAM-MD5" -> auth_cram_enabled := true
          | _ -> ()) methods
        | _ -> ()
        end;
        if final then n else loop ()
      in
      loop ()
    in

    try
      if read_response ic <> 220 then bad_response ();

      send1_string oc "EHLO" (gethostname ());
      if read_response_auth ic <> 250 then bad_response ();

      if mail.smtp_login <> "" then
      begin
        if !auth_cram_enabled then (* prefer CRAM-MD5 *)
        begin
          send_string oc "AUTH CRAM-MD5";
          match get_response ic with
          | (334,true,s) ->
            (* RFC 2195 *)
            let digest = hmac_md5 mail.smtp_password (Bytes.to_string (Base64.decode s)) in
            send_bytes oc (Base64.encode (Printf.sprintf "%s %s" mail.smtp_login digest));
            if read_response ic <> 235 then bad_response ()
          | _ -> bad_response ()
        end
        else if !auth_login_enabled then
        begin
          send_string oc "AUTH LOGIN";
          if read_response ic <> 334 then bad_response (); 

          send_bytes oc (Base64.encode mail.smtp_login);
          if read_response ic <> 334 then bad_response (); 

          send_bytes oc (Base64.encode mail.smtp_password);
          if read_response ic <> 235 then bad_response ()
        end
        else if !auth_plain_enabled then
        begin
          let auth = Printf.sprintf "\x00%s\x00%s" mail.smtp_login mail.smtp_password in
          send1_bytes oc (Bytes.of_string "AUTH PLAIN") (Base64.encode auth);
          if read_response ic <> 235 then bad_response ()
        end
      end;

      send1_string oc "MAIL FROM:" (mail_address new_style (canon_addr mail.mail_from));
      if read_response ic <> 250 then bad_response ();

      List.iter begin fun address ->
        send1_string oc "RCPT TO:" (mail_address new_style (canon_addr address));
        if read_response ic <> 250 then bad_response ();
      end mail.mail_to;

      send_string oc "DATA";
      if read_response ic <> 354 then bad_response ();

      let body = make_mail mail new_style in
      send_string oc body;
      send_string oc ".";
      if read_response ic <> 250 then bad_response ();

      send_string oc "QUIT";
      if read_response ic <> 221 then bad_response ();

      close_out oc;
    with e ->
        send_string oc "QUIT";
        if read_response ic <> 221 then bad_response ();
        close_out oc;
        raise e

  with e ->
      lprintf_nl "Exception %s while sending mail" (Printexc2.to_string e)
