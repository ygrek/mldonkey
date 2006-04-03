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
open Options
open Unix
open Date
  
type mail = {
    mail_to : string;
    mail_from : string;
    mail_subject : string;
    mail_body : string;
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
          if l < !pos + 2 then newline ();
          Buffer.add_char buf '=';
          Buffer.add_char buf (hexa_digit (Char.code c / 16));
          Buffer.add_char buf (hexa_digit (Char.code c mod 16));
          pos := !pos + 3;	
  done;
  copy ending;
  Buffer.contents buf
 
 
let simple_connect hostname port = (* from netbase.ml *)
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
  
let read_response ic =
  last_response := input_line ic;
  if String.length !last_response > 3 then
    int_of_string (String.sub !last_response 0 3)
  else 
    bad_response ()

let make_mail mail new_style =
  let mail_date = Date.mail_string (Unix.time ()) in
  
  if new_style then
	Printf.sprintf 
	"From: mldonkey <%s>\r\nTo: %s\r\n%s\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nDate: %s\r\n\r\n%s"
	mail.mail_from
	mail.mail_to
	(rfc2047_encode "Subject: " "utf-8" mail.mail_subject)
	mail_date
	mail.mail_body
    else
	Printf.sprintf 
	"From: mldonkey %s\r\nTo: %s\r\n%s\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nDate: %s\r\n\r\n%s"
	mail.mail_from
	mail.mail_to
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
      
let sendmail smtp_server smtp_port new_style mail =
(* a completely synchronous function (BUG) *)
  try
    let s = simple_connect smtp_server smtp_port in
    let ic = in_channel_of_descr s in
    let oc = out_channel_of_descr s in
    
    try
      if read_response ic <> 220 then bad_response ();
      
      Printf.fprintf oc "HELO %s\r\n" (gethostname ()); flush oc;
      if read_response ic <> 250 then bad_response ();
      
      if new_style then
        Printf.fprintf oc "MAIL FROM:<%s>\r\n" (canon_addr mail.mail_from)
      else
        Printf.fprintf oc "MAIL FROM:%s\r\n" (canon_addr mail.mail_from);
      flush oc;
      if read_response ic <> 250 then bad_response ();
      
      if new_style then 
        Printf.fprintf oc "RCPT TO:<%s>\r\n" (canon_addr mail.mail_to)
      else
        Printf.fprintf oc "RCPT TO:%s\r\n" (canon_addr mail.mail_to); 
      
      flush oc;
      if read_response ic <> 250 then bad_response ();
      
      Printf.fprintf oc "DATA\r\n"; flush oc;
      if read_response ic <> 354 then bad_response ();

      let body = make_mail mail new_style in
      Printf.fprintf oc "%s\r\n.\r\n" body; flush oc;
      if read_response ic <> 250 then bad_response ();
      
      Printf.fprintf oc "QUIT\r\n"; flush oc;
      if read_response ic <> 221 then bad_response ();

      close_out oc;
    with e ->
        Printf.fprintf oc "QUIT\r\n"; flush oc;
        if read_response ic <> 221 then bad_response ();
        close_out oc;
        raise e
        
  with e ->
      lprintf_nl "Exception %s while sending mail" (Printexc2.to_string e)
