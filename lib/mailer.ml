open Options
open Unix
  
type mail = {
    mail_to : string;
    mail_from : string;
    mail_subject : string;
    mail_body : string;
  }
  
let simple_connect hostname port = (* from netbase.ml *)
  let s = socket PF_INET SOCK_STREAM 0 in
  let h = Unix.gethostbyname hostname in
  let addr = h.h_addr_list.(0) in
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

let make_mail mail =
  Printf.sprintf 
    "From: %s\r\nTo: %s\r\nSubject: %s\r\nMIME-Version: 1.0\r\nContent-Type: text/plain\r\n\r\n%s"
  mail.mail_from
  mail.mail_to
  mail.mail_subject
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
      
let sendmail smtp_server smtp_port mail =
(* a completely synchronous function (BUG) *)
  try
    let s = simple_connect smtp_server smtp_port in
    let ic = in_channel_of_descr s in
    let oc = out_channel_of_descr s in
    
    try
      if read_response ic <> 220 then bad_response ();
      
      Printf.fprintf oc "HELO %s\r\n" (gethostname ()); flush oc;
      if read_response ic <> 250 then bad_response ();
      
      Printf.fprintf oc "MAIL FROM:%s\r\n" (canon_addr mail.mail_from); 
      flush oc;
      if read_response ic <> 250 then bad_response ();
      
      Printf.fprintf oc "RCPT TO:%s\r\n" (canon_addr mail.mail_to); flush oc;
      if read_response ic <> 250 then bad_response ();
      
      Printf.fprintf oc "DATA\r\n"; flush oc;
      if read_response ic <> 354 then bad_response ();

      let body = make_mail mail in
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
      Printf.printf "Exception %s while sending mail" (Printexc2.to_string e);
      print_newline ()
