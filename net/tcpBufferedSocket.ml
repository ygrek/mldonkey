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

open BasicSocket

(* let _ = Unix2.init () *)
  
type event = 
  WRITE_DONE
| CAN_REFILL
| BUFFER_OVERFLOW
| READ_DONE of int
| BASIC_EVENT of BasicSocket.event
  
type buf = {
    mutable buf : string;
    mutable pos : int;
    mutable len : int;
    mutable max_buf_size:int;
  }

type t = {
    mutable sock : BasicSocket.t;
    mutable rbuf : buf;
    mutable wbuf : buf;
    mutable event_handler : handler;
    mutable error : string;
    mutable nread : int;
    mutable nwrite : int;
    mutable monitored : bool;
    
    mutable read_control : bandwidth_controler option;
    mutable write_control : bandwidth_controler option;
    mutable write_power : int;    
  }
  
  
and handler = t -> event -> unit

and bandwidth_controler = {
    mutable remaining_bytes : int;
    mutable total_bytes : int;
    mutable nconnections : int;
    mutable connections : t list;
    allow_io : bool ref;
    mutable last_remaining : int;
  }


let uploaded_bytes = ref Int64.zero
let downloaded_bytes = ref Int64.zero
  
let nread t = t.nread

let min_buffer_read = 500
let min_read_size = min_buffer_read - 100  
  
let old_strings_size = 20
let old_strings = Array.create old_strings_size ""
let old_strings_len = ref 0
  
let new_string () =
  if !old_strings_len > 0 then begin
      decr old_strings_len;
      let s = old_strings.(!old_strings_len) in
      old_strings.(!old_strings_len) <- "";
      s
    end else
    String.create min_buffer_read
  
let delete_string s =
  if !old_strings_len < old_strings_size &&
    String.length s = min_buffer_read then begin
      old_strings.(!old_strings_len) <- s;
      incr old_strings_len;
    end

let close t s = 
(*
  if t.monitored then begin
      Printf.printf "close with %s %s" t.error s; print_newline ();
end;
*)
  begin
    try
      delete_string t.rbuf.buf;
      delete_string t.wbuf.buf;
      t.rbuf.buf <- "";
      t.wbuf.buf <- "";
      close t.sock (Printf.sprintf "%s after %d/%d" s t.nread t.nwrite)
    with e ->
        Printf.printf "Exception %s in TcpBufferedSocket.close" 
          (Printexc.to_string e); print_newline ();
        raise e
  end
  
let shutdown t s =
  (*
  if t.monitored then begin
      Printf.printf "shutdown"; print_newline ();
end;
  *)
  (try BasicSocket.shutdown t.sock s with e -> 
       Printf.printf "exception %s in shutdown" (Printexc.to_string e);
        print_newline () );
  (try close t s with  e -> 
        Printf.printf "exception %s in shutdown" (Printexc.to_string e);
        print_newline ())

let buf_create max = 
  {
    buf = "";
    pos = 0;
    len = 0;
    max_buf_size = max;
  } 

let error t = t.error
      
      
let set_reader t f =
  let old_handler = t.event_handler in
  let handler t ev =
(*    if t.monitored then (Printf.printf "set_reader handler"; print_newline ()); *)
    match ev with
      READ_DONE nread ->
(*        Printf.printf "READ_DONE %d" nread; print_newline (); *)
        f t nread
    |_ -> old_handler t ev
  in
  t.event_handler <- handler

let set_closer t f =
  let old_handler = t.event_handler in
  let handler t ev =
(*    if t.monitored then (Printf.printf "set_closer handler"; print_newline ()); *)
    match ev with
      BASIC_EVENT (CLOSED s) ->
(*        Printf.printf "READ_DONE %d" nread; print_newline (); *)
        f t s
    |_ -> old_handler t ev
  in
  t.event_handler <- handler

      
let buf_used t nused =
  let b = t.rbuf in
  if nused = b.len then
    ( b.len <- 0; 
      b.pos <- 0;
      delete_string b.buf;
      b.buf <- "";
      )
  else
    (b.len <- b.len - nused; b.pos <- b.pos + nused)

let set_handler t event handler =
  let old_handler = t.event_handler in
  let handler t ev =
(*    if t.monitored then (Printf.printf "set_handler handler"; print_newline ()); *)
    if ev = event then
      handler t
    else
      old_handler t ev
  in
  t.event_handler <- handler

let set_refill t f =
  set_handler t CAN_REFILL f;
  if t.wbuf.len = 0 then (try f t with _ -> ())

let buf t = t.rbuf
let sock t = t.sock
  
let closed t = closed t.sock

let buf_add t b s pos1 len =
  let curpos = b.pos + b.len in
  let max_len = 
    if b.buf = "" then
      begin
        b.buf <- new_string ();
        min_buffer_read
      end else
      String.length b.buf in
  if max_len - curpos < len then (* resize before blit *)
    if b.len + len < max_len then (* just move to 0 *)
      begin
        String.blit b.buf b.pos b.buf 0 b.len;
        String.blit s pos1 b.buf b.len len;            
        b.len <- b.len + len;
        b.pos <- 0;
      end
    else
    if b.len + len > b.max_buf_size then begin
        Printf.printf "BUFFER OVERFLOW %d+%d> %d" b.len len b.max_buf_size ; 
        print_newline ();
        
        Printf.printf "MESSAGE [";
        for i = pos1 to pos1 + (mini len 20) - 1 do
          Printf.printf "(%d)" (int_of_char s.[i]);
        done;
        if len > 20 then Printf.printf "...";
        Printf.printf "]"; print_newline ();
        
        t.event_handler t BUFFER_OVERFLOW;
      end
    else
    let new_len = mini (maxi (2 * max_len) (b.len + len)) b.max_buf_size  in
(*    if t.monitored then
      (Printf.printf "Allocate new for %d" len; print_newline ()); *)
    let new_buf = String.create new_len in
    String.blit b.buf b.pos new_buf 0 b.len;
    String.blit s pos1 new_buf b.len len;            
    b.len <- b.len + len;
    b.pos <- 0;
    if max_len = min_buffer_read then delete_string b.buf;
(*    if t.monitored then
      (Printf.printf "new buffer allocated"; print_newline ()); *)
    b.buf <- new_buf
  else begin
      String.blit s pos1 b.buf curpos len;
      b.len <- b.len + len
    end
    
let write t s pos1 len =
(*  Printf.printf "want_write %d" len; print_newline (); *)
  if len > 0 && not (closed t) then
    let pos2 = pos1 + len in
    let b = t.wbuf in
    let pos1 =
      if b.len = 0 && (match t.write_control with
            None -> 
(*              Printf.printf "NO CONTROL"; print_newline (); *)
              true
          | Some bc -> 
(*              Printf.printf "LIMIT %d" bc.total_bytes; print_newline (); *)
              bc.total_bytes = 0)
      then 
        try
(*          Printf.printf "try write %d" len; print_newline (); *)
          let nw = Unix.write (fd t.sock) s pos1 len in
(*          if t.monitored then begin
              Printf.printf "write: direct written %d" nw; print_newline (); 
end; *)
          uploaded_bytes := Int64.add !uploaded_bytes (Int64.of_int nw);
          t.nwrite <- t.nwrite + nw;
          if nw = 0 then (close t "closed on write"; pos2) else
            pos1 + nw
        with
          Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN | Unix.ENOTCONN), _, _) -> pos1
        | e ->
            t.error <- Printf.sprintf "Write Error: %s" (Printexc.to_string e);
            close t t.error;
            
(*      Printf.printf "exce %s in read" (Printexc.to_string e); print_newline (); *)
            raise e

      else pos1
    in
    if pos2 > pos1 then
      let sock = t.sock in
      must_write sock true;
      buf_add t b s pos1 (pos2 - pos1)

let write_string t s = write t s 0 (String.length s)
      
let dummy_sock = Obj.magic 0

let exn_exit = Exit
  
let can_read_handler t sock max_len =
  let b = t.rbuf in
  let curpos = b.pos + b.len in
  let can_read =
    if b.buf = "" then begin
      b.buf <- new_string ();
      min_buffer_read
    end 
    else
      if max_len - curpos < min_read_size then
	if b.len + min_read_size > b.max_buf_size then
          (
	   t.event_handler t BUFFER_OVERFLOW; 
            Printf.printf "[OVERFLOW] in %s" (info sock); 
	   close t "buffer overflow"; 
	   raise exn_exit; 
	   0
	  )
	else
	  if b.len + min_read_size < max_len then
            ( 
	     String.blit b.buf b.pos b.buf 0 b.len;
             b.pos <- 0;
             max_len - b.len
	    )
	  else
	    let new_len = mini 
		(maxi 
		   (2 * max_len) (b.len + min_read_size)) b.max_buf_size  
	    in
	    let new_buf = String.create new_len in
	    String.blit b.buf b.pos new_buf 0 b.len;
	    b.pos <- 0;
	    b.buf <- new_buf;
	    new_len - b.len
      else
	max_len - curpos
  in
  let can_read = mini max_len can_read in
  if can_read > 0 then
    let nread = try
(*        Printf.printf "try read %d" can_read; print_newline ();*)
      Unix.read (fd sock) b.buf (b.pos + b.len) can_read
    with 
      Unix.Unix_error((Unix.EWOULDBLOCK | Unix.EAGAIN), _,_) as e -> raise e
    | e ->
        t.error <- Printf.sprintf "Can Read Error: %s" (Printexc.to_string e);
        close t t.error;

(*      Printf.printf "exce %s in read" (Printexc.to_string e); print_newline (); *)
        raise e
	  
    in
    downloaded_bytes := Int64.add !downloaded_bytes (Int64.of_int nread);
    t.nread <- t.nread + nread;
    if nread = 0 then begin
      close t "closed on read";
    end else begin
      let curpos = b.pos in
      b.len <- b.len + nread;
      try
(*              if t.monitored then 
   (Printf.printf "event handler READ DONE"; print_newline ()); *)
        t.event_handler t (READ_DONE nread);
      with
      | e ->
(*                if t.monitored then
   (Printf.printf "Exception in READ DONE"; print_newline ()); *)
          t.error <- Printf.sprintf "READ_DONE Error: %s" (Printexc.to_string e);
          close t t.error;

(*      Printf.printf "exce %s in read" (Printexc.to_string e); print_newline (); *)
          raise e
    end

let can_write_handler t sock max_len =
(*      if t.monitored then (
          Printf.printf "CAN_WRITE (%d)" t.wbuf.len; print_newline ();
        ); *)
  let b = t.wbuf in
  if b.len > 0 then
    begin
      try
(*     Printf.printf "try write %d/%d" max_len t.wbuf.len; print_newline (); *)
        let nw = Unix.write (fd sock) b.buf b.pos max_len in
(*            if t.monitored then
(Printf.printf "written %d" nw; print_newline ()); *)
        uploaded_bytes := Int64.add !uploaded_bytes (Int64.of_int nw);
        t.nwrite <- t.nwrite + nw;
        b.len <- b.len - nw;
        b.pos <- b.pos + nw;
        if nw = 0 then close t "closed on write" else
        if b.len = 0 then begin
            b.pos <- 0;
            delete_string b.buf;
            b.buf <- "";
          end
      with 
        Unix.Unix_error((Unix.EWOULDBLOCK | Unix.EAGAIN), _,_) as e -> raise e
      | e ->
          t.error <- Printf.sprintf "Can Write Error: %s" (Printexc.to_string e);
          close t t.error;

(*      Printf.printf "exce %s in read" (Printexc.to_string e); print_newline (); *)
          raise e
    
    end;
  if not (closed t) then begin
      t.event_handler t CAN_REFILL;
      if b.len = 0 then begin
          delete_string b.buf;
          b.pos <- 0;
          must_write t.sock false;
          t.event_handler t WRITE_DONE
        end
    end      

let remaining_to_write t =
  let b = t.wbuf in
  b.len
    
let tcp_handler t sock event = 
  match event with
  | CAN_READ ->
(*      Printf.printf "CAN_READ"; print_newline (); *)
      begin
        match t.read_control with
          None ->
            can_read_handler t sock (String.length t.rbuf.buf)
        | Some bc ->
            if bc.total_bytes = 0 then 
              can_read_handler t sock (String.length t.rbuf.buf)
            else begin
(*                Printf.printf "DELAYED"; print_newline (); *)
                if bc.remaining_bytes > 0 then
                  begin
                    bc.connections <- t :: bc.connections;
                    bc.nconnections <- 1 + bc.nconnections
                  end
              end
      end
  | CAN_WRITE ->
(*      Printf.printf "CAN_WRITE"; print_newline (); *)
      begin
        match t.write_control with
          None ->
            can_write_handler t sock t.wbuf.len
        | Some bc ->
            if bc.total_bytes = 0 then
              can_write_handler t sock t.wbuf.len
            else  begin
(*                Printf.printf "DELAYED"; print_newline (); *)
            if bc.remaining_bytes > 0 then begin
                bc.connections <- t :: bc.connections;
                bc.nconnections <- t.write_power + bc.nconnections
                  end
              end
      end
  | _ -> t.event_handler t (BASIC_EVENT event)

let read_bandwidth_controlers = ref []
let write_bandwidth_controlers = ref []
  
      
let create_read_bandwidth_controler rate = 
  let bc = {
      remaining_bytes = rate;
      total_bytes = rate;
      nconnections = 0;
      connections = [];
      allow_io = ref true;
      last_remaining = 0;
    } in
  read_bandwidth_controlers := bc :: !read_bandwidth_controlers;
  bc
      
let create_write_bandwidth_controler rate = 
  let bc = {
      remaining_bytes = rate;
      total_bytes = rate;
      nconnections = 0;
      connections = [];
      allow_io = ref true;
      last_remaining = 0;
    } in
  write_bandwidth_controlers := bc :: !write_bandwidth_controlers;
  bc

let change_rate bc rate =
  bc.total_bytes <- rate

let bandwidth_controler t sock = 
  (match t.read_control with
      None -> ()
    | Some bc ->
        must_read sock (bc.total_bytes = 0 || bc.remaining_bytes > 0));
  (match t.write_control with
      None -> ()
    | Some bc ->
        must_write sock ((bc.total_bytes = 0 || bc.remaining_bytes > 0)
          && t.wbuf.len > 0))  
  
let set_read_controler t bc =
  t.read_control <- Some bc;
(*  set_before_select t.sock (bandwidth_controler t); *)
  set_allow_read t.sock bc.allow_io;
  bandwidth_controler t t.sock
  
let set_write_controler t bc =
  t.write_control <- Some bc;
(*  set_before_select t.sock (bandwidth_controler t); *)
  set_allow_write t.sock bc.allow_io;
  bandwidth_controler t t.sock
  
let max_buffer_size = ref 100000 
  
let create name fd handler =
  if !debug then begin
      Printf.printf "[fd %d %s]" (Obj.magic fd) name; print_newline ();
    end;
  Unix.set_close_on_exec fd;
  let t = {
      sock = dummy_sock;
      rbuf = buf_create !max_buffer_size;
      wbuf = buf_create !max_buffer_size;
      event_handler = handler;
      error = "";
      nread = 0;
      nwrite = 0;
      monitored = false;
      read_control = None;
      write_control = None;
      write_power = 1;
    } in
  let sock = BasicSocket.create name fd (tcp_handler t) in
  let name = (fun () ->
        Printf.sprintf "%s (nread: %d nwritten: %d)" name t.nread t.nwrite
    ) in
  set_printer sock name;
  t.sock <- sock;
  t

let create_blocking name fd handler =
  Unix.set_close_on_exec fd;
  let t = {
      sock = dummy_sock;
      rbuf = buf_create !max_buffer_size;
      wbuf = buf_create !max_buffer_size;
      event_handler = handler;
      error = "";
      nread = 0;
      nwrite = 0;
      monitored = false;
      read_control = None;
      write_control = None;
      write_power = 1;
    } in
  let sock = create_blocking name fd (tcp_handler t) in
  t.sock <- sock;
  t
  
let create_simple name fd =
  create name fd (fun _ _ -> ())
  
let connect name host port handler =
  try
(*   Printf.printf "CONNECT"; print_newline ();*)
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let t = create name s handler in
    must_write (sock t) true;
    try
      Unix.connect s (Unix.ADDR_INET(host,port));
      t
    with 
      Unix.Unix_error((Unix.EINPROGRESS|Unix.EINTR),_,_) -> t
    | e -> 
        Printf.printf "+++ Exception while CONNECT %s" (Printexc.to_string e);
        print_newline ();
        t
  with e -> 
      Printf.printf "+++ Exception BEFORE CONNECT %s" (Printexc.to_string e);
      print_newline ();
      raise e

  
  
let exec_command cmd args handler =
  let (in_read, output) = Unix.pipe() in
  let (input, out_write) = Unix.pipe() in
  match Unix.fork() with
    0 -> begin
        try
          match Unix.fork () with
            0 -> begin
                try
                  if input <> Unix.stdin then
                    begin Unix.dup2 input Unix.stdin; Unix.close input end;
                  if output <> Unix.stdout then
                    begin Unix.dup2 output Unix.stdout; Unix.close output end;
                  Unix.close in_read;
                  Unix.close out_write;
                  Unix.execv cmd args;
                  exit 127
                with e -> 
                    Printf.eprintf "Exception %s in exec_command\n"
                      (Printexc.to_string e) ; 
                    exit 1
              end
          | id -> 
              exit 2
        with _ -> 
            exit 3
      end
  | id -> 
      ignore (snd(Unix.waitpid [] id));
      Unix.close input;
      Unix.close output;
      let t_in = create "pipe_int" in_read handler in
      let t_out = create "pipe_out" out_write (fun _ _ -> ()) in
      must_read (sock t_out) false;
      (t_in, t_out)
  
let set_max_write_buffer t len =
  t.wbuf.max_buf_size <- len;
  t.rbuf.max_buf_size <- len
  
let can_write t =
  t.wbuf.len = 0

let can_write_len t len =
(*  Printf.printf "CAN WRITE %d > %d + %d"  t.wbuf.max_buf_size t.wbuf.len len; print_newline (); *)
  t.wbuf.max_buf_size > t.wbuf.len + len
  
let close_after_write t =
  if t.wbuf.len = 0 then begin
      shutdown t "close after write"
    end
  else
    set_handler t WRITE_DONE (fun t -> 
        shutdown t "close after write")

let set_monitored t =
  t.monitored <- true
  
  
let _ =
  add_infinite_timer 1.0 (fun timer ->
      List.iter (fun bc ->
          bc.last_remaining <- bc.remaining_bytes;
          bc.remaining_bytes <- bc.total_bytes;
          if bc.remaining_bytes > 0 then bc.allow_io := true
(*            Printf.printf "READ remaining_bytes: %d" bc.remaining_bytes;  *)
      ) !read_bandwidth_controlers;
      List.iter (fun bc ->
          bc.last_remaining <- bc.remaining_bytes;
          bc.remaining_bytes <- bc.total_bytes;          
          if bc.remaining_bytes > 0 then bc.allow_io := true
(*          Printf.printf "WRITE remaining_bytes: %d" bc.remaining_bytes;  
          print_newline (); *)
      ) !write_bandwidth_controlers
  );

  set_before_select_hook (fun _ ->
      List.iter (fun bc ->
          bc.allow_io := (bc.total_bytes = 0 || bc.remaining_bytes > 0);
      ) !read_bandwidth_controlers;
        List.iter (fun bc ->
          bc.allow_io := (bc.total_bytes = 0 || bc.remaining_bytes > 0);
      ) !write_bandwidth_controlers;
  );

  set_after_select_hook (fun _ ->
      List.iter (fun bc ->
          List.iter (fun t ->
              if bc.remaining_bytes > 0 then
                let can_read = maxi 1 (bc.remaining_bytes / bc.nconnections) in
                let old_nread = t.nread in
                (try
                    can_read_handler t t.sock (mini can_read 
                        (String.length t.rbuf.buf)) 
                  with _ -> ());
                bc.remaining_bytes <- bc.remaining_bytes - 
                t.nread + old_nread;
                bc.nconnections <- bc.nconnections - 1;
          ) bc.connections;
          if bc.remaining_bytes = 0 then bc.allow_io := false;
          bc.connections <- [];
          bc.nconnections <- 0;
      ) !read_bandwidth_controlers;
      List.iter (fun bc ->
(*          Printf.printf "Nconn: %d" bc.nconnections; print_newline (); *)
          List.iter (fun t ->
              if bc.remaining_bytes > 0 then
                let can_write = maxi 1 (bc.remaining_bytes / bc.nconnections) in
                let can_write = can_write * t.write_power in
                let old_nwrite = t.nwrite in
                (try
(*                    Printf.printf "WRITE"; print_newline (); *)
                    can_write_handler t t.sock (mini can_write t.wbuf.len)
                  with _ -> ());
                bc.remaining_bytes <- bc.remaining_bytes - 
                t.nwrite + old_nwrite;
                bc.nconnections <- bc.nconnections - t.write_power;
          ) bc.connections;
          if bc.remaining_bytes = 0 then bc.allow_io := false;
          bc.connections <- [];
          bc.nconnections <- 0;
      ) !write_bandwidth_controlers
  )


let my_ip t =
  let fd = fd t.sock in
  match Unix.getsockname fd with
    Unix.ADDR_INET (ip, port) -> Ip.of_inet_addr ip
  | _ -> raise Not_found
      
let stats buf t =
  BasicSocket.stats buf t.sock;
  Printf.bprintf buf "  rbuf size: %d/%d\n" (String.length t.rbuf.buf)
  t.rbuf.max_buf_size;
  Printf.bprintf buf "  wbuf size: %d/%d\n" (String.length t.wbuf.buf)
  t.wbuf.max_buf_size

let buf_size  t =
  (String.length t.rbuf.buf),
  (String.length t.wbuf.buf)
  
let can_fill t =
  t.wbuf.len < (t.wbuf.max_buf_size / 2)
  
let if_possible bc len = 
  bc.total_bytes = 0 ||
  if bc.last_remaining >= len then begin
      bc.last_remaining <- bc.last_remaining - len;
      true;
    end else false
    
let set_rtimeout s t = set_rtimeout (sock s) t
let set_wtimeout s t = set_wtimeout (sock s) t

open LittleEndian
   
let internal_buf = Buffer.create 17000

let simple_send_buf buf sock =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  buf_int8 buf 228;
  let len = String.length s in
  buf_int buf len;
  write sock (Buffer.contents buf) 0 5;
  write sock s 0 len

let value_send sock m =
  Buffer.clear internal_buf;
  Buffer.add_string internal_buf (Marshal.to_string m []);
  simple_send_buf internal_buf sock

let value_handler f sock nread =
  let b = buf sock in
  try
    while b.len >= 5 do
      let msg_len = get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          let s = String.sub b.buf (b.pos+5) msg_len in
          let t = Marshal.from_string  s 0 in
          buf_used sock  (msg_len + 5);
          f t sock;
          ()
        end
      else raise Not_found
    done
  with Not_found -> ()

let set_write_power t p = t.write_power <- p
  
let set_lifetime s = set_lifetime (sock s)
  
  