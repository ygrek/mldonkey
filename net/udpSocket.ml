
open BasicSocket

type event = 
  WRITE_DONE
| CAN_REFILL
| READ_DONE 
| BASIC_EVENT of BasicSocket.event

type udp_packet = {
    content: string;
    addr: Unix.sockaddr;
(*
    val sendto : Unix.file_descr -> string -> int -> int ->
       Unix.msg_flag MSG_DONTWAITlist -> Unix.sockaddr -> int
*)
  }
  
type t = {
    mutable sock : BasicSocket.t;
    mutable rlist : udp_packet list;
    mutable wlist : udp_packet list;
    mutable event_handler : handler;
  }
  
and handler = t -> event -> unit

let read t = 
  match t.rlist with
    [] -> raise Not_found
  | p :: l ->
      t.rlist <- l;
      p

let set_handler t event handler =
  let old_handler = t.event_handler in
  let handler t ev =
    if ev = event then
      handler t
    else
      old_handler t ev
  in
  t.event_handler <- handler

let set_refill t f =
  set_handler t CAN_REFILL f;
  match t.wlist with
    [] -> (try f t with _ -> ())
  | _ -> ()
    
let set_reader t f =
  set_handler t READ_DONE f;
  match t.rlist with
    [] -> ()
  | _ -> (try f t with _ -> ())

let sock t = t.sock
let closed t = closed t.sock
let close t = close t.sock
    
let write t s pos len addr =
  if not (closed t) then 
    match t.wlist with
      [] ->
        begin
          let sock = sock t in
          try
            ignore (
              Unix.sendto (fd sock) s pos len [] addr);
            (*
            Printf.printf "UDP sent [%s]" (String.escaped
              (String.sub s pos len)); print_newline ();
*)
          with
            Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> 
              t.wlist <- {
                content = String.sub s  pos len;
                addr = addr;
                } :: t.wlist;
              must_write sock true;              
        end
    | _ -> 
        t.wlist <- {
          content = String.sub s  pos len;
          addr = addr;
        } :: t.wlist
      
let dummy_sock = Obj.magic 0

let buf = String.create 66000
  
let udp_handler t sock event = 
  Printf.printf "udp_handler EVENT"; print_newline ();
  match event with
  | CAN_READ ->
      let (len, addr) = Unix.recvfrom (fd sock) buf 0 66000 [] in
      t.rlist <- {
        content = String.sub buf 0 len;
        addr = addr;
      } :: t.rlist;
      t.event_handler t READ_DONE
     
  | CAN_WRITE ->
      begin
        match t.wlist with
          [] -> ()
        | p :: l ->
            t.wlist <- l;
            try
              ignore (
                Unix.sendto (fd sock) p.content 0 (String.length p.content)
                [] p.addr);
(*
  Printf.printf "UDP sent after [%s]" (String.escaped
p.content); print_newline ();
  *)
            with
              Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> 
                t.wlist <- p :: l
      end;
      if not (closed t) then begin
          t.event_handler t CAN_REFILL;
          if t.wlist = [] then begin
              must_write t.sock false;
              t.event_handler t WRITE_DONE
            end
        end      
  | _ -> t.event_handler t (BASIC_EVENT event)

let create port handler =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, port));
  let t = {
      rlist = [];
      wlist = [];
      sock = dummy_sock;
      event_handler = handler;
    } in
  let sock = BasicSocket.create fd (udp_handler t) in
  t.sock <- sock;
  t
    
