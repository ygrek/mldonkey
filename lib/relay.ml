open Unix
open Netbase
open TcpClientSocket

(* let _ = Unix2.init () *)
  
type fake =
  Server
| Client
| Raw
  
type proto = Udp | Tcp
  
let ports = 
  [ 
    4800, 4700, Tcp, Server;
  ]
  
let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)
    
let buf = Buffer.create 65000

let client_to_server port_in port_out out_t in_t curpos =
  let module Proto= Mftp_server in
  let b = TcpClientSocket.buf in_t in
  try
  while b.len >= 5 do
      let msg_len = Mftp.get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          Printf.printf "client_to_server %d --> %d" port_in port_out; print_newline ();
          let s = String.sub b.buf (b.pos+5) msg_len in
          TcpClientSocket.buf_used in_t  (msg_len + 5);
          let t = Proto.parse s in
          Proto.print t;
          Proto.write buf t;
          let s = Buffer.contents buf in
          Buffer.clear buf;
          Mftp.buf_int8 buf 227;
          let len = String.length s in
          Mftp.buf_int32_32 buf (Int32.of_int len);
          TcpClientSocket.write out_t (Buffer.contents buf) 0 5;
          TcpClientSocket.write out_t s 0 len;
        end
      else raise Not_found
    done
  with Not_found -> ()

let client_to_client port_in port_out out_t in_t curpos =
  let module Proto= Mftp_client in
  let b = TcpClientSocket.buf in_t in
  try
  while b.len >= 5 do
      let msg_len = Mftp.get_int b.buf (b.pos+1) in
      if b.len >= 5 + msg_len then
        begin
          Printf.printf "client_to_client %d --> %d" port_in port_out; print_newline ();
          let s = String.sub b.buf (b.pos+5) msg_len in
          TcpClientSocket.buf_used in_t  (msg_len + 5);
          let t = Proto.parse s in
          Proto.print t;
          Proto.write buf t;
          let s = Buffer.contents buf in
          Buffer.clear buf;
          Mftp.buf_int8 buf 227;
          let len = String.length s in
          Mftp.buf_int32_32 buf (Int32.of_int len);
          TcpClientSocket.write out_t (Buffer.contents buf) 0 5;
          TcpClientSocket.write out_t s 0 len;
        end
      else raise Not_found
    done
  with Not_found -> ()

let raw_forward port_in port_out out_t in_t curpos =
  Printf.printf "raw forward %d --> %d" port_in port_out; print_newline ();
  let b = TcpClientSocket.buf in_t in
  Printf.printf "dec [";
  for i = b.pos to b.pos + b.len - 1 do
    let c = b.buf.[i] in
    let n = int_of_char c in
    Printf.printf "(%d)" n
  done;
  Printf.printf "]\nascii [";
  for i = b.pos to b.pos + b.len - 1 do
    let c = b.buf.[i] in
    let n = int_of_char c in
    if n > 31 && n < 127 then
      Printf.printf "%c" c
    else
      Printf.printf "(%d)" n
  done;
  Printf.printf "]"; print_newline ();
  TcpClientSocket.write out_t b.buf b.pos b.len;
  b.pos <- 0;
  b.len <- 0

let dummy_handler _ _ = ()
  
let handler port1 port2 server t event =
  Printf.printf "CONNECTION TO port %d" port1; print_newline ();
  match event with
    TcpServerSocket.CONNECTION (in_fd, Unix.ADDR_INET(_, port0)) ->
      let out_fd = TcpClientSocket.connect Host.local_host port2
          dummy_handler in
      Printf.printf "CONNECTING port %d" port2; print_newline ();
      
      let in_fd = TcpClientSocket.create in_fd dummy_handler in
      
      begin
        match server with
          Server ->
            TcpClientSocket.set_reader in_fd
            (client_to_server port0 port2 out_fd);
            TcpClientSocket.set_reader out_fd
            (client_to_server port2 port0 in_fd);
        | Client ->
            TcpClientSocket.set_reader in_fd (
              client_to_client port0 port2 out_fd);
            TcpClientSocket.set_reader out_fd (
              client_to_client port0 port2 in_fd);
        | Raw ->
            TcpClientSocket.set_reader in_fd (
              raw_forward port0 port2 out_fd);
            TcpClientSocket.set_reader out_fd (
              raw_forward port0 port2 in_fd);
      end
  | _ -> ()
  
let udp_sockets = ref []
      
let rec udp_handler port1 port2 server t event =
  match event with
    UdpSocket.READ_DONE ->
      List.iter (fun p ->
          let dest =
            match p.UdpSocket.addr with
              Unix.ADDR_INET(_, port0) ->
                Printf.printf "MESSAGE %d --> %d --> %d" port0 port1 port2;
                print_newline ();
                begin
                  try
                    List.assoc port0 !udp_sockets
                  with Not_found ->
                      let t = UdpSocket.create 0 (
                          udp_handler 0 port0 server) 
                      in 
                      udp_sockets := (port0,t) :: !udp_sockets;
                      t
                end
            | _ -> assert false
          in
          Mftp.dump p.UdpSocket.content;
          UdpSocket.write dest p.UdpSocket.content 0 (
            String.length p.UdpSocket.content) (
            Unix.ADDR_INET (Host.local_ip, port2))
      ) t.UdpSocket.rlist;
      t.UdpSocket.rlist <- []
  | _ -> ()
      
  
  
  
let _ =
  Mftp.ports := List.map (fun (port1, port2, _, _) ->
      (port2, port1)
  ) ports;
  
  List.iter (fun (port1, port2, proto, server) ->
      match proto with
        Udp ->
          
          let t = UdpSocket.create  port1 (udp_handler port1 port2 server)
          in 
          udp_sockets := (port2,t) :: !udp_sockets
      | _ ->
          ignore(TcpServerSocket.create  port1 (handler port1 port2 server))
  )
  ports;
  BasicSocket.loop ()
  
