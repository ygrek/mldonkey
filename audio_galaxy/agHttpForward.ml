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
open TcpBufferedSocket

open Options
open AgOptions

let verbose = false
  
let start () =
  
  let dest = Ip.from_name "www.audiogalaxy.com" in  
  
  let sock = TcpServerSocket.create "audiogalaxy http server"
    Unix.inet_addr_any !!http_port
      (fun sock event ->
        match event with
          TcpServerSocket.CONNECTION(s, Unix.ADDR_INET(from_ip, from_port)) ->
            Printf.printf "CONNECTION"; print_newline ();
            let http_client = TcpBufferedSocket.create 
              "audiogalaxy http to client"
              s (fun _ _ -> ()) in
            let http_server = TcpBufferedSocket.connect
                "audiogalaxy http to server"
                (Ip.to_inet_addr dest) 80 (fun _ _ -> ())
            in
            let done_header = ref false in
            TcpBufferedSocket.set_reader http_client (fun sock nread ->
                let b = TcpBufferedSocket.buf sock in
                if !done_header then begin
                    write http_server b.buf b.pos b.len;
                    buf_used http_client b.len
                  end else
                let begin_pos = b.pos + b.len - nread in
                
                if verbose then begin                
                    Printf.printf "SENT CLIENT-->SERVER %d bytes" b.len;
                    print_newline ();
                    AgProtocol.dump (String.sub b.buf b.pos b.len);
                  end;
                
                let end_pos = b.pos + b.len in
                let rec iter pos =
                  if pos < end_pos then
                    if b.buf.[pos] = '\n' then begin
                        let nsent = pos - b.pos + 1 in
                        if pos > b.pos + 10 && 
                            String.lowercase (String.sub b.buf b.pos 8) 
                            = "referer:"
                        then 
                          begin
                            let s = String.sub b.buf (b.pos+9) (nsent - 9) in
                            if verbose then begin
                                Printf.printf "DISCARD referer: [%s]" s; print_newline (); 
                              end;
                            let len = String.length s in
                            if len > 10 && String.sub s 0 7 = "http://" then
                              try
                                let pos = String.index_from s 7 '/' in
                                if verbose then begin
                                    Printf.printf "POSITION: %d" pos;
                                    print_newline ();
                                  end;
                                let tosend =
                                  ("Referer: http://www.audiogalaxy.com:80" ^ 
                                      (String.sub s pos (len - pos)))
                                in
                                if verbose then begin
                                Printf.printf "SENDING [%s]" tosend;
                                    print_newline ();
                                  end;
                                write_string http_server tosend

                                
                              with _ -> 
                                  if verbose then begin
                                      Printf.printf "DISCARDED"; 
                                      print_newline ();
                                    end
                              
                          end
                        else 
                        if not (pos > b.pos + 5 && 
                            String.lowercase (String.sub b.buf b.pos 5) 
                            = "host:")
                        then begin
                            if nsent < 3 then begin
                                done_header := true;
                                if verbose then begin
                                    Printf.printf "DONE WITH HEADER"; 
                                    print_newline ();
                                  end;
                                write http_server b.buf b.pos b.len;
                                buf_used http_client b.len;
                                raise Exit
                              end else
                              write http_server b.buf b.pos nsent
                          end
                        else begin
                            write_string http_server 
                              "Host: www.audiogalaxy.com:80\r\n";
                            if verbose then begin
                                Printf.printf "CHANGE HOST"; 
                                print_newline (); 
                              end;
                          end;
                        if verbose then begin
                            AgProtocol.dump (String.sub b.buf b.pos nsent); 
                            print_newline ();
                          end;
                        buf_used http_client nsent;
                        iter (pos+1)
                      end
                    else
                      iter (pos+1)
                in
                try
                  iter begin_pos;
                with Exit -> ()
            );
            TcpBufferedSocket.set_reader http_server (fun sock nread ->
                let b = TcpBufferedSocket.buf sock in
                TcpBufferedSocket.write http_client
                  b.buf b.pos b.len;
                if verbose then begin
                    Printf.printf "SENT SERVER-->CLIENT %d bytes" b.len;
                    print_newline ();
                  end;
  
                TcpBufferedSocket.buf_used sock b.len;

            );
            TcpBufferedSocket.set_closer http_client (fun sock s ->
                if verbose then begin
                    Printf.printf "CONNECTION FROM CLIENT CLOSED"; 
                    print_newline ();
                  end;
                TcpBufferedSocket.close http_server s
            );
            TcpBufferedSocket.set_closer http_server (fun sock s ->
                if verbose then begin
                    Printf.printf "CONNECTION FROM SERVER CLOSED"; 
                    print_newline ();
                  end;
                let b = TcpBufferedSocket.buf http_client in
                if b.len = 0 then
                  TcpBufferedSocket.close http_client "closed by server"
                else
                  TcpBufferedSocket.set_handler http_client WRITE_DONE (fun _ ->
                      TcpBufferedSocket.close http_client "closed by server");
            );
        | _ -> ())
  in
  ()
