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

open LittleEndian
open CommonTypes
open CommonGlobals
open DonkeyMftp

module Connect = struct 
    type t = {
        md4 : Md4.t;
        ip: Ip.t;
        port: int;
        tags : tag list;
      }
    
    let names_of_tag =
      [
        1, "name";
        17, "version";
        15, "port";
      ]
    
    let parse len s =
      let md4 = get_md4 s 1 in
      let ip = get_ip s 17 in
      let port = get_port s 21 in
(*      Printf.printf "port: %d" port; print_newline ();*)
      let ntags = get_int s 23 in
      let tags, pos = get_tags s 27 ntags names_of_tag in
      {
        md4 = md4;
        ip = ip;
        port = port;
        tags = tags;
      }
    
    let print t = 
      Printf.printf "CONNECT:\n";
      Printf.printf "MD4: %s\n" (Md4.to_string t.md4); 
      Printf.printf "ip: %s\n" (Ip.to_string t.ip);
      Printf.printf "port: %d\n" t.port;
      Printf.printf "tags: ";
      print_tags t.tags
    
    let fprint oc t = 
      Printf.fprintf oc "CONNECT:\n";
      Printf.fprintf oc "%s\n" (Md4.to_string t.md4); 
      Printf.fprintf oc "%s\n" (Ip.to_string t.ip);
      Printf.fprintf oc "%d\n" t.port;
      Printf.fprintf oc "TAGS:\n";
      fprint_tags oc t.tags;
       Printf.fprintf oc "\n"

    let write buf t =
      buf_md4 buf t.md4;
      buf_ip buf t.ip;
      buf_port buf t.port;
      let ntags = List.length t.tags in
      buf_int buf ntags;
      buf_tags buf t.tags names_of_tag  
  end

module ChatRooms = struct (* request: 57 *)
    
    (* example:
ascii [ 9(3)(4)(0) M a i n(0)(0)(2)(0)(5)(0) M u s i c(0)(0)(3)(0)(3)(0) A r t(0)(0)(4)(0)] 
*)
    
    
    type channel = {
        name : string;
        number : int;
      }
    
    type t = channel list
      
    let parse len s =
      let nchans = get_int8 s 1 in
      let rec iter s pos nchans =
        if nchans = 0 then [] else
        let name, pos = get_string s pos in
        let number = get_int s pos in
        { name = name; number = number; } :: (iter s (pos+4) (nchans-1))
      in
      iter s 2 nchans

    let print t =
      Printf.printf "CHANNELS:\n";
      List.iter (fun c ->
          Printf.printf "  %s: %d" c.name c.number;
          print_newline ();
      ) t

     let fprint oc t =
      Printf.fprintf oc "CHANNELS:\n";
      List.iter (fun c ->
          Printf.fprintf oc "  %s: %d" c.name c.number;
          print_newline ();
      ) t

    let write buf t =
      buf_int buf (List.length t);
      List.iter (fun c ->
          buf_string buf c.name;
          buf_int buf c.number) t
      
  end
  
module SetID = struct 
    type t = Ip.t
      
    
    let parse len s = get_ip s 1 
    
    let print t = 
      Printf.printf "SET_ID:\n";
      Printf.printf "id: %s\n" (Ip.to_string t)

    let fprint oc t = 
      Printf.fprintf oc "SET_ID:\n";
      Printf.fprintf oc "id: %s\n" (Ip.to_string t)
    
    let write buf t =
      buf_ip buf t
  
  end

let unit = ()

module AckID = struct 
    type t = unit
    
    let parse len s = ()
    
    let print t = 
      Printf.printf "ACK_ID:\n"

    let fprint oc t = 
      Printf.fprintf oc "ACK_ID\n"
    
    let write (buf: Buffer.t) (t: t) = unit

    let t = (() : t)
  end

module Message = struct 
    type t = string
    
    let parse len s = 
      let v, pos = get_string s 1 in
      v
    
    let print t = 
      Printf.printf "MESSAGE:\n";
      Printf.printf "message = \"%s\"" (String.escaped t)
    
    let fprint oc t = 
      Printf.fprintf oc "MESSAGE:\n";
      Printf.fprintf oc "%s\n" (String.escaped t)

    let write buf t =
      buf_string buf t
  end

module Share = struct 
    
    type t = tagged_file list
    
    let names_of_tag =
      [
        1, "filename";
        2, "size";
        3, "type";
        4, "format";
      ]
    
    let rec get_files  s pos n =
      if n = 0 then [], pos else
      let md4 = get_md4 s pos in
      let ip = get_ip s (pos + 16) in
      let port = get_port s (pos + 20) in
      let ntags = get_int s (pos+22) in
      let tags, pos = get_tags s (pos+26) ntags names_of_tag in
      let file = {
          f_md4 = md4;
          f_ip = ip;
          f_port = port;
          f_tags = tags;
        } in
      let files, pos =  get_files s pos (n-1) in
      file :: files, pos
    
    
    let parse len s =
      let n = get_int s 1 in
      let files, pos = get_files s 5 n in
      files
    
    let print t = 
      Printf.printf "SHARED:\n";
      List.iter (fun t ->
          Printf.printf "FILE:\n";
          Printf.printf "  MD4: %s\n" (Md4.to_string t.f_md4);
          Printf.printf "  ip: %s\n" (Ip.to_string t.f_ip);
          Printf.printf "  port: %d\n" t.f_port;
          Printf.printf "  tags: ";
          print_tags t.f_tags;
          print_newline ();) t
    
    let fprint oc t = 
      Printf.fprintf oc "SHARED:\n";
      List.iter (fun t ->
          Printf.fprintf oc "FILE:\n";
          Printf.fprintf oc " %s\n" (Md4.to_string t.f_md4);
          Printf.fprintf oc "%s\n" (Ip.to_string t.f_ip);
          Printf.fprintf oc "%d\n" t.f_port;
          Printf.fprintf oc "TAGS:\n";
          fprint_tags oc t.f_tags;
	  Printf.fprintf oc "\n"
          ) t

    let rec write_files buf files =
      match files with
        [] -> ()
      | file :: files ->
          buf_md4 buf file.f_md4;
          buf_ip buf file.f_ip;
          buf_port buf file.f_port;
          buf_int buf (List.length file.f_tags);
          buf_tags buf file.f_tags names_of_tag;
          write_files buf files
    
    let write buf t = 
      buf_int buf (List.length t);
      write_files buf t
  end

module Info = struct 
    type t = int * int
    
    let parse len s =
      let users = get_int s 1 in
      let files = get_int s 5 in
      users, files
    
    let print (users, files) = 
      Printf.printf "INFO:\n";
      Printf.printf "users: %d files: %d\n"  users files

   let fprint oc (users, files) = 
      Printf.fprintf oc "INFO:\n";
      Printf.fprintf oc "%d\n %d\n"  users files
    
    let write buf (users, files) =
      buf_int buf users;
      buf_int buf files
  end

module ServerList = struct 
    type server = {
        ip : Ip.t;
        port : int;
      }
    
    type t = server list
      
    let parse len s = 
      let n = get_int8 s 1 in
      let rec iter i  =
        if i = n then [] else
        let ip = get_ip s (2 + i * 6) in
        let port = get_port s (6+ i * 6) in
        { ip = ip; port = port; } :: (iter (i+1))
      in
      iter 0
          
    let print t = 
      Printf.printf "SERVER LIST";
      print_newline ();
      List.iter (fun l -> 
          Printf.printf "   %s : %d" (Ip.to_string l.ip) l.port;
          print_newline ();
      ) t

    let fprint oc t = 
      Printf.fprintf oc "SERVER LIST\n";
      List.iter (fun l -> 
          Printf.fprintf oc "%s:%d\n" (Ip.to_string l.ip) l.port;
      ) t
      
    let write buf t = 
      buf_int8 buf (List.length t);
      List.iter (fun l ->
          buf_ip buf l.ip;
          buf_int16 buf l.port
      ) t
  end

  
module DBServerList = struct 
    type server = {
        ip : Ip.t;
        port : int;
      }
    
    type t = server list
      
    let parse len s = 
      let n = get_int16 s 1 in
      let rec iter i  =
        if i = n then [] else
        let ip = get_ip s (3 + i * 6) in
        let port = get_port s (7+ i * 6) in
        { ip = ip; port = port; } :: (iter (i+1))
      in
      iter 0
          
    let print t = 
      Printf.printf "SERVER LIST";
      print_newline ();
      List.iter (fun l -> 
          Printf.printf "   %s : %d" (Ip.to_string l.ip) l.port;
          print_newline ();
      ) t

    let fprint oc t = 
      Printf.fprintf oc "SERVER LIST\n";
      List.iter (fun l -> 
          Printf.fprintf oc "%s:%d\n" (Ip.to_string l.ip) l.port;
      ) t
      
    let write buf t = 
      buf_int16 buf (List.length t);
      List.iter (fun l ->
          buf_ip buf l.ip;
          buf_int16 buf l.port
      ) t
  end


module ServerInfo = struct 
    type t = {
        md4 : Md4.t;
        ip: Ip.t;
        port: int;
        tags : tag list;
      }
    
    let names_of_tag =
      [
        1, "name";
        11, "description";
      ]
    
    let parse len s =
      let md4 = get_md4 s 1 in
      let ip = get_ip s 17 in
      let port = get_port s 21 in
(*      Printf.printf "port: %d" port; print_newline (); *)
      let ntags = get_int s 23 in
      let tags, pos = get_tags s 27 ntags names_of_tag in
      {
        md4 = md4;
        ip = ip;
        port = port;
        tags = tags;
      }
    
    let print t = 
      Printf.printf "SERVER INFO:\n";
      Printf.printf "MD4: %s\n" (Md4.to_string t.md4);
      Printf.printf "ip: %s\n" (Ip.to_string t.ip);
      Printf.printf "port: %d\n" t.port;
      Printf.printf "tags: ";
      print_tags t.tags

    let fprint oc t = 
      Printf.fprintf oc "SERVER INFO:\n";
      Printf.fprintf oc "%s\n" (Md4.to_string t.md4);
      Printf.fprintf oc "%s\n" (Ip.to_string t.ip);
      Printf.fprintf oc "%d\n" t.port;
      Printf.fprintf oc "TAGS:\n";
      fprint_tags oc t.tags;
      Printf.fprintf oc "\n"
    
    let write buf t =
      buf_md4 buf t.md4;
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_int buf (List.length t.tags);
      buf_tags buf t.tags names_of_tag
  
  end

module QueryReply  = struct 
      
    type t = tagged_file list

    let names_of_tag = [
        1, "filename";
        2, "size";
        3, "type";
        4, "format";
        21, "availability";
      ]        

          
    let rec get_files  s pos n =
      if n = 0 then [], pos else
      let md4 = get_md4 s pos in
      let ip = get_ip s (pos + 16) in
      let port = get_port s (pos + 20) in
      let ntags = get_int s (pos+22) in
      let tags, pos = get_tags s (pos+26) ntags names_of_tag in
      let file = {
          f_md4 = md4;
          f_ip = ip;
          f_port = port;
          f_tags = tags;
        } in
      let files, pos =  get_files s pos (n-1) in
      file :: files, pos
    

    let get_replies s pos = 
      let n = get_int s pos in
      let files, pos = get_files s (pos+4) n in
      files
    
    let parse len s = get_replies s 1
    
    let print t = 
      Printf.printf "FOUND:\n";
      List.iter (fun t ->
          Printf.printf "FILE:\n";
          Printf.printf "  MD4: %s\n" (Md4.to_string t.f_md4);
          Printf.printf "  ip: %s\n" (Ip.to_string t.f_ip);
          Printf.printf "  port: %d\n" t.f_port;
          Printf.printf "  tags: ";
          print_tags t.f_tags;
          print_newline ();) t

    let fprint oc t = 
      Printf.fprintf oc "FOUND:\n";
      List.iter (fun t ->
          Printf.fprintf oc "FILE:\n";
          Printf.fprintf oc "%s\n" (Md4.to_string t.f_md4);
          Printf.fprintf oc "%s\n" (Ip.to_string t.f_ip);
          Printf.fprintf oc "%d\n" t.f_port;
          Printf.fprintf oc "TAGS:\n";
          fprint_tags oc t.f_tags;
             Printf.fprintf oc "\n"
          ) t
    
    let rec write_files buf files =
      match files with
        [] -> ()
      | file :: files ->
          buf_md4 buf file.f_md4;
          buf_ip buf file.f_ip;
          buf_port buf file.f_port;
          buf_int buf (List.length file.f_tags);
          buf_tags buf file.f_tags names_of_tag;
          write_files buf files

    let write_replies buf t =          
      buf_int buf (List.length t);
      write_files buf t
      
      let write buf t = 
        write_replies buf t;
      buf_int8 buf 0

  end

module QueryUdpReply  = struct 
    
    type t = tagged_file
    
    let names_of_tag = [
        1, "filename";
        2, "size";
        3, "type";
        4, "format";
        21, "availability";
      ]        
    
    
    let get_file  s pos =
      let md4 = get_md4 s pos in
      let ip = get_ip s (pos + 16) in
      let port = get_port s (pos + 20) in
      let ntags = get_int s (pos+22) in
      let tags, pos = get_tags s (pos+26) ntags names_of_tag in
      let file = {
          f_md4 = md4;
          f_ip = ip;
          f_port = port;
          f_tags = tags;
        } in
      file
    
    
    let parse len s =
      get_file s 1
    
    let print t = 
      Printf.printf "FOUND:\n";
      Printf.printf "  MD4: %s\n" (Md4.to_string t.f_md4);
      Printf.printf "  ip: %s\n" (Ip.to_string t.f_ip);
      Printf.printf "  port: %d\n" t.f_port;
      Printf.printf "  tags: ";
      print_tags t.f_tags;
      print_newline ()

    let fprint oc t = 
      Printf.fprintf oc "FOUND:\n";
      Printf.fprintf oc "%s\n" (Md4.to_string t.f_md4);
      Printf.fprintf oc "%s\n" (Ip.to_string t.f_ip);
      Printf.fprintf oc "%d\n" t.f_port;
      Printf.fprintf oc "TAGS:\n";
      fprint_tags oc t.f_tags;
         Printf.fprintf oc "\n"
    
    let write buf file =
      buf_md4 buf file.f_md4;
      buf_ip buf file.f_ip;
      buf_port buf file.f_port;
      buf_int buf (List.length file.f_tags);
      buf_tags buf file.f_tags names_of_tag

  end

let unit = ()
module NoArg = functor(M: sig val m : string end) -> (struct 
        type t = unit
        
        let parse len s = ()
        
        let print t = 
          Printf.printf "%s:\n" M.m
        
        let write (buf: Buffer.t) (t: t) = unit
          
        let t = (() : t)
      end : sig
        type t
          val parse : int -> string  -> t
          val print : t -> unit
          val write : Buffer.t -> t  -> unit
          val t :t 
          end
      )

module QueryNext = NoArg(struct let m = "QUERY NEXT" end)

  
  
module Query  = struct (* request 22 *)
    
    let names_of_tag =
      [
        2, "size";
        3, "type";
        4, "format";
        21, "availability";
      ]
        
    let rec parse_query s pos =
      let t = get_int8 s pos in
      match t with
        0 -> 
          let t = get_int8 s (pos+1) in
          begin
            match t with
              0 -> 
                let q1, pos  = parse_query s (pos + 2) in
                let q2, pos  = parse_query s pos in
                QAnd (q1,q2), pos
            | 1 -> 
                let q1, pos  = parse_query s (pos + 2) in
                let q2, pos  = parse_query s pos in
                QOr (q1,q2), pos
            | 2 -> 
                let q1, pos  = parse_query s (pos + 2) in
                let q2, pos  = parse_query s pos in
                QAndNot (q1,q2), pos
            |_ -> failwith "Unknown QUERY operator"
          end
      | 1 -> let s, pos = get_string s (pos + 1) in QHasWord s, pos
      | 2 -> 
          let field, pos = get_string s (pos + 1) in
          let name, pos = get_string s pos in          
          let name = 
            if String.length name = 1 then
              try
                List.assoc (get_int8 name 0) names_of_tag
              with _ -> name 
            else name in
          
          QHasField (name, field), pos
      
      | 3 -> 
          let field = get_int32_32 s (pos + 1) in
          let minmax = get_int8 s (pos + 5) in
          let name, pos = get_string s (pos + 6) in
          let name = 
            if String.length name = 1 then
              try
                List.assoc (get_int8 name 0) names_of_tag
              with _ -> name 
            else name in
          begin
            match minmax with
              1 -> QHasMinVal (name, field)
            | 2 -> QHasMaxVal (name, field)
            | _ -> failwith "Unknown QUERY minmax"
          end, pos
      | 4 -> QHasWord "", pos + 1
      | _ -> failwith "Unknown QUERY format"
    
    let parse len s = 
      let t, pos = parse_query s 1 in t

(*
  type = "Col" pour voir les collections
    Fields:
  "Album" | "Artist" | "Title"
    
    *)
    
    let rec print_query t =
      match t with
        QOr (q1, q2) -> 
          print_query q1; 
          print_string " OR ";
          print_query q2
      | QAnd (q1, q2) -> 
          print_query q1; 
          print_string " AND ";
          print_query q2
      | QAndNot (q1, q2) -> 
          print_query q1; 
          print_string " NOT ";
          print_query q2
      
      | QHasWord s ->
          Printf.printf "Contains[%s]" s
      | QHasField (name, field) ->
          Printf.printf "Field[%s] = [%s]" name field
      | QHasMinVal (name, field) ->
          Printf.printf "Field[%s] > [%s]" name (Int32.to_string field)
      | QHasMaxVal (name, field) ->
          Printf.printf "Field[%s] < [%s]" name (Int32.to_string field)
      |	QNone ->
	  prerr_endline "print_query: QNone in query";
	  ()
    
    let  print t = 
      Printf.printf "QUERY";
      print_query t
    

     let rec fprint_query oc t =
      match t with
        QOr (q1, q2) -> 
          print_query q1; 
          Printf.fprintf oc " OR ";
          print_query q2
      | QAnd (q1, q2) -> 
          print_query q1; 
          Printf.fprintf oc " AND ";
          print_query q2
      | QAndNot (q1, q2) -> 
          print_query q1; 
          Printf.fprintf oc " NOT ";
          print_query q2
      
      | QHasWord s ->
          Printf.fprintf oc "Contains[%s]" s
      | QHasField (name, field) ->
          Printf.fprintf oc "Field[%s] = [%s]" name field
      | QHasMinVal (name, field) ->
          Printf.fprintf oc "Field[%s] > [%s]" name (Int32.to_string field)
      | QHasMaxVal (name, field) ->
          Printf.fprintf oc "Field[%s] < [%s]" name (Int32.to_string field)
      |	QNone ->
	  prerr_endline "print_query: QNone in query\n";
	  ()


    let fprint oc t = 
      Printf.fprintf oc "QUERY:\n";
      fprint_query oc t;
      Printf.fprintf oc "\n"

    let rec write buf t = 
      match t with
        QOr (q1, q2) -> 
          buf_int8 buf 0;
          buf_int8 buf 1;
          write buf  q1; 
          write buf  q2; 
      | QAnd (q1, q2) -> 
          buf_int8 buf 0;
          buf_int8 buf 0;
          write buf  q1; 
          write buf  q2; 
      | QAndNot (q1, q2) -> 
          buf_int8 buf 0;
          buf_int8 buf 2;
          write buf  q1; 
          write buf  q2; 
      | QHasWord s ->
          buf_int8 buf 1;
          buf_string buf s
      | QHasField (name, field) ->
          
          let name = try
              let i = rev_assoc name names_of_tag in
              String.make 1 (char_of_int i)            
            with _ -> name in
          
          buf_int8 buf 2;
          buf_string buf field;
          buf_string buf name
          
      | QHasMinVal (name, field) ->
          
          let name = try
              let i = rev_assoc name names_of_tag in
              String.make 1 (char_of_int i)            
            with _ -> name in
          
          buf_int8 buf 3;
          buf_int32_32 buf field;
          buf_int8 buf 1;
          buf_string buf name

      | QHasMaxVal (name, field) ->
                    
          let name = try
              let i = rev_assoc name names_of_tag in
              String.make 1 (char_of_int i)            
            with _ -> name in
          
          buf_int8 buf 3;
          buf_int32_32 buf field;
          buf_int8 buf 2;
          buf_string buf name

      |	QNone ->
	  prerr_endline "print_query: QNone in query";
	  ()
            
  end

  
module QueryUsers = struct (* request 26 *)
    (* TODO: Utilise le meme format que Query *)
    
    type t = string
      
    let parse len s =
      let targ = get_int8 s 1 in
      match targ with
        4 -> ""
      | 1 -> 
          let name, pos = get_string s 2 in
          name
      | _ -> 
          Printf.printf "QueryUsers: unknown tag %d" targ;
          print_newline ();
          raise Not_found
          
    let print t =
      Printf.printf "QUERY USERS [%s]" t;
      print_newline () 

     let fprint oc t =
      Printf.fprintf oc "QUERY USERS [%s]\n" t
      
    let write buf t =
      if t = "" then
        buf_int8 buf 4
      else begin
          buf_int8 buf 1;
          buf_string buf t
        end
  end
  
module QueryUsersReply = struct (* request 67 *)
    type client = {
        md4 : Md4.t;
        ip: Ip.t;
        port: int;
        tags : tag list;
      }
    
    type t = client list
    
    let names_of_tag =
      [
        1, "name";
        17, "version";
        15, "port";
      ]
    
    let rec parse_clients s pos nclients left =
      if nclients = 0 then List.rev left else
      let md4 = get_md4 s pos in
       let ip = get_ip s (pos+16) in
      let port = get_port s (pos+20) in
      let ntags = get_int s (pos+22) in
      let tags, pos = get_tags s (pos+26) ntags names_of_tag in
      parse_clients s pos (nclients-1) (
        {
          md4 = md4;
          ip = ip;
          port = port;
          tags = tags;
        } :: left)
    
    let parse len s =
      let nclients = get_int s 1 in
      parse_clients s 5 nclients []
    
    let print t = 
      Printf.printf "QUERY USERS REPLY:\n";
      List.iter (fun t ->
          Printf.printf "MD4: %s\n" (Md4.to_string t.md4); 
          Printf.printf "ip: %s\n" (Ip.to_string t.ip);
          Printf.printf "port: %d\n" t.port;
          Printf.printf "tags: ";
          print_tags t.tags; print_newline ();) t

    let fprint oc t = 
      Printf.fprintf oc "QUERY USERS REPLY:\n";
      List.iter (fun t ->
          Printf.fprintf oc "%s\n" (Md4.to_string t.md4); 
          Printf.fprintf oc "%s\n" (Ip.to_string t.ip);
          Printf.fprintf oc "%d\n" t.port;
          Printf.fprintf oc "TAGS:\n";
          fprint_tags oc t.tags;
	  Printf.fprintf oc "\n"
          ) t
    
    let write buf t =
      buf_int buf (List.length t);
      List.iter (fun t ->
          buf_md4 buf t.md4;
          buf_ip buf t.ip;
          buf_port buf t.port;
          let ntags = List.length t.tags in
          buf_int buf ntags;
          buf_tags buf t.tags names_of_tag) t
  end
  
module QueryLocation  = struct 
    type t = Md4.t
      
    let parse len s = 
      get_md4 s 1
      
    let print t = 
      Printf.printf "QUERY LOCATION OF %s" (Md4.to_string t)

    let fprint oc t = 
      Printf.fprintf oc "QUERY LOCATION OF %s\n" (Md4.to_string t)
      
    let write buf t = 
      buf_md4 buf t
  end

module QueryLocationReply  = struct 
    type location = {
        ip : Ip.t;
        port : int;
      }
    
    type t = {
        md4: Md4.t;
        locs :location list; 
      }
      
    let parse len s = 
      let md4 = get_md4 s 1 in
      let n = get_int8 s 17 in
      let rec iter i  =
        if i = n then [] else
        let ip = get_ip s (18 + i * 6) in
        let port = get_port s (22+ i * 6) in
        { ip = ip; port = port; } :: (iter (i+1))
      in
      let locs = iter 0 in
      { locs =locs; md4 = md4 }
          
    let print t = 
      Printf.printf "LOCATION OF %s" (Md4.to_string t.md4);
      print_newline ();
      List.iter (fun l -> 
          Printf.printf "   %s : %d" (Ip.to_string l.ip) l.port;
          print_newline ();
      ) t.locs

     let fprint oc t = 
      Printf.fprintf oc "LOCATION OF %s\n" (Md4.to_string t.md4);
      List.iter (fun l -> 
          Printf.fprintf oc "%s:%d\n" (Ip.to_string l.ip) l.port;
      ) t.locs
      
    let write buf t = 
      buf_md4 buf t.md4;
      buf_int8 buf (List.length t.locs);
      List.iter (fun l ->
          buf_ip buf l.ip;
          buf_port buf l.port
      ) t.locs
      
  end
  
module QueryID  = struct 
    type t = Ip.t
      
    let parse len s = 
      get_ip s 1
      
    let print t = 
      Printf.printf "QUERY IP OF %s" (Ip.to_string t)

    let fprint oc t = 
      Printf.fprintf oc "QUERY IP OF %s\n" (Ip.to_string t)
      
    let write buf t = 
      buf_ip buf t
  end
  
module QueryIDFailed  = struct 
    type t = Ip.t
      
    let parse len s = 
      get_ip s 1
      
    let print t = 
      Printf.printf "QUERY IP OF %s FAILED" (Ip.to_string t)

    let fprint oc t = 
      Printf.fprintf oc "QUERY IP OF %s FAILED\n" (Ip.to_string t)
      
    let write buf t = 
      buf_ip buf t
  end

module QueryIDReply  = struct 
    type t = {
        ip : Ip.t;
        port : int;
      }
          
    let parse len s = 
      let ip = get_ip s 1 in
      let port = get_port s 5 in
      { ip = ip; port = port; }
      
    let print t = 
      Printf.printf "IDENTIFICATION %s : %d" (Ip.to_string t.ip) t.port;
      print_newline ()

    let fprint oc t = 
      Printf.fprintf oc "IDENTIFICATION %s : %d\n" (Ip.to_string t.ip) t.port
    
      
    let write buf t = 
      buf_ip buf t.ip;
      buf_port buf t.port
      
  end

module QueryCallUdp  = struct 
    type t = {
        ip : Ip.t;
        port : int;
        id : Ip.t;
      }
          
    let parse len s = 
      let ip = get_ip s 1 in
      let port = get_port s 5 in
      let id = get_ip s 7 in
      { ip = ip; port = port; id = id; }
      
    let print t = 
      Printf.printf "QueryCall %s : %d --> %s" (Ip.to_string t.ip) t.port
        (Ip.to_string t.id);
      print_newline ()

    let fprint oc t = 
      Printf.fprintf oc "QueryCall %s : %d --> %s\n" (Ip.to_string t.ip) t.port
        (Ip.to_string t.id)
      
    let write buf t = 
      buf_ip buf t.ip;
      buf_port buf t.port;
      buf_ip buf t.id
      
  end

module QueryServers  = struct 
    type t = {
        ip : Ip.t;
        port : int;
      }
      
    let parse len s = 
      let ip = get_ip s 1 in
      let port = get_port s 5 in
      { ip = ip; port = port; }
      
    let print t = 
      Printf.printf "QUERY SERVERS %s : %d" (Ip.to_string t.ip) t.port;
      print_newline ()

    let fprint oc t = 
      Printf.fprintf oc "QUERY SERVERS %s : %d\n" (Ip.to_string t.ip) t.port
    
      
    let write buf t = 
      buf_ip buf t.ip;
      buf_port buf t.port
      
  end

module QueryServersReply  = struct 
    type server = {
        ip : Ip.t;
        port : int;
      }

    type t = {
        server_ip : Ip.t;
        server_port : int;
        servers: server list;
      }
      
    let rec parse_servers nservers s pos = 
      if nservers = 0 then [] else
      let ip = get_ip s pos in
      let port = get_port s (pos+4) in
      { ip = ip; port = port; } ::
      (parse_servers (nservers-1) s (pos+6))
      
    let parse len s =
      try
	let ip = get_ip s 1 in
	let port = get_port s 5 in
	let nservers = get_int8 s 7 in
	let servers = parse_servers nservers s 8 in
	  { server_ip = ip; server_port = port; servers = servers }
      with _ ->
	let nservers = get_int8 s 1 in
	let servers = parse_servers nservers s 2 in
	  { server_ip = Ip.null; server_port = 0; servers = servers }  
      
    let print t = 
      Printf.printf "SERVERS QUERY REPLY %s : %d" (
        Ip.to_string t.server_ip) t.server_port;
      print_newline ();
      List.iter (fun s -> 
          Printf.printf "   %s:%d" (Ip.to_string s.ip) s.port; 
          print_newline ();
      ) t.servers

     let fprint oc t = 
      Printf.fprintf oc "SERVERS QUERY REPLY:\n"; 
      Printf.fprintf oc  "%s:%d\n" (
        Ip.to_string t.server_ip) t.server_port;
      List.iter (fun s -> 
          Printf.fprintf oc  "%s:%d\n" (Ip.to_string s.ip) s.port; 
      ) t.servers
      
    let write buf t = 
      if (t.server_port = 0) then
	begin
	  buf_int8 buf (List.length t.servers);
	  List.iter (fun s -> 
		       buf_ip buf s.ip; buf_int16 buf s.port) t.servers 
	end
      else
	begin
	  buf_ip buf t.server_ip;
	  buf_port buf t.server_port;
	  buf_int8 buf (List.length t.servers);
	  List.iter (fun s -> 
		       buf_ip buf s.ip; buf_int16 buf s.port) t.servers
	end
      
  end

module PingServerUdp = struct (* client -> serveur pour identification ? *)
    type t = int32
      
      
    let parse len s =
      get_int32_32 s 1(*, get_int8 s 2, get_int8 s 3*)
      
    (*let print (t1,t2,t3) = 
      Printf.printf "MESSAGE 150 UDP %d %d %d" t1 t2 t3;
      print_newline ()*)

    let print t =
      Printf.printf "PING %s\n " (Int32.to_string t)
            
    let fprint oc t =
      Printf.fprintf oc "PING %s\n" (Int32.to_string t)
      
    let write buf t =
      buf_int32_32 buf t
                 
                   
    (* let fprint oc (t1,t2,t3) = 
      Printf.fprintf oc "MESSAGE 150 UDP %d %d %d\n" t1 t2 t3*)
      
    (*let write buf (t1,t2,t3) = 
      buf_int8 buf t1;
      buf_int8 buf t2;
      buf_int8 buf t3;*)
      
  end

module PingServerReplyUdp = struct (* reponse du serveur a 150 *)
    type t = int32 *  int32 * int32
      
    (*let parse len s =
      get_int8 s 1, get_int8 s 2, get_int8 s 3, get_int8 s 4,
      get_int32_32 s 5, get_int32_32 s 9
      
    let print (t1,t2,t3,t4,t5,t6) = 
      Printf.printf "MESSAGE 150 UDP %d %d %d %d %s %s" t1 t2 t3 t4
        (Int32.to_string t5) (Int32.to_string t6);
      print_newline ()

    let fprint oc (t1,t2,t3,t4,t5,t6) = 
      Printf.fprintf oc "MESSAGE 150 UDP %d %d %d %d %s %s" t1 t2 t3 t4
        (Int32.to_string t5) (Int32.to_string t6)
      
    let write buf (t1,t2,t3,t4,t5,t6) = 
      buf_int8 buf t1;
      buf_int8 buf t2;
      buf_int8 buf t3;
      buf_int8 buf t4;
      buf_int32_32 buf t5;
      buf_int32_32 buf t6;*)
      
    let parse len s =
        get_int32_32 s 1, get_int32_32 s 5, get_int32_32 s 9

     let print (t1,t2,t3) =
         Printf.printf "PING REPLY no:%s clients:%s files:%s\n" (Int32.to_string t1)
                           (Int32.to_string t2) (Int32.to_string t3)
      
     let fprint oc (t1,t2,t3) =
         Printf.fprintf oc "PING REPLY no:%s clients:%s files:%s\n" (Int32.to_string t1)
                           (Int32.to_string t2) (Int32.to_string t3)

      let write buf (t1,t2,t3) =
          buf_int32_32 buf t1;
          buf_int32_32 buf t2;
          buf_int32_32 buf t3;
                           
  end
  
module ServerDescUdp = struct
  type t = {
    ip : Ip.t;
  }

  let parse len s =
    let ip = get_ip s 1 in
     {
	ip = ip
      }
      
  let print t =
    Printf.printf "ServerDescUdpReq %s\n" (Ip.to_string t.ip)

  let write buf t =
    buf_ip buf t.ip

end

module ServerDescReplyUdp = struct
  type t = {
    name : string;
    desc : string;
  }

  let parse len s =
    let name, pos = get_string s 1 in
    let desc, pos = get_string s pos in
     {
       name = name;
       desc = desc;
     }
      
  let print t =
    Printf.printf "ServerDescReplyUdpReq\n";
    Printf.printf "name : %s\n" t.name;
    Printf.printf "desc : %s\n" t.desc

  let write buf t =
    buf_string buf t.name;
    buf_string buf t.desc

end


    
module ServerListUdp = struct
  type t = {
    ip : Ip.t;
  }

  let parse len s =
    let ip = get_ip s 1 in
      {
	ip = ip;
      }
      
  let print t =
    Printf.printf "ServerListUdp %s\n" (Ip.to_string t.ip)

  let write buf t =
    buf_ip buf t.ip

end


module Req  = struct 
    type t 
      
    let parse len s = raise Not_found
    let print t = raise Not_found
    let write buf s = raise Not_found
  end

type t = 
| ConnectReq of Connect.t
| SetIDReq of SetID.t
| AckIDReq of AckID.t
| MessageReq of Message.t
| ShareReq of Share.t
| InfoReq of Info.t
| ServerListReq of ServerList.t
| ServerInfoReq of ServerInfo.t
| QueryReplyReq of QueryReply.t
| QueryReq of CommonTypes.query
| QueryLocationReq of QueryLocation.t
| QueryLocationReplyReq of QueryLocationReply.t
| QueryIDReq of QueryID.t
| QueryIDFailedReq of QueryIDFailed.t
| QueryIDReplyReq of QueryIDReply.t
| UnknownReq of string
| ChatRoomsReq of ChatRooms.t
| QueryUsersReq of QueryUsers.t
| QueryUsersReplyReq of QueryUsersReply.t
| QueryServersUdpReq of QueryServers.t  
| QueryServersReplyUdpReq of QueryServersReply.t  

| PingServerUdpReq of PingServerUdp.t
| PingServerReplyUdpReq of PingServerReplyUdp.t
  
| DBServerListReq of DBServerList.t
| QueryLocationUdpReq of QueryLocation.t  
| QueryLocationReplyUdpReq of QueryLocationReply.t  
| QueryReplyUdpReq of QueryUdpReply.t
| QueryUdpReq of CommonTypes.query
| QueryCallUdpReq of QueryCallUdp.t
| FileGroupInfoUdpReq of QueryLocationReply.t    
| ServerDescUdpReq of ServerDescUdp.t
| ServerDescReplyUdpReq of ServerDescReplyUdp.t   
| ServerListUdpReq of ServerListUdp.t    
| QueryMoreResultsReq
  
(****************
                     MLdonkey extensions messages 
***************)
  
(* server to client: client has been recognized as
a mldonkey client by a mldonkey server *)
| Mldonkey_MldonkeyUserReplyReq
(* client to server: the client want to subscribe to this query *)
| Mldonkey_SubscribeReq of int * CommonTypes.query
(* server to client: the server send a notification to a subscription *)
| Mldonkey_NotificationReq of int * QueryReply.t
(* client to server: the client want to cancel a subscription *)
| Mldonkey_CloseSubscribeReq of int
  
let mldonkey_extensions len s =
  let opcode = int_of_char s.[1] in
  match opcode with  
  | 1 -> 
      Mldonkey_MldonkeyUserReplyReq
  | 2 -> 
      let num = get_int s 2 in
      let query, pos = Query.parse_query s 6 in
      Mldonkey_SubscribeReq (num, query)
  | 3 ->
      let num = get_int s 2 in
      let files = QueryReply.get_replies s 6 in
      Mldonkey_NotificationReq (num, files)
      
  | 4 ->
      let num = get_int s 2 in
      Mldonkey_CloseSubscribeReq num
  | _ -> raise Not_found
  
let parse s =
  try 
    let len = String.length s in
    if len = 0 then raise Not_found;
    let opcode = int_of_char (s.[0]) in
(*    Printf.printf "opcode: %d" opcode; print_newline (); *)
    match opcode with 
    | 1 -> ConnectReq (Connect.parse len s)
(*    | 5 -> BadProtocolVersion *)
    | 20 -> AckIDReq (AckID.parse len s)
    | 21 -> ShareReq (Share.parse len s)
    | 22 -> QueryReq (Query.parse len s)
    | 25 -> QueryLocationReq (QueryLocation.parse len s)
    | 26 -> QueryUsersReq (QueryUsers.parse len s)
    | 28 -> QueryIDReq (QueryID.parse len s)
(*    | 29 -> QueryChats (C->S) *)
(*    | 30 -> ChatMessage (C->S) *)
(*    | 31 -> JoinRoom (C->S) *)
    | 33 -> QueryMoreResultsReq
    | 50 -> ServerListReq (ServerList.parse len s)
    | 51 -> QueryReplyReq (QueryReply.parse len s)
    | 52 -> InfoReq (Info.parse len s)
    | 53 -> QueryIDReplyReq (QueryIDReply.parse len s)
    | 54 -> QueryIDFailedReq (QueryIDFailed.parse len s)
    | 56 -> MessageReq (Message.parse len s)
    | 57 -> ChatRoomsReq (ChatRooms.parse len s)
(*    | 58 -> ChatBroadcastMessage (S->C) *)
(*    | 59 -> ChatUserJoin (S->C) *)
(*    | 60 -> ChatUserLeave (S->C) *)
(*    | 61 -> ChatUsers (S->C) *)
    | 64 -> SetIDReq (SetID.parse len s)
    | 65 -> ServerInfoReq (ServerInfo.parse len s)
    | 66 -> QueryLocationReplyReq (QueryLocationReply.parse len s)
    | 67 -> QueryUsersReplyReq (QueryUsersReply.parse len s)
(* UDP *)
    
    | 150 -> PingServerUdpReq (PingServerUdp.parse len s)
    | 151 -> PingServerReplyUdpReq (PingServerReplyUdp.parse len s)
    
    | 152 -> QueryUdpReq (Query.parse len s)
    | 153 -> QueryReplyUdpReq (QueryUdpReply.parse len s)
    | 154 -> QueryLocationUdpReq (QueryLocation.parse len s)
    | 155 -> QueryLocationReplyUdpReq (QueryLocationReply.parse len s)
    | 156 -> QueryCallUdpReq (QueryCallUdp.parse len s)
    | 160 -> QueryServersUdpReq (QueryServers.parse len s)
    | 161 -> QueryServersReplyUdpReq (QueryServersReply.parse len s)
    | 162 -> ServerDescUdpReq (ServerDescUdp.parse len s)
    | 163 -> ServerDescReplyUdpReq (ServerDescReplyUdp.parse len s)
    | 164 -> ServerListUdpReq (ServerListUdp.parse len s)

(* MLDONKEY *)
    | 250 -> mldonkey_extensions len s
    | _ ->
	Printf.printf "COOOOOOL\n";
	raise Not_found
  with
    e -> 
      Printf.printf "From server:"; print_newline ();
      dump s;
      UnknownReq s
      
let print t =
  begin
    match t with
      ConnectReq t -> Connect.print t
    | SetIDReq t -> SetID.print t
    | AckIDReq t -> AckID.print t
    | MessageReq t -> Message.print t
    | ShareReq t -> Share.print t
    | InfoReq t -> Info.print t
    | ServerListReq t -> ServerList.print t
    | DBServerListReq t -> DBServerList.print t
    | ServerInfoReq t -> ServerInfo.print t
    | QueryReq t
    | QueryUdpReq t -> Query.print t
    | QueryReplyReq t -> QueryReply.print t
    | QueryReplyUdpReq t -> QueryUdpReply.print t
    | QueryLocationReq t
    | QueryLocationUdpReq t
      -> QueryLocation.print t
    | QueryLocationReplyReq t
    | QueryLocationReplyUdpReq t
    | FileGroupInfoUdpReq t
      -> 
        QueryLocationReply.print t
    | QueryIDReq t -> QueryID.print t
    | QueryCallUdpReq t -> QueryCallUdp.print t
    | QueryIDFailedReq t -> QueryIDFailed.print t
    | QueryIDReplyReq t -> QueryIDReply.print t
    | QueryUsersReq t -> QueryUsers.print t
    | QueryUsersReplyReq t -> QueryUsersReply.print t
    | ChatRoomsReq t -> ChatRooms.print t
    | QueryServersUdpReq t -> QueryServers.print t
    | QueryServersReplyUdpReq t -> QueryServersReply.print t

    | PingServerUdpReq t -> PingServerUdp.print t
    | PingServerReplyUdpReq t -> PingServerReplyUdp.print t
    | ServerDescUdpReq t -> ServerDescUdp.print t
    | ServerDescReplyUdpReq t -> ServerDescReplyUdp.print t
    | ServerListUdpReq t -> ServerListUdp.print t

    | QueryMoreResultsReq -> 
        Printf.printf "QUERY MORE RESULTS"; print_newline ()
    | Mldonkey_MldonkeyUserReplyReq ->
        Printf.printf "MLDONKEY USER"; print_newline ()
    | Mldonkey_SubscribeReq (num,t) -> 
        Printf.printf "MLDONKEY SUBSCRIPTION %d" num; print_newline ();
        Query.print t
    | Mldonkey_NotificationReq (num,t) ->
        Printf.printf "MLDONKEY NOTIFICATIONS TO %d" num; print_newline ();
        QueryReply.print t
    | Mldonkey_CloseSubscribeReq num ->
        Printf.printf "MLDONKEY CLOSE SUBSCRIPTION %d" num; print_newline ();
    | UnknownReq s -> 
        let len = String.length s in
        Printf.printf "UnknownReq:\n";
        Printf.printf "ascii: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          if n > 31 && n < 127 then
            Printf.printf " %c" c
          else
            Printf.printf "(%d)" n
        done;
        Printf.printf "]\n";
        Printf.printf "dec: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          Printf.printf "(%d)" n            
        done;
        Printf.printf "]\n";
        print_newline ()
  end;
  print_newline ()

let fprint oc t =
  begin
    match t with
      ConnectReq t -> Connect.fprint oc t
    | SetIDReq t -> SetID.fprint oc t
    | AckIDReq t -> AckID.fprint oc t
    | MessageReq t -> Message.fprint oc t
    | ShareReq t -> Share.fprint oc t
    | InfoReq t -> Info.fprint oc t
    | ServerListReq t -> ServerList.fprint oc t
    | DBServerListReq t -> DBServerList.fprint oc t
    | ServerInfoReq t -> ServerInfo.fprint oc t
    | QueryReq t -> Query.fprint oc t
    | QueryUdpReq t -> Query.fprint oc t
    | QueryReplyReq t -> QueryReply.fprint oc t
    | QueryReplyUdpReq t -> QueryUdpReply.fprint oc t
    | QueryLocationReq t -> QueryLocation.fprint oc t
    | QueryLocationUdpReq t
      -> QueryLocation.fprint oc t
    | QueryLocationReplyReq t -> QueryLocationReply.fprint oc t
    | QueryLocationReplyUdpReq t ->  QueryLocationReply.fprint oc t
    | FileGroupInfoUdpReq t
      -> 
        QueryLocationReply.fprint oc t
    | QueryIDReq t -> QueryID.fprint oc t
    | QueryCallUdpReq t -> QueryCallUdp.fprint oc t
    | QueryIDFailedReq t -> QueryIDFailed.fprint oc t
    | QueryIDReplyReq t -> QueryIDReply.fprint oc t
    | QueryUsersReq t -> QueryUsers.fprint oc t
    | QueryUsersReplyReq t -> QueryUsersReply.fprint oc t
    | ChatRoomsReq t -> ChatRooms.fprint oc t
    | QueryServersUdpReq t -> QueryServers.fprint oc t
    | QueryServersReplyUdpReq t -> QueryServersReply.fprint oc t

    | PingServerUdpReq t -> PingServerUdp.fprint oc  t
    | PingServerReplyUdpReq t -> PingServerReplyUdp.fprint oc  t

    | ServerDescUdpReq t -> ()
    | ServerDescReplyUdpReq t -> ()
    | ServerListUdpReq t -> ()

    | QueryMoreResultsReq -> 
        Printf.fprintf oc "QUERY MORE RESULTS\n"
    | Mldonkey_MldonkeyUserReplyReq ->
        Printf.fprintf oc "MLDONKEY USER\n"
    | Mldonkey_SubscribeReq (num,t) -> 
        Printf.fprintf oc "MLDONKEY SUBSCRIBE %d\n" num;
        Query.fprint oc t
    | Mldonkey_NotificationReq (num,t) ->
        Printf.fprintf oc "MLDONKEY NOTIFICATIONS TO %d\n" num; 
        QueryReply.fprint oc t
    | Mldonkey_CloseSubscribeReq num ->
        Printf.printf "MLDONKEY CLOSE SUBSCRIPTION %d\n" num;
        
    | UnknownReq s -> 
(* let len = String.length s in*)
        Printf.fprintf oc "UnknownReq\n"
(* Printf.printf "ascii: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          if n > 31 && n < 127 then
            Printf.printf " %c" c
          else
            Printf.printf "(%d)" n
        done;
        Printf.printf "]\n";
        Printf.printf "dec: [";
        for i = 0 to len - 1 do
          let c = s.[i] in
          let n = int_of_char c in
          Printf.printf "(%d)" n            
        done;
        Printf.printf "]\n";
        print_newline ()*)
  end
  
(* Why is this called udp_write ??? It is the normal function to encode messages
  both on UDP and TCP connections !!! *)
  
let udp_write buf t =
  match t with
  | ConnectReq t -> 
      buf_int8 buf 1;
      Connect.write buf t
  | SetIDReq t -> 
      buf_int8 buf 64;
      SetID.write buf  t
  | AckIDReq t -> 
      buf_int8 buf 20;
      AckID.write buf  t
  | MessageReq t -> 
      buf_int8 buf 56;
      Message.write buf  t
  | ShareReq t -> 
      buf_int8 buf 21;
      Share.write buf  t
  | InfoReq t -> 
      buf_int8 buf 52;
      Info.write buf  t
  | ServerListReq t -> 
      buf_int8 buf 50;
      ServerList.write buf  t
  | DBServerListReq t -> 
      buf_int8 buf 250;
      DBServerList.write buf  t
  | ServerInfoReq t -> 
      buf_int8 buf 65;
      ServerInfo.write buf  t
  | QueryReplyReq t ->
      buf_int8 buf 51;
      QueryReply.write buf t
  | QueryReq t -> 
      buf_int8 buf 22;
      Query.write buf t
  | QueryLocationReq t ->
      buf_int8 buf 25;
      QueryLocation.write buf t
  | QueryLocationReplyReq t -> 
      buf_int8 buf 66;
      QueryLocationReply.write buf t
  | QueryIDReq t -> 
      buf_int8 buf 28;
      QueryID.write buf t
  | QueryIDReplyReq t -> 
      buf_int8 buf 53;
      QueryIDReply.write buf t
  | QueryIDFailedReq t -> 
      buf_int8 buf 54;
      QueryIDFailed.write buf t
  | ChatRoomsReq t ->
      buf_int8 buf 57;
      ChatRooms.write buf t
  | UnknownReq s ->
      Buffer.add_string buf s
  | QueryUsersReq t -> 
      buf_int8 buf 26;
      QueryUsers.write buf t
  | QueryUsersReplyReq t -> 
      buf_int8 buf 67;
      QueryUsersReply.write buf t
  | QueryServersUdpReq t -> 
      buf_int8 buf 160;
      QueryServers.write buf t
  | QueryServersReplyUdpReq t -> 
      buf_int8 buf 161;
      QueryServersReply.write buf t

  | ServerDescUdpReq t ->
      buf_int8 buf 162;
      ServerDescUdp.write buf t
  | ServerDescReplyUdpReq t ->
      buf_int8 buf 163;
      ServerDescReplyUdp.write buf t
  | ServerListUdpReq t ->
      buf_int8 buf 164;
      ServerListUdp.write buf t

  | PingServerUdpReq t -> 
      buf_int8 buf 150;
      PingServerUdp.write buf t
  | PingServerReplyUdpReq t -> 
      buf_int8 buf 151;
      PingServerReplyUdp.write buf t
  
  | QueryLocationUdpReq t ->
      buf_int8 buf 154;
      QueryLocation.write buf t
  | QueryLocationReplyUdpReq t ->
      buf_int8 buf 155;
      QueryLocationReply.write buf t
  | QueryUdpReq t -> 
      buf_int8 buf 152;
      Query.write buf t
  | QueryReplyUdpReq t ->
      buf_int8 buf 153;
      QueryUdpReply.write buf t
  | QueryCallUdpReq t -> 
      buf_int8 buf 156;
      QueryCallUdp.write buf t
  | FileGroupInfoUdpReq t ->
      buf_int8 buf 251;
      QueryLocationReply.write buf t
  | QueryMoreResultsReq ->
      buf_int8 buf 33
      
(**************
mldonkey extensions to the protocol
**************)
  
  | Mldonkey_MldonkeyUserReplyReq ->
      buf_int8 buf 250; (* MLdonkey extensions opcode *)
      buf_int8 buf 1
      
  | Mldonkey_SubscribeReq (num,t) ->
      buf_int8 buf 250; (* MLdonkey extensions opcode *)
      buf_int8 buf 2;
      buf_int buf num;
      Query.write buf t

  | Mldonkey_NotificationReq (num,t) ->
      buf_int8 buf 250; (* MLdonkey extensions opcode *)
      buf_int8 buf 3;
      buf_int buf num;
      QueryReply.write_replies buf t

  | Mldonkey_CloseSubscribeReq num ->
      buf_int8 buf 250; (* MLdonkey extensions opcode *)
      buf_int8 buf 4;
      buf_int buf num

            
let write buf t =
  udp_write buf t
