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
open AnyEndian
open Printf2
open Md4
open LittleEndian
  
open CommonTypes
open CommonGlobals
  
open DonkeyTypes
open DonkeyMftp

let dump_file filename =
  let ic = open_in filename in
  let s = String.create 20 in
  try
    lprintf "file: %s" filename; lprint_newline ();
    let pos = ref 0 in
    while true do 
      let n = input ic s 0 20 in
      lprintf "pos = %d" !pos; lprint_newline ();
      if n = 0 then raise Exit;
      dump (String.sub s 0 n);
      pos := !pos + n;
      lprint_newline ();
    done
  with _ ->
      close_in ic
      
module Server = struct
    
    type server = {
        ip : Ip.t;
        port : int;
        tags : tag list;
      }
    
    type t = server list
    
    
    let names_of_tag =
      [
        "\001", "name";
        "\015", "port";
        "\016", "ip";
        "\012", "ping";
        "\014", "prof";
        "\013", "history";
        "\011", "description";
      ]
    
    
    let rec read_servers s pos left =
      if pos + 9 >= String.length s then List.rev left else
      match
        try
          let ip = get_ip s pos in
          let port = get_port s (pos+4) in
          let tags, pos = get_tags s (pos+6) names_of_tag in
          Some ({
            ip = ip;
            port = port;
            tags = tags;
            }, pos)
        with e -> 
            (*
            let len = String.length s - pos in
            lprintf "Error while reading servers %s (left %d)"
              (Printexc2.to_string e) len; lprint_newline ();
dump (String.sub s pos len);      
  *)
            None
      with
        None -> List.rev left
      | Some (server, pos) ->
          read_servers s pos (server :: left)
      
    let read s =
      read_servers s 5  []
      
    let write buf t =
      buf_int8 buf 14;
      buf_int buf (List.length t);
      List.iter (fun s ->
          buf_ip buf s.ip;
          buf_port buf s.port;
          buf_tags buf s.tags names_of_tag
      ) t

    let print t =
      lprintf "SERVER.MET: %d servers" (List.length t); lprint_newline ();
      List.iter (fun s ->
          lprintf "  SERVER %s:%d" (Ip.to_string s.ip) s.port;
          lprint_newline ();
          print_tags s.tags;
          lprint_newline ();
      ) t;
      
  end
  
module Known = struct
    
    type file = {
        mtime : int64; 
        md4 : Md4.t;
        blocks : Md4.t array;
        tags : tag list;
      }
    
    type t = file list      
    
    let names_of_tag =
      [
        "\001", "filename";
        "\002", "size";
        "\003", "type";
        "\004", "format";
      ]
    
    
    let rec read_files s pos n left =
      if n = 0 then List.rev left else
      let mtime = get_int64_32 s pos in
(*      lprintf "file at pos %d" pos; lprint_newline (); *)
      let md4 = get_md4 s (pos+4) in
      let nblocks = get_int16 s (pos+20) in
(*      lprintf "nblocks = %d" nblocks; lprint_newline ();  *)
      let blocks = Array.init nblocks (fun i ->
            let b = get_md4 s (pos+22+16*i) in
(*            lprintf "b: [%s]" (String.escaped b);
            lprint_newline (); *)
            b
        ) in
      let pos = pos + 22 + 16 * nblocks in
      let tags, pos = get_tags s pos names_of_tag in
      read_files s pos (n-1) ({
          mtime = mtime;
          md4 = md4;
          blocks = blocks;
          tags = tags;
        } :: left)
    
    let read s =
      let nfiles = get_int s 1 in
      read_files s 5 nfiles []
    
    let write buf t =
      buf_int8 buf 14;
      buf_int buf (List.length t);
      List.iter (fun file ->
          buf_int64_32 buf file.mtime;
          buf_md4 buf file.md4;
          buf_int16 buf (Array.length file.blocks);
          Array.iter (buf_md4 buf) file.blocks;
          buf_tags buf file.tags names_of_tag
      ) t
    
    
    let print t =
      lprintf "KNOWN.MET: %d files" (List.length t); lprint_newline ();
      List.iter (fun f ->
          try
            lprintf "  FILE %s" (Md4.to_string f.md4);
            lprint_newline ();
            lprintf "  mtime: %s" (Int64.to_string f.mtime);
            lprint_newline ();
            lprintf "  Blocks: %d" (Array.length f.blocks);
            lprint_newline ();
            Array.iter (fun m ->
                lprintf "    %s" (Md4.to_string m);
                lprint_newline ();
            ) f.blocks;
            print_tags f.tags;
            lprint_newline ();
          with _ -> lprintf "EEEEE"; lprint_newline ();
      ) t;
      lprint_newline ();
      
      
  end

module Part = struct 
    
    type t = {
        md4 : Md4.t;
        blocks : Md4.t array;
        tags : tag list;
        absents : (int64 * int64) list;
      }
    
    let names_of_tag =
      [
        "\001", "filename";
        "\002", "size";
        "\004", "format";
        "\008", "downloaded";
        "\018", "diskname";
        "\019", "priority";
        "\020", "status";
      ]
    
    
    let rec read_file s pos =
(*      lprintf "file at pos %d" pos; lprint_newline (); *)
      let md4 = get_md4 s (pos) in
      let nblocks = get_int16 s (pos+16) in
(*      lprintf "nblocks = %d" nblocks; lprint_newline ();  *)
      let blocks = Array.init nblocks (fun i ->
            let b = get_md4 s (pos+18+16*i) in
(*            lprintf "b: [%s]" (String.escaped b);
            lprint_newline (); *)
            b
        ) in
      let pos = pos + 18 + 16 * nblocks in
      let tags, pos = get_tags s pos names_of_tag in
      let start_pos = ref Int64.zero in
      let absents = ref [] in
      List.iter (fun tag ->
          let s = tag.tag_name in
          if String.length s > 0 then
            let c = s.[0] in
            match c, tag.tag_value with
              '\t', Uint64 p -> start_pos := p;
            | '\n', Uint64 p -> 
                absents := (!start_pos, p) :: !absents;
            | _ -> ()
      ) tags;
      let absents = List.sort (fun (s1,e1) (s2,e2) ->
            compare s1 s2
        ) !absents in
      {
        md4 = md4;
        blocks = blocks;
        tags = tags;
        absents = absents;
      }
      
    let read s =
      assert (get_int8 s 0 = 224);
      (* assert (get_int s 1 = 0); *)
      read_file s 5
    
    let write buf file =
      buf_int8 buf 224;
      buf_int buf 0;
      buf_md4 buf file.md4;
      buf_int16 buf (Array.length file.blocks);
      Array.iter (buf_md4 buf) file.blocks;
      buf_tags buf file.tags names_of_tag
    
    
    let print f =
      try
        lprintf "  FILE %s" (Md4.to_string f.md4);
        lprint_newline ();
        lprintf "  Blocks: %d" (Array.length f.blocks);
        lprint_newline ();
        Array.iter (fun m ->
            lprintf "    %s" (Md4.to_string m);
            lprint_newline ();
        ) f.blocks;
        lprint_newline ();
        lprintf " Absent blocks:"; lprint_newline ();
        List.iter (fun (t1,n1) ->
            lprintf "%10s - %10s" (Int64.to_string t1)
            (Int64.to_string n1); lprint_newline ();
        ) f.absents;
        print_tags f.tags;
        lprint_newline ();
      with _ -> lprintf "EEEEE"; lprint_newline ();
          
    
  end
  
module Pref = struct
    
    type t = {
        md4: Md4.t;
        client_tags : tag list;
        option_tags : tag list;
      }
    
    let names_of_client_tag =
      [
        "\001", "name";
        "\017", "version";
        "\015", "port";
      ]
      
    let names_of_option_tag = []
      
    let read s =
      assert (get_int s 1 = 2);
      assert (get_int s 5 = 0);
      let md4 = get_md4 s 9 in
      assert (get_int16 s 25 = 0);
(*      lprintf "ntags : %d at pos %d" ntags 27; lprint_newline ();  *)
      let client_tags, pos = get_tags s 27 names_of_client_tag in
      
      assert (get_int s pos = 0);
      assert (get_md4 s (pos+4) = Md4.null);
      assert (get_int16 s (pos + 20) = 0);
      let option_tags, pos = get_tags s (pos+22) names_of_option_tag in
      
      {
        md4 = md4;
        client_tags = client_tags;
        option_tags = option_tags;
      }
      
    let print t = 
      lprintf "PREF.MET %s" (Md4.to_string t.md4);
      lprint_newline ();
      
      print_tags t.client_tags;
      lprint_newline ();
      
      print_tags t.option_tags;
      lprint_newline ()
(*
(14)
(2)(0)(0)(0)

(0)(0)(0)(0) # premier record: client desc
(18)(36)(161)(160)(102)(31)(245)(198)(40)(142)(6)(63)(5)(92)(71)(240) # md4
(0)(0)
(3)(0)(0)(0) # ntags
(2)
(1)(0)(1) # name
(5)(0)(118)(107)(105)(115)(109)
(3)
(1)(0)(17) # version
(57)(0)(0)(0)
(3)
(1)(0)(15) # port
(48)(17)(0)(0)

(0)(0)(0)(0) # second record
(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)(0)
(0)(0)
(28)(0)(0)(0) # ntags
(2)
(8)(0)(105)(110)(99)(111)(109)(105)(110)(103) "incoming"
(39)(0)(47)(104)(111)(109)(101)(47)(108)(101)(102)(101)(115)(115)(97)(110)(47)(101)(100)(111)(110)(107)(101)(121)(47)(99)(108)(105)(101)(110)(116)(50)(47)(105)(110)(99)(111)(109)(105)(110)(103) "/home/.../incoming"
(2) ...
  *)
  end
