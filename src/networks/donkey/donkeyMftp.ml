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
open LittleEndian
open Int64ops

open CommonTypes
open CommonGlobals
open CommonOptions
  
let output_int32_8 oc i =
  output_char oc (char_of_int (Int64.to_int (Int64.logand i 255L)))

let output_int32_32 oc i =
  output_int32_8 oc i;
  output_int32_8 oc (right64 i  8);
  output_int32_8 oc (right64 i  16);
  output_int32_8 oc (right64 i  24)
  
let output_int8 oc i =
  output_char oc (char_of_int (i land 255))
  
let output_int oc i =
  output_int8 oc i;
  output_int8 oc (i lsr 8);
  output_int8 oc (i lsr 16);
  output_int8 oc (i lsr 24)
        
(*  
let buf_int buf int =
  buf_int32_32 buf (Int32.of_int int)
  *)

let rec rev_assoc x = function
    [] -> raise Not_found
  | (b,a)::l -> if a = x then b else rev_assoc x l

let buf_string buf s =
  buf_int16 buf (String.length s);
  Buffer.add_string buf s

let buf_port buf port =
  buf_int16 buf port
        

let buf_addr buf (ip,port) =
  buf_ip buf ip;
  buf_port buf port

let buf_tag buf tag names_of_tag =
  let name = try rev_assoc tag.tag_name names_of_tag 
    with _ -> string_of_field tag.tag_name
  in
(* try
            let i = rev_assoc name names_of_tag in
            String.make 1 (char_of_int i)
          with _ -> name *) 
  match tag.tag_value with
  | Uint64 n -> 
      buf_int8 buf 3;
      buf_string buf name;
      buf_int64_32 buf n
  | Fint64 n -> 
      buf_int8 buf 4;
      buf_string buf name;
      buf_int64_32 buf n
  | Addr ip -> assert false
  | String s -> 
      buf_int8 buf 2;
      buf_string buf name;
      buf_string buf s
  | Uint16 n ->
      buf_int8 buf 8;
      buf_int16 buf n
  | Uint8 n ->
      buf_int8 buf 9;
      buf_int8 buf n
  | Pair _ -> assert false
      
let buf_tags buf tags names_of_tag =
  buf_int buf (List.length tags);
  let rec iter_tags tags =
    match tags with
      [] -> ()
    | tag :: tags ->
        buf_tag buf tag names_of_tag;
        iter_tags tags
  in
  iter_tags tags
  
let read_uint8 ic =
  Int64.of_int (int_of_char (input_char ic))
  
let read_uint64_32 ic =
  let a0 = read_uint8 ic in
  let a1 = read_uint8 ic in
  let a2 = read_uint8 ic in
  let a3 = read_uint8 ic in
  a0 ++ (left64 a1  8) ++ (left64 a2 16) ++ (left64 a3 24) 

let read_request ic =
  let c = int_of_char (input_char ic) in
  assert (c = 227);
  let len32 = read_uint64_32 ic in
  let len = Int64.to_int len32 in
  let s = String.create len in
  really_input ic s 0 len;
  (*
  lprintf "read_request %d [%s]" len (String.escaped s); 
lprint_newline ();
  *)
  Bytes.to_string s

let output_request oc s =
  output_char oc (char_of_int 227);
  let len = String.length s in
  (*
  lprintf "output_request %d [%s]" len (String.escaped s); 
lprint_newline ();
  *)
  output_int oc len;
  output_string oc s
  
let get_port s pos =
  get_int16 s pos

let get_string = get_string16
  
let get_tag (names_of_tag : (string * field) list) s pos =
  let t2 = get_uint8 s pos in
  let name, pos2 = 
    if t2 land 0x80 = 0x80 then
      String.sub s (pos+1) 1, pos+2
    else
      get_string s (pos+1)
  in
(*  lprintf "tag name = %s" (String.escaped name);   *)
  let t = t2 land 0x7f in
  let v, pos = match t with
    | 2 -> let v, pos = get_string s pos2 in
        String v, pos
    | 1|3 -> let v = get_uint64_32 s pos2 in
        Uint64 v, pos2+4
    | 4 -> let v = get_uint64_32 s pos2 in
        Fint64 v, pos2+4
    | 8 -> let v = get_int16 s pos2 in
        Uint16 v, pos2 + 2
    | 9 -> let v = get_uint8 s pos2 in
        Uint8 v, pos2 + 1
    | _ when t >= 0x11 && t <= 0x20 -> let v = String.sub s pos2 (t-0x10) in
        String v, pos2 + t - 0x10
    | _ -> 
        if !verbose_unknown_messages then
          lprintf "get_tags: unknown tag %d at pos %d\n" t pos;
        raise Not_found
  in
  {
    tag_name = (try
        List.assoc name names_of_tag
      with Not_found ->
(*          lprintf "Unknown tag \"%s\"\n" (String.escaped name); *)
          match field_of_string name with
          | Field_KNOWN s -> Field_UNKNOWN s
          | field -> field);
    tag_value = v
  }, pos
  
let get_tags s pos names_of_tag =
  let rec iter_tags ntags pos tags =
    if ntags = 0 then List.rev tags, pos else
    let tag, pos = get_tag names_of_tag s pos in
    iter_tags (ntags-1) pos (tag :: tags) 
  in
  iter_tags (get_int s pos) (pos+4) []
  
let get_peer s pos =
  let ip = get_ip s pos in
  let port = get_port s (pos+4) in
  (ip,port), pos+6
    
module type Request = sig
    type t
    val parse: int -> string -> t
    val print: t -> unit
    val write: Buffer.t -> t -> unit
  end
  
let file_common_tags = [
    "\001", Field_Filename;
    "\002", Field_Size;
    "\003", Field_Type;
    "\004", Field_Format;
    "\005", Field_Lastseencomplete;
    "\021", Field_Availability;
    "\048", Field_Completesources;
    "\058", Field_Size_Hi;
    "\208", Field_Artist;
    "\209", Field_Album;
    "\210", Field_Title;
    "\211", Field_Medialength;
    "\212", Field_Bitrate;
    "\213", Field_Mediacodec;
    "\247", Field_Filerating;

    "Artist", Field_Artist;
    "Album", Field_Album;
    "Title", Field_Title;
  ]

let client_common_tags =
      [
        "\001", "name";
        "\015", "port";
        "\017", "version";
        "\031", "udpport";
        "\032", "compression";
        "\033", "udpport";
        "\034", "udpver";
        "\035", "sourceexchange";
        "\036", "comments";
        "\037", "extendedrequest";
        "\038", "compatibleclient";
        "\039", "features";
        "\059", "extrainfo";
        "\060", "downloadtime";
        "\061", "incompleteparts";
        "\062", "l2hac";
        "\063", "realparts";
        "\065", "mod_unknown41";
        "\066", "mod_unknown42";
        "\067", "mod_unknown43";
        "\078", "neo_features";
        "\084", "mod_featureset";
        "\085", "mod_version";
        "\086", "mod_protocol";
        "\090", "mod_bowlfish";
        "\092", "mod_secure_community";
        "\093", "mod_unknown0x5d";
        "\096", "mod_unknown0x60";
        "\100", "mod_unknown0x64";
        "\102", "mod_fusion";
        "\103", "mod_fusion_version";

(* http://forums.shareaza.com/showthread.php?threadid=37323&perpage=15&pagenumber=2 *)
        "\105", "edonkeyclc serverip?";
        "\106", "edonkeyclc serverport?";

        "\108", "mod_unknown0x6c";
        "\117", "mod_unknown0x75"; (* http://emule-project.net @ NewMule *)
        "\118", "mod_unknown0x76";
        "\119", "mod_tarod";
        "\120", "mod_tarod_version";
        "\121", "mod_morph";
        "\128", "mod_morph_version";
        "\130", "mod_mortillo";
        "\131", "mod_mortillo_version";
        "\132", "chanblard_version";
        "\133", "signature";
        "\134", "cache";
        "\135", "mod_lsd";
        "\136", "mod_lsd_version";
        "\144", "mod_lovelace_version";
        "\148", "os_info"; (* reused by aMule to transfer client OS type *)
        "\153", "mod_plus";
        "\160", "mod_wombat";
        "\161", "dev_wombat";
        "\170", "koizo"; (* http://sourceforge.net/projects/koizo *)
        "\205", "mod_unknown0xcd";
        "\224", "isp_bypass";
        "\225", "nat_tunneling";
        "\239", "emule_compatoptions";
        "\240", "nat_security";
        "\249", "emule_udpports";
        "\250", "emule_miscoptions1";
        "\251", "emule_version";
        "\252", "buddy_ip";
        "\253", "buddy_udp";
        "\254", "emule_miscoptions2";
        "pr",   "edonkeyclc horde";
        "wombia", "wombat a";
        "wombib", "wombat b";
      ]
