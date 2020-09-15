(*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Printf
open Xml_types

exception Error of error
exception File_not_found of string

exception Not_element of xml
exception Not_pcdata of xml
exception No_attribute of string

let default_parser = XmlParser.make()

let pos source =
        let line, lstart, min, max = Xml_lexer.pos source in
        {
                eline = line;
                eline_start = lstart;
                emin = min;
                emax = max;
        }

let parse (p:XmlParser.t) (source:XmlParser.source) =
        (* local cast Xml.xml -> xml *)
        (Obj.magic XmlParser.parse p source : xml)

let parse_in ch = parse default_parser (XmlParser.SChannel ch)
let parse_string str = parse default_parser (XmlParser.SString str)

let parse_file f =
        let p = XmlParser.make() in
        let path = Filename.dirname f in
        XmlParser.resolve p (fun file -> 
                let name = (match path with "." -> file | _ -> path ^ "/" ^ file) in
                Xml_dtd.check (Xml_dtd.parse_file name)
        );
        parse p (XmlParser.SFile f)

let error_msg = function
        | UnterminatedComment -> "Unterminated comment"
        | UnterminatedString -> "Unterminated string"
        | UnterminatedEntity -> "Unterminated entity"
        | IdentExpected -> "Ident expected"
        | CloseExpected -> "Element close expected"
        | NodeExpected -> "Xml node expected"
        | AttributeNameExpected -> "Attribute name expected"
        | AttributeValueExpected -> "Attribute value expected"
        | EndOfTagExpected tag -> sprintf "End of tag expected : '%s'" tag
        | EOFExpected -> "End of file expected"

let error (msg,pos) =
        if pos.emin = pos.emax then
                sprintf "%s line %d character %d" (error_msg msg) pos.eline (pos.emin - pos.eline_start)
        else
                sprintf "%s line %d characters %d-%d" (error_msg msg) pos.eline (pos.emin - pos.eline_start) (pos.emax - pos.eline_start)
        
let line e = e.eline

let range e = 
        e.emin - e.eline_start , e.emax - e.eline_start

let abs_range e =
        e.emin , e.emax

let tag = function
        | Element (tag,_,_) -> tag
        | x -> raise (Not_element x)

let pcdata = function 
        | PCData text -> text
        | x -> raise (Not_pcdata x)

let attribs = function 
        | Element (_,attr,_) -> attr
        | x -> raise (Not_element x)

let attrib x att =
        match x with
        | Element (_,attr,_) ->
                (try
                        let att = String.lowercase att in
                        snd (List.find (fun (n,_) -> String.lowercase n = att) attr)
                with
                        Not_found ->
                                raise (No_attribute att))
        | x ->
                raise (Not_element x)

let children = function
        | Element (_,_,clist) -> clist
        | x -> raise (Not_element x)

(*let enum = function
        | Element (_,_,clist) -> List.to_enum clist
        | x -> raise (Not_element x)
*)

let iter f = function
        | Element (_,_,clist) -> List.iter f clist
        | x -> raise (Not_element x)

let map f = function
        | Element (_,_,clist) -> List.map f clist
        | x -> raise (Not_element x)

let fold f v = function
        | Element (_,_,clist) -> List.fold_left f v clist
        | x -> raise (Not_element x)

let buffer_escape b text =
  let l = String.length text in
  for p = 0 to l-1 do 
    match text.[p] with
    | '>' -> Buffer.add_string b "&gt;"
    | '<' -> Buffer.add_string b "&lt;"
    | '&' -> Buffer.add_string b "&amp;"
    | '\'' -> Buffer.add_string b "&apos;"
    | '"' -> Buffer.add_string b "&quot;"
    | '\x0A' -> Buffer.add_string b "&#x0A;"
    | '\x0D' -> Buffer.add_string b "&#x0D;"
    | c -> Buffer.add_char b c
  done

let escape s =
  let b = Buffer.create (String.length s) in
  buffer_escape b s;
  Buffer.contents b

let tmp = Buffer.create 200

let buffer_pcdata = buffer_escape tmp

let buffer_attr (n,v) =
        Buffer.add_char tmp ' ';
        Buffer.add_string tmp n;
        Buffer.add_string tmp "=\"";
        buffer_pcdata v;
        Buffer.add_char tmp '"'

let to_string x = 
        let pcdata = ref false in
        let rec loop = function
                | Element (tag,alist,[]) ->
                        Buffer.add_char tmp '<';
                        Buffer.add_string tmp tag;
                        List.iter buffer_attr alist;
                        Buffer.add_string tmp "/>";
                        pcdata := false;
                | Element (tag,alist,l) ->
                        Buffer.add_char tmp '<';
                        Buffer.add_string tmp tag;
                        List.iter buffer_attr alist;
                        Buffer.add_char tmp '>';
                        pcdata := false;
                        List.iter loop l;
                        Buffer.add_string tmp "</";
                        Buffer.add_string tmp tag;
                        Buffer.add_char tmp '>';
                        pcdata := false;
                | PCData text ->
                        if !pcdata then Buffer.add_char tmp ' ';
                        buffer_pcdata text;
                        pcdata := true;
        in
        Buffer.reset tmp;
        loop x;
        let s = Buffer.contents tmp in
        Buffer.reset tmp;
        s

  let to_string_fmt x =
  let rec loop ?(newl=false) tab = function
    | Element (tag,alist,[]) ->
        Buffer.add_string tmp tab;
        Buffer.add_char tmp '<';
        Buffer.add_string tmp tag;
        List.iter buffer_attr alist;
        Buffer.add_string tmp "/>";
        if newl then Buffer.add_char tmp '\n';
    | Element (tag,alist,[PCData text]) ->
        Buffer.add_string tmp tab;
        Buffer.add_char tmp '<';
        Buffer.add_string tmp tag;
        List.iter buffer_attr alist;
        Buffer.add_string tmp ">";
        buffer_pcdata text;
        Buffer.add_string tmp "</";
        Buffer.add_string tmp tag;
        Buffer.add_char tmp '>';
        if newl then Buffer.add_char tmp '\n';
    | Element (tag,alist,l) ->
        Buffer.add_string tmp tab;
        Buffer.add_char tmp '<';
        Buffer.add_string tmp tag;
        List.iter buffer_attr alist;
        Buffer.add_string tmp ">\n";
        List.iter (loop ~newl:true (tab^"  ")) l;
        Buffer.add_string tmp tab;
        Buffer.add_string tmp "</";
        Buffer.add_string tmp tag;
        Buffer.add_char tmp '>';
        if newl then Buffer.add_char tmp '\n';
    | PCData text ->
        buffer_pcdata text;
        if newl then Buffer.add_char tmp '\n';
  in
  Buffer.reset tmp;
  loop "" x;
  let s = Buffer.contents tmp in
  Buffer.reset tmp;
  s

let _ =
  XmlParser._raises (fun x p -> 
(* local cast : Xml.error_msg -> error_msg *)
      Error ((Obj.magic x : error_msg),pos p))
  (fun f -> File_not_found f)
  (fun x p -> Xml_dtd.Parse_error (x,
(* local cast : Xml.error_pos -> error_pos *)
        (Obj.magic (pos p))));
  Xml_dtd._raises (fun f -> File_not_found f)

let xml_of xml = match xml with
    Element (a,b,c) -> a,b,c
  | _ -> failwith "Xml.xml_of: bad XML type"

