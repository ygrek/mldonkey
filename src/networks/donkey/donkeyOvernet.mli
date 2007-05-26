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

open Md4
open CommonTypes
  
type peer = 
  { 
    peer_md4 : Md4.t;
    mutable peer_ip : Ip.t;
    mutable peer_port : int; 
    mutable peer_tcpport : int;
    mutable peer_country_code : int option;
    mutable peer_kind : int;
    mutable peer_expire : int;
    mutable peer_last_send : int;
    peer_created : int;
  }

type search_kind = 
  Search_for_keyword of unit option
| Search_for_file
| Search_for_kind of int
  
type t =
| OvernetConnect of peer

| OvernetConnectReply of peer list

| OvernetPublicize of peer

| OvernetPublicized of peer option

| OvernetSearch of 
(* 2 is OK for most searches, number of replies? *) int * 
(* searched file or keyword *) Md4.t *
    Md4.t option (* Our UID *)
  
| OvernetSearchReply of 
  Md4.t *
  peer list (* the two closest peers in the binary tree of md4s *)

| OvernetGetSearchResults of 
  Md4.t * search_kind * int * int

(* KADEMLIA_SEARCH_RES *)
| OvernetSearchFilesResults of
(* query *)  Md4.t *
  (Md4.t * tag list) list (* results *)

(* KADEMLIA_SEARCH_RES *)  
| OvernetSearchSourcesResults of Md4.t * peer list
  
(* KADEMLIA_PUBLISH_REQ *)
| OvernetPublishFiles of 
(* keyword or file md4 *) Md4.t *
(* md4 of file or client md4 *) (Md4.t *  tag list) list

| OvernetPublishSources of 
(* keyword or file md4 *) Md4.t * peer list

(* Published or not Published ??? *)
| OvernetPublished of Md4.t

| OvernetNoResult of Md4.t

| OvernetUnknown of int * string

| OvernetGetMyIP of int

| OvernetGetMyIPResult of Ip.t

| OvernetGetMyIPDone

| OvernetFirewallConnection of Md4.t*int

| OvernetFirewallConnectionACK of Md4.t

| OvernetFirewallConnectionNACK of Md4.t

| OvernetPeerNotFound of peer

| OvernetUnknown21 of peer

exception MessageNotImplemented

val message_to_string : t -> string
  
module Make(Proto: sig

      val enable_overnet : bool Options.option_record
      val overnet_port : int Options.option_record
      val overnet_tcpport : int Options.option_record
      val overnet_section : Options.options_section

      val checking_kind_timeout : int

      val redirector_section : string
      val options_section_name : string
      val command_prefix : string
      val source_brand : bool
        
      val udp_send : UdpSocket.t -> Ip.t -> int -> bool -> t -> unit
      val udp_handler : (t -> UdpSocket.udp_packet -> unit) -> 
        UdpSocket.t -> UdpSocket.event -> unit
        
      val web_info : string
      val web_info_descr : string
    end) : sig
    
    val overnet_search : CommonTypes.search -> unit
    val recover_file : DonkeyTypes.file -> unit
    val enable : unit -> unit
    val disable : unit -> unit
    val gui_overnet_options_panel : (string * string * string) list
    val bootstrap : Ip.t -> int -> unit
    val cancel_recover_file : DonkeyTypes.file -> unit
    val forget_search : CommonTypes.search -> unit
  end

  
