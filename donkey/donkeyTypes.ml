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

          
module SourcesQueue = struct
    type 'a t = {
        head : (unit -> int * 'a);
        put : (int * 'a -> unit);
        length : (unit -> int);
        take : (unit -> int * 'a);
        iter : ( ('a -> unit) -> unit);
        put_back : (int * 'a -> unit);
      }
      
    let head t = t.head ()
    let put t x = t.put x
    let iter f t = t.iter f
    let length t = t.length ()
    let take t = t.take ()
    let put_back t e = t.put_back e
  end

type request_record = {
  mutable last_request : int;
  mutable nwarnings : int;
}
  
type client_score =
  Client_not_connected 
| Client_has_file
| Client_has_priority_file
| Client_has_chunk
| Client_has_priority_chunk
| Client_has_upload
| Client_has_priority_upload
  
type server = (*[]*){
    mutable server_server : server CommonServer.server_impl;
    mutable server_ip : Ip.t;
    mutable server_cid : Ip.t;
    mutable server_port : int;
    mutable server_sock : TcpBufferedSocket.t option;
    mutable server_nqueries : int;
    mutable server_search_queries : CommonTypes.search Fifo.t;
    mutable server_users_queries : bool Fifo.t;
    mutable server_connection_control : connection_control;
    mutable server_score : int;
    mutable server_tags : CommonTypes.tag list;
    mutable server_nusers : int;
    mutable server_nfiles : int;
    mutable server_name : string;
    mutable server_description : string;
    mutable server_users: user list;
    mutable server_next_udp : int;
    mutable server_master : bool;
    mutable server_mldonkey : bool;
    mutable server_last_message : int; (* used only by mldonkey server *)

    mutable server_id_requests : file option Fifo.t;
    
    mutable server_queries_credit : int;
    mutable server_waiting_queries : file list;
  } 


and user = {
    user_user : user CommonUser.user_impl;
    user_md4 : Md4.t;
    mutable user_name : string;
    user_ip : Ip.t;
    user_port : int;
    user_tags : CommonTypes.tag list;
    user_server : server;
  }

(*
and server_search = {
    search_search : CommonTypes.search;
    mutable nhits : int;
  }
*)

(*
and local_search = {
    search_search : CommonTypes.search;
    mutable search_handler : (search_event -> unit);
    mutable search_xs_servers : server list;
    mutable search_overnet : bool;
  }
*)

and result = {
    result_result : result CommonResult.result_impl;
    mutable result_index : Store.index;
  }

and search_event =
(*  Result of result *)
| Waiting of int

and file_change_kind = 
  NoFileChange
| FileAvailabilityChange
| FileInfoChange

and client_change_kind = 
  NoClientChange
| ClientStateChange
| ClientFriendChange
| ClientInfoChange
| ClientFilesChange

and server_change_kind = 
  NoServerChange
| ServerStateChange
| ServerUsersChange
| ServerInfoChange
| ServerBusyChange

and availability = bool array

and brand = 
  Brand_unknown
| Brand_edonkey
| Brand_mldonkey1
| Brand_mldonkey2
| Brand_mldonkey3
| Brand_overnet
| Brand_oldemule
| Brand_newemule
| Brand_server

and challenge = {
    mutable challenge_md4 : Md4.t;
    mutable challenge_solved : Md4.t;
    mutable challenge_ok : bool;
  }
  
and client = {
    client_client : client CommonClient.client_impl;
    mutable client_kind : location_kind;
    mutable client_source : source option;
    mutable client_md4 : Md4.t;
    mutable client_chunks : availability;
    mutable client_sock : TcpBufferedSocket.t option;
    mutable client_power : int ;
    mutable client_block : block option;
    mutable client_zones : zone list;
    mutable client_connection_control : connection_control;
    mutable client_file_queue : (file * availability) list;
    mutable client_next_view_files :  int;
    mutable client_all_files : result list option;
    mutable client_tags: CommonTypes.tag list;
    mutable client_name : string;
    mutable client_all_chunks : string;
    mutable client_rating : int ;
    mutable client_upload : upload_info option;
    mutable client_checked : bool;
    mutable client_chat_port : int ;
    mutable client_connected : bool;
(* statistics *)
    mutable client_last_filereqs : int;    
    mutable client_downloaded : Int64.t;
    mutable client_uploaded : Int64.t;
    mutable client_brand : brand;
    mutable client_banned : bool;
    mutable client_has_a_slot : bool;
    mutable client_overnet : bool;
    mutable client_score : int;
    mutable client_files : file_request list;
    mutable client_next_queue : int;
    mutable client_rank : int;
    mutable client_connect_time : int;
    mutable client_requests_received : int;
    mutable client_requests_sent: int;
    mutable client_from_queues : file list;
  }
  
and upload_info = {
    mutable up_file : file;
    mutable up_pos : int64;
    mutable up_end_chunk : int64;
    mutable up_chunks : (int64 * int64) list;
    mutable up_waiting : bool;
  }
  
and chunk = 
  PresentTemp
| AbsentTemp
| PartialTemp of block
| PresentVerified
| AbsentVerified
| PartialVerified of block
  
and block = {
    mutable block_present: bool;
    block_begin : int64;
    block_end : int64;
    mutable block_zones : zone list;
    mutable block_nclients : int;
    mutable block_pos : int;
    block_file : file;
  }
  
and zone = {
    mutable zone_begin : int64;
    mutable zone_end : int64;
    mutable zone_nclients : int;
    mutable zone_present : bool;
  }
  
and source = {
    source_num : int;
    source_addr : Ip.t * int;
    mutable source_client: client_kind;     
(* This field is not kept up-to-date when source_client = SourceClient c,
  change c.client_source_for *)
    mutable source_files : file_request list;
    mutable source_overnet : bool;
    mutable source_score : int;
    mutable source_age : int;
  }

  
and file_request = {
    request_file : file;
    mutable request_time : int;
    mutable  request_result : request_result;
  }

and request_result = 
| File_possible (* we asked, but didn't know *)
| File_not_found (* we asked, the file is not there *)
| File_expected (* we asked, because it was announced *)
| File_new_source (* we never asked *)
| File_found    (* the file was found *)
| File_chunk    (* the file has chunks we want *)
| File_upload   (* we uploaded from this client *)
  
and client_kind = 
  SourceClient of client
| SourceLastConnection of 
  int *
  int * (* last connection attempt *)
  int   (* booked client num *)
  
and file = {
    file_file : file CommonFile.file_impl;
    file_md4 : Md4.t;
    file_exists : bool;
    mutable file_nchunks : int;
    mutable file_chunks : chunk array;
    mutable file_chunks_order : int array;
    mutable file_chunks_age : int array;
(*    mutable file_all_chunks : string; *)
    mutable file_absent_chunks : (int64 * int64) list;
    mutable file_filenames : string list;
    mutable file_nsources : int;
    mutable file_md4s : Md4.t list;
    mutable file_format : format;
    mutable file_available_chunks : int array;
    mutable file_shared : file CommonShared.shared_impl option;
    mutable file_locations : client Intmap.t; 
    mutable file_mtime : float;
    mutable file_initialized : bool;
(* Source management number 3 !! *)
    mutable file_clients : (client * int) Fifo.t;
    mutable file_sources : source SourcesQueue.t array;
  }

and file_to_share = {
    shared_name : string;
    shared_size : int64;
    mutable shared_list : Md4.t list;
    mutable shared_pos : int64;
    mutable shared_fd : Unix32.t;
    shared_shared : file_to_share CommonShared.shared_impl;
  }
  
module UdpClientMap = Map.Make(struct
      type t = location_kind
      let compare = compare
    end)

  
type udp_client = {
    udp_client_ip : Ip.t;
    udp_client_port : int;
    udp_client_can_receive : bool;
    mutable udp_client_last_conn : int;
  }
  
and file_group = {
    mutable group : udp_client UdpClientMap.t;
  }


type old_result = result
    
exception NoSpecifiedFile
exception Already_done

type shared_file_info = {
    sh_name : string;
    sh_md4s : Md4.t list;
    sh_mtime : float;
    sh_size : int64;
  }

  
open CommonFile
open CommonClient
open CommonServer
open CommonResult 
        
let set_client_state c s =
  let cc = as_client c.client_client in
  set_client_state cc s
        
let set_client_disconnected c =
  let cc = as_client c.client_client in
  set_client_disconnected cc
   
let set_server_state s state =
  let ss = as_server s.server_server in
  if server_state ss <> state then begin
      set_server_state ss state;
    end
  
let file_state file =
  CommonFile.file_state (as_file file.file_file)
  
let file_last_seen file = file.file_file.impl_file_last_seen
  
let file_must_update file =
  file_must_update (as_file file.file_file)
    
let client_state client =
  CommonClient.client_state (as_client client.client_client)
    
let client_new_file client r =
  client_new_file (as_client client.client_client) ""
  (as_result r.result_result)
    
let server_state server =
  CommonServer.server_state (as_server server.server_server)

    
module SourcesQueueCreate = struct  
    open SourcesQueue
    
    let fifo () = 
      let t = Fifo.create () in
      {
        head = (fun _ -> Fifo.head t);
        put = (fun x -> Fifo.put t x);
        length = (fun _ -> Fifo.length t);
        take = (fun _ -> Fifo.take t);
        iter = (fun f -> Fifo.iter (fun (_,x) -> f x) t);
        put_back = (fun e -> Fifo.put_back_ele t e);
      }

    let lifo () = 
      let t = ref [] in
      {
        head = (fun _ -> match !t with 
              [] -> raise Fifo.Empty | x :: _ -> x);
        put = (fun x -> t := x :: !t);
        length = (fun _ -> List.length !t);
        take = (fun _ -> match !t with 
              [] -> raise Fifo.Empty | x :: tail -> 
                t:=tail; x);        
        iter = (fun f -> List.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := e :: !t);
        }      
      
    module SourcesSet = Set.Make (
        struct
          type t = int * source
          let compare (t1,s1) (t2,s2) = 
            if s1.source_addr = s2.source_addr then begin
                0 end else              
            let x = compare t1 t2 in
            if x = 0 then compare s1.source_addr s2.source_addr else x
        end
      )

    let oldest_first () = 
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> try SourcesSet.min_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try 
              let x = SourcesSet.min_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

    let oldest_last () = 
      let t = ref SourcesSet.empty in
      {
        head = (fun _ ->
            try SourcesSet.max_elt !t with _ -> raise Fifo.Empty);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            try
              let x = SourcesSet.max_elt !t in
              t := SourcesSet.remove x !t;
              x
            with _ -> raise Fifo.Empty);
        iter = (fun f ->
            SourcesSet.iter (fun (_,x) -> f x) !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
      }

      (*
    let max_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.max_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.max_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }

    let min_first compare =
      let module SourceSet = Set.Make(struct
            type t = source
            let compare = compare
          end) in
      let t = ref SourcesSet.empty in
      {
        head = (fun _ -> SourcesSet.min_elt !t);
        put = (fun x ->  t := SourcesSet.add x !t);
        length = (fun _ -> SourcesSet.cardinal !t);
        take = (fun _ ->
            let x = SourcesSet.min_elt !t in
            t := SourcesSet.remove x !t;
            x);
        iter = (fun f ->
            SourcesSet.iter f !t);
        put_back = (fun e -> t := SourcesSet.add e !t);
        }
*)
  end
