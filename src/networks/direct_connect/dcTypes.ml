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
  
(* TODO
- directory download...
- remove download list is not working
- normal mynick: Search unknown names by ip ?
- blink users if they have messages
- there is no method to clear message memory atm

verbose_msg_clients
verbose_msg_servers
"mr" | "raw" -> verbose_msg_raw := true
"mct" -> verbose_msg_clienttags := true
"ms" -> verbose_msg_servers := true
"verb" -> verbose := true
"sm" -> incr verbose_sources
"file" -> Unix32.verbose := true
"gui" -> GuiProto.verbose_gui_decoding := true
"no-login" -> verbose_no_login := true
"do" -> verbose_download := true
"up" -> verbose_upload := true
"unk" -> verbose_unknown_messages := true
"ov" -> verbose_overnet := true
"loc" -> verbose_location := true
"share" -> verbose_share := true
"md4" -> verbose_md4 := true
"connect" -> verbose_connect := true
"udp" -> verbose_udp := true
"ultra" | "super" -> verbose_supernode := true
"swarming" -> verbose_swarming := true
"hc" -> Http_client.verbose := true
"hs" -> Http_server.verbose := true
"act" -> verbose_activity := true
"bw" -> incr BasicSocket.verbose_bandwidth
"unexp" -> verbose_unexpected_messages := true
          verbose_sources := 1;

  mc : debug client messages
  mr|raw : debug raw messages
  mct : debug emule clients tags
  ms : debug server messages
  sm : debug source management
  net : debug net
  gui : debug gui
  no-login : disable login messages
  file : debug file handling
  do : some download warnings
  up : some upload warnings
  unk : unknown messages
  ov : overnet
  loc : debug source research/master servers
  share: debug sharing
  md4 : md4 computation
  connect : debug connections
  udp : udp messages
  ultra|super : debug supernode
  swarming : debug swarming
  hc : http_client messages
  hs : http_server messages
  act : debug activity
  bw : debug bandwidth
  unexp : debug unexpected messages

*)
  
type server = {
    server_server: server CommonServer.server_impl;
  server_connection_control : CommonTypes.connection_control;
  mutable server_connection_time: float;
    mutable server_name : string;
    mutable server_addr : Ip.addr;
  mutable server_ip : Ip.t;
  mutable server_port : int;
  mutable server_supports : dc_hub_supports option;
  mutable server_hub_state: dc_hub_state;
    mutable server_info : string;
    mutable server_sock : tcp_connection;
  mutable server_autoconnect : bool;
    mutable server_last_nick : string;
    mutable server_search : search option;
    mutable server_search_timeout : int;
  mutable server_users : user list;
  mutable server_topic : string;
  mutable server_messages : (int * string * room_message) list;
  mutable server_read_messages : int;
  }

  (*
and result = {
    result_result : result CommonResult.result_impl;
    result_name : string;
    result_size : int64;
    mutable result_sources : (user * string) list;
  }
*)
  
and user = {
    user_user : user CommonUser.user_impl; 
  mutable user_nick : string;
  mutable user_ip : Ip.addr; (* $IpUser *)
    mutable user_servers : server list;
  mutable user_clients : client list;
    mutable user_link : string;
  mutable user_uploaded : int64;
  mutable user_downloaded : int64;
  mutable user_myinfo : dc_myinfo;
    mutable user_data : float;
  mutable user_type : dc_usertype;
  mutable user_state : dc_userstate;
  mutable user_messages : (int * string * room_message) list;
  mutable user_read_messages : int;
  }

and file = {
    file_file : file CommonFile.file_impl;
  mutable file_unchecked_tiger_root : string;
  mutable file_directory : string;
  mutable file_name : string;
  (*mutable file_tiger_array : TigerTree.t array;*)
    mutable file_clients : client list;
  mutable file_search : (CommonTypes.search * float) option; (* search , time *)
  mutable file_autosearch_count : int;
  }

and dc_shared_file = {
  mutable dc_shared_fullname : string;
  mutable dc_shared_codedname : string;
  mutable dc_shared_searchname : string;
  mutable dc_shared_size : int64;
  mutable dc_shared_tiger_list : TigerTree.t list;
  mutable dc_shared_tiger_root : string;
  (*mutable dc_shared_tiger_array : TigerTree.t array;*)
  mutable dc_shared_pos : int64;
  mutable dc_shared_chunks : int;
}

and client = {  (* remember DcGlobals.copy_client if add anything *)
    client_client : client CommonClient.client_impl;
  mutable client_name : string option;
    mutable client_addr : (Ip.t * int) option;
    mutable client_sock : tcp_connection;
  mutable client_supports : dc_client_supports option;
  mutable client_lock : string;
  mutable client_file : file option;
  mutable client_state : dc_clientstate;
  mutable client_error : dc_client_error; (* last error message  *)
  mutable client_error_count : int;       (* error message count *)
  mutable client_preread_bytes_left : int;
    mutable client_pos : int64;
  mutable client_endpos : int64;
    mutable client_receiving : int64;
  mutable client_user : user option;
    mutable client_connection_control : connection_control;
  }
  
and dc_result = { (* rst of the info (filename & size) are on Commonresults.result_info *)
  user : user;
  tth : string; 
  directory : string;
} 

and dc_direction = Upload of int | Download of int

and dc_connection_style = (* dc_direction is what client wants/needs to do *)
  | ClientActive of dc_direction 
  | MeActive of dc_direction  

(* Client to client procedure:    S=Server M=Me C=Client
Me in Active mode - Client in Active/Passive mode: Me want to download
FROM______________________________________TO__USER_STATE________________CLIENT_STATE______________
                                              UserIdle                  (No client yet)
                                              UserActiveMeInitiating    DcDownloadWaiting file          
M  ConnectToMe                        ->  S   .                         .            
C  MyNick                             ->  M   UserIdle                  DcConnectionStyle MeActive Upload 0
C        |Lock                        ->  M   .                         .
M  MyNick|Lock|Supports|Direction|Key ->  C   .                         .
C  Supports|Direction                 ->  M   .                         If direction conflict loss
                                                                          DcConnectionStyle MeActive Download 65535
C                       | Key         ->  M   .                         If direction conflict loss
                                                                          Wait client commands for uploading
                                                                        else
                                                                          DcDownload file
M  Get/AdcGet                         ->  C   .                               
C  Filelength/AdcSnd                  ->  M   .                         
M  Send (if not ADC)                  ->  C   .

Me in Passive mode - Client in Active: Me want to download
FROM______________________________________TO__USER_STATE________________CLIENT_STATE______________
                                              UserIdle                  (No client yet)
                                              UserActiveUserInitiating  DcDownloadWaiting file
M  RevConnectToMe                     ->  S   .                         .
S  ConnectToMe                        ->  M   .                         DcConnectionStyle ClientActive Upload 0    
M  MyNick|Lock                        ->  C   .                         .
C  MyNick                             ->  M   UserIdle                  DcConnectionStyle ClientActive Upload 0
C        |Lock|Supports|Direction     ->  M   .                         If direction conflict loss
                                                                          DcConnectionStyle ClientActive Download 65535
C                                |Key ->  M   .                         If direction conflict loss
                                                                          Wait client commands for uploading
M  Supports                           ->  C   .                         DcConnectionStyle ClientActive Upload level
M          |Direction|Key             ->  C   .                         DcDownload file                               
M                        |Get/AdcGet  ->  C   .
C  Filelength/AdcSnd                  ->  M   .
M  Send ((if not ADC)                 ->  C   .

Me in Active/Passive mode - Client in Active: Client wants to download
FROM______________________________________TO__USER_STATE________________CLIENT_STATE______________
                                              UserIdle                  (No client yet)
S  ConnectToMe                        ->  M   .                         DcConnectionStyle ClientActive Upload 0
M  MyNick|Lock                        ->  C   .                         .
C  MyNick                             ->  M   UserIdle                  DcConnectionStyle ClientActive Download 0
C        |Lock|Supports|Direction     ->  M   .                         .
C                                |Key ->  M   .                         .
M  Supports                           ->  C   .                         DcConnectionStyle ClientActive Download level
M          |Direction|Key             ->  C   .                         .
C  Get/AdcGet                         ->  M   .                         If filelist loading
                                                                          DcUploadListStarting
                                                                        else  
                                                                          DcUploadStarting
M  Filelength/AdcSnd                  ->  C   .                         .
C  Send (if not ADC)                  ->  M   .                         DcUpload/DcUploadList


Me in Active mode - Client in Passive: Client wants to download
FROM______________________________________TO__USER_STATE________________CLIENT_STATE______________
                                              UserIdle                  (No client yet)
S  RevConnectToMe                     ->  M   UserPassiveUserInitiating (No client yet)
M  ConnectToMe                        ->  S   .                         (No client yet)
C  MyNick                             ->  M   .                         DcConnectionStyle MeActive Download 0
C        |Lock                        ->  M   .                         .
M  MyNick|Lock|Supports               ->  C   .                         DcConnectionStyle MeActive Download 0
M                      |Direction|Key ->  C   .                         .
C  Supports|Direction|Key             ->  M   .                         .
C  Get/AdcGet                         ->  M   .                         If filelist loading
                                                                          DcUploadListStarting
                                                                        else
                                                                          DcUploadStarting
M  Filelength/AdcSnd                  ->  C   .                         .
C  Send (if not ADC)                  ->  M   .                         DcUpload/DcUploadList

  *)

and dc_clientstate =
| DcIdle                                      (* client is doing nothing *)
| DcDownloadWaiting of file                   (* client is waiting for downloading slot *)
| DcDownloadConnecting of (file * float)      (* client has passed from waiting state to initializing state *)	
| DcDownloadListWaiting                       (* client is waiting a slot for list downloading *)
| DcDownloadListConnecting of (int * bool * float)(* client has passed waiting state to initializing state,     *)
                                                  (* int = $Direction level , bool = firewalled state , timeout *)
| DcConnectionStyle of (dc_connection_style)  (* how the initialization is processed with clients *)
| DcDownload of file		              (* we are downloading a file from client, switched on $Get *)
| DcDownloadList of Unix32.t                  (* we are downloading a file list from client *) 
| DcUploadStarting of (dc_shared_file * int64 * int64)    (* we are starting to upload file to client *) 
| DcUpload of (dc_shared_file * Unix32.t * int64 * int64) (* we are uploading a file to client *)
| DcUploadListStarting of string              (* we are starting to upload file with file name *)
| DcUploadList of Unix32.t                    (* we are uploading file list to client *)
| DcUploadDoneWaitingForMore                  (* upload is complete and we are waiting for more *)

and dc_userstate = (* used for determining states with users/clients *)
| UserIdle
| TryingToSendFirstContact
| UserActiveMeInitiating             (* We have sent ConnectToMe to active client                        *)
| UserActiveUserInitiating           (* We have sent RevConnect                                          *)
| UserPassiveUserInitiating of float (* User has sent RevConnect and we have answered, time of reveiving *)

and dc_client_error =
| NoError
| NoFreeSlots
| FileNotAvailable
| UserNotReachable
| ClosedOnInit
| ConnectionResetByPeer
| UploadError
| UserDontReplyOnTime

(* Type for keeping and manipulation hublist *)
and dc_hub = {
  mutable dc_name : string;
  mutable dc_ip : Ip.addr;
  mutable dc_port : int;
  mutable dc_info : string;
  mutable dc_nusers : int;
}
  
and sizelimit = 
| AtLeast of int64
| AtMost of int64
| NoLimit

(* Type for $Supports usage with hubs *)
and dc_hub_supports = {
  nogetinfo : bool;   
  nohello : bool;     
  userip2 : bool;
  usercommand : bool;
  tthsearch : bool;
  opplus : bool;
  feed : bool;
  mcto : bool;
  hubtopic : bool;
}

(* Type for $Supports usage with clients *)
and dc_client_supports = {
  bzlist : bool;
  minislots : bool;
  getzblock : bool;
  xmlbzlist : bool;
  adcget : bool;
  tthl : bool;
  tthf : bool;
  zlig : bool;
  clientid : bool;
  chunk : bool;
  gettestzblock : bool;
  getcid : bool;
}

and dc_supports = 
  | ClientSupports of dc_client_supports 
  | HubSupports of dc_hub_supports

and dc_hub_state =
   | Waiting 
   | User    
   | Vipped
   | Opped


and dc_usertype =
   | Normal
   | Vip
   | Op

and dc_myinfo = {
  mutable dest : string;
  mutable nick : string;
  mutable description : string;
  mutable client_brand : string;
  mutable version: string;
  mutable mode : char;
  mutable hubs :  int * int * int;
  mutable slots : int;
  mutable open_upload_slot : int;
  mutable conn_speed : string;
  mutable flag : int;
  mutable email : string;
  mutable sharesize : int64;
  mutable bwlimit : int;
}

and dc_mylistnode = (* type for mylist parsing *)
   | MylistDirectory of (string * dc_mylistnode list ref)
   | MylistFile of (string * string) (* filename * size *)

and adc_type = AdcTthl | AdcFile

and dc_shared_tree =
  {
    shared_dirname : string;
    mutable shared_files : dc_shared_file list;
    mutable shared_dirs : (string * dc_shared_tree) list;
  }

