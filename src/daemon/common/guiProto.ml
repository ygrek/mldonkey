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

open CommonTypes
open GuiTypes
    
exception UnsupportedGuiMessage
let verbose_gui_decoding = ref false
  

type gift_command = 
  GiftCommand of string * string option * gift_command list
  
let gui_extension_poll = 1
  
let to_gui_last_opcode = 60
let from_gui_last_opcode = 69
let best_gui_version = 41
  
(* I will try to report all changes to the protocol here: send me patches
if I don't !

Version 27: 
  GUI -> Core: message 4 [RESULT_INFO]
    Field result_md4 (16 bytes) has been replaced by result_uids (string list)
    A new Field result_time (4 bytes) has been added.
  Core -> GUI: In message 4 [RESULT_INFO], for protocols < 27, the MD4 sent
     is now invalid (filled with zeroes). Don't use it !
  GUI -> Core: new message [INTERESTED_IN_SOURCES]
    By sending (INTERESTED_IN_SOURCES interestedp), a GUI can declare
     whether it wants or not to receive information on sources.

Version 31:
  Core -> GUI: message 52 [FILE_INFO]
    Field file_uids is now encoded (string list) for protocol > 30
  Core -> GUI: message 48 [SHARED_FILE_INFO]
    Field shared_id (md4 - 16 bytes) has been replaced by shared_uids (string list)
    For protocols < 31, the MD4 sent is now invalid (filled with zeroes). Don't use it !

Version 32:
  Core -> GUI: message 52 [FILE_INFO]
    Field file_format now includes OGG files (as per OggDS, theora
    video and vorbis audio).
    'OGG' is used for files using the Ogg bitstream specification.
    See commonTypes.ml 'ogg_stream_info' for further details as well as
    guiEncoding.ml .
  GUI -> Core: message 66 [SERVER_RENAME]
  GUI -> Core: message 67 [SERVER_SET_PREFERRED]

Version 33:
  CORE -> GUI: message 15 [CLIENT_INFO] encoding and decoding of the field client_release 

Version 34:
  GUI -> CORE: message GetSysInfo
  CORE -> GUI: message SysInfo
  *)
  
  
type from_gui =
(* These two messages are protocol independant: they MUST be sent to
  establish the connection. *)
| GuiProtocol of int
| Password of string * string
  
| ConnectMore_query
| CleanOldServers
| KillServer
| ExtendedSearch of int * extend_search
| Search_query of query_entry search_request
| Download_query of string list * int * bool (* forced ? *)
| Url of string
| RemoveServer_query of int
| SaveOptions_query of (string * string) list (* options *)
| RemoveDownload_query of int
| ServerUsers_query of int
| SaveFile of int * string
| AddClientFriend of int
| AddUserFriend of int
| RemoveFriend of int
| RemoveAllFriends
| FindFriend of string
| ViewUsers of int
| ConnectAll of int
| ConnectServer of int
| DisconnectServer of int
| SwitchDownload of int * bool
| VerifyAllChunks of int
| QueryFormat of int
| ModifyMp3Tags of int * Mp3tag.Id3v1.tag
| CloseSearch of int * bool
| SetOption of string * string
| Command of string
| Preview of int
| ConnectFriend of int  
| GetServer_users of int
| GetClient_files of int
| GetFile_locations of int
| GetServer_info of int
| GetClient_info of int
| GetFile_info of int
| GetUser_info of int
| SendMessage of int * room_message
| EnableNetwork of int * bool
| BrowseUser of int
| AddServer_query of int * Ip.t * int

| ConnectClient of int
| DisconnectClient of int
  
(* New messages from protocol 3 *)
| GuiExtensions of (int * bool) list
| GetConnectedServers
| GetDownloadFiles
| GetDownloadedFiles
| MessageToClient of int * string
| SetRoomState of int * room_state
  
(* New messages from protocol 4  *)
| RefreshUploadStats
  
| SetFilePriority of int * int

| RenameFile of int * string
| GetUploaders
| GetPending
| GetSearches
| GetSearch of int

(* This message must be sent only to increase or decrease the protocol version
on some messages in the range accepted by the core, ie between 0 and the 
version sent in the CoreProtocol message. For other messages, the core uses
the mininum of the versions in the CoreProtocol and GuiProtocol messages.

If a GUI expects to use a special version for a message, and the version is
not supported by the core (greater than the CoreProtocol version), the GUI
should disconnect as the core will not be able to correctly encode or decode
the messages (it will use the version specified in CoreProtocol instead
 of the one expected by the GUI).
*)
  
| MessageVersions of (int * (* from gui ? *) bool * int) list

  
| GiftAttach of string * string * string
| GiftStats  

| NetworkMessage of int * string

(* Understood by core protocol 27 *)
| InterestedInSources of bool 
| GetVersion

(* Understood by core protocol 32 *)
| ServerRename of (int * string)
| ServerSetPreferred of (int * bool)
| GetStats of int

(* Understood by core protocol 34 *)
| GetSysInfo

type to_gui =
(* This message is the first message sent by the core *)
| CoreProtocol of int * int * int
  
| Options_info of Options.option_info list (*  options *)
| DefineSearches of (string * CommonTypes.query_entry) list

| Result_info of result_info
  
| Search_result of int * int * (result_info option)
| Search_waiting of int * int
  
| File_info of file_info
| File_downloaded of int * int64 * float * int
| File_add_source of int * int
| File_update_availability of int * int * string
| File_remove_source of int * int
  
| Server_busy of int * int64 * int64
| Server_user of int * int
| Server_state of int * host_state
| Server_info of server_info
  
| Client_info of client_info
| Client_state of int * host_state
| Client_friend of int * client_type
| Client_file of int * string * int

| Console of string

| Network_info of network_info
| User_info of user_info

| Room_info of room_info
| Room_message of int * room_message
| Room_add_user of int * int
  
| Client_stats of client_stats

(* New messages from protocol 3 *)
| ConnectedServers of server_info list
| DownloadFiles of file_info list
| DownloadedFiles of file_info list
| MessageFromClient of int * string

(* New message for protocol 4  *)
| Room_remove_user of int * int
| Shared_file_info of shared_info
| Shared_file_upload of int * (* upload *) int64 * (* requests *) int
| Shared_file_unshared of int

(* New message for protocol 5 *)
| Add_section_option of string * Options.option_info 
  
(* New message for protocol 6 *)
| Add_plugin_option of string * Options.option_info

| BadPassword
| CleanTables of (* clients *) int list * (* servers *) int list
| Uploaders of int list
| Pending of int list
| Search of string search_request
| Version of string
  
| GiftServerAttach of string * string
| GiftServerStats of (string * string * string * string) list
| Stats of int * (string * int * network_stat_info list) list  

| SysInfo of (string * string) list
  
let string_of_from_gui t = 
  match t with
  | GuiProtocol _ -> "GuiProtocol"
  | ConnectMore_query -> "ConnectMore_query"
  | CleanOldServers -> "CleanOldServers"
  | KillServer -> "KillServer"
  | ExtendedSearch _ -> "ExtendedSearch"
  | Password _ -> "Password"
  | Search_query _ -> "Search_query"
  | Download_query _ -> "Download_query"
  | Url _ -> "Url"
  | RemoveServer_query _ -> "RemoveServer_query"
  | SaveOptions_query _ -> "SaveOptions_query"
  | RemoveDownload_query _ -> "RemoveDownload_query"
  | ServerUsers_query _ -> "ServerUsers_query"
  | SaveFile _ -> "SaveFile"
  | AddClientFriend _ -> "AddClientFriend"
  | AddUserFriend _ -> "AddUserFriend"
  | RemoveFriend _ -> "RemoveFriend"
  | RemoveAllFriends -> "RemoveAllFriends"
  | FindFriend _ -> "FindFriend"
  | ViewUsers _ -> "ViewUsers"
  | ConnectAll _ -> "ConnectAll"
  | ConnectServer _ -> "ConnectServer"
  | DisconnectServer _ -> "DisconnectServer"
  | SwitchDownload _ -> "SwitchDownload"
  | VerifyAllChunks _ -> "VerifyAllChunks"
  | QueryFormat _ -> "QueryFormat"
  | ModifyMp3Tags _ -> "ModifyMp"
  | CloseSearch _ -> "CloseSearch"
  | SetOption _ -> "SetOption"
  | Command _ -> "Command"
  | Preview _ -> "Preview"
  | ConnectFriend _ -> "ConnectFriend"
  | GetServer_users _ -> "GetServer_users"
  | GetClient_files _ -> "GetClient_files"
  | GetFile_locations _ -> "GetFile_locations"
  | GetServer_info _ -> "GetServer_info"
  | GetClient_info _ -> "GetClient_info"
  | GetFile_info _ -> "GetFile_info"
  | GetUser_info n -> Printf.sprintf "GetUser_info %d" n
  | SendMessage _ -> "SendMessage"
  | EnableNetwork _ -> "EnableNetwork"
  | BrowseUser _ -> "BrowseUser"      

  | MessageToClient _ -> "MessageToClient"
  | GuiExtensions _ -> "GuiExtensions"
  | GetConnectedServers -> "GetConnectedServers"
  | GetDownloadFiles -> "GetDownloadFiles"
  | GetDownloadedFiles -> "GetDownloadedFiles"
  | SetRoomState _ -> "CloseRoom"
      
  | RefreshUploadStats -> "RefreshUploadStats"
  | SetFilePriority _ -> "SetFilePriority"      
  | AddServer_query _ -> "AddServer_query"
      
  | MessageVersions _ -> "MessageVersions"

  | RenameFile _ -> "RenameFile"
  | GetUploaders -> "GetUploaders"
  | GetPending -> "GetPending"
  | GetSearches -> "GetSearches"
  | GetSearch _ -> "GetSearch"

  | ConnectClient _ -> "ConnectClient"
  | DisconnectClient _ -> "DisconnectClient"
  | NetworkMessage _ -> "NetworkMessage"
      
  | GiftAttach _ -> "GiftAttach"
  | GiftStats -> "GiftStats"
  
  | InterestedInSources _ -> "InterestedInSources"
  | GetVersion -> "GetVersion"

  | ServerRename _ -> "ServerRename"
  | ServerSetPreferred _ -> "ServerSetPreferred"
  | GetStats _ -> "GetStats"

  | GetSysInfo -> "GetSysInfo"

let string_of_to_gui t =
  match t with
  
  | CoreProtocol _ -> "CoreProtocol"
  
  | Options_info _ -> "Options_info"
  | DefineSearches _ -> "DefineSearches"
  
  | Result_info _ -> "Result_info"
  
  | Search_result _ -> "Search_result"
  | Search_waiting _ -> "Search_waiting"
  
  | File_info _ -> "File_info"
  | File_downloaded _ -> "File_downloaded"
  | File_add_source _ -> "File_add_source"
  | File_update_availability _ -> "File_update_availability"
  | File_remove_source _ -> "File_remove_source"
  
  | Server_busy _ -> "Server_busy"
  | Server_user _ -> "Server_user"
  | Server_state _ -> "Server_state"
  | Server_info _ -> "Server_info"
  
  | Client_info _ -> "Client_info"
  | Client_state _ -> "Client_state"
  | Client_friend _ -> "Client_friend"
  | Client_file _ -> "Client_file"
  
  | Console _ -> "Console"
  
  | Network_info _ -> "Network_info"
  | User_info _ -> "User_info"
  
  | Room_info r -> Printf.sprintf "Room_info %d" r.room_num
  | Room_message _ -> "Room_message"
  | Room_add_user _ -> "Room_add_user"
  
  | Client_stats _ -> "Client_stats"

(* New messages from protocol 3 *)
  | ConnectedServers _ -> "ConnectedServers"
  | DownloadFiles _ -> "DownloadFiles"
  | DownloadedFiles _ -> "DownloadedFiles"
  | MessageFromClient _ -> "MessageFromClient"

(* New message for protocol 4  *)
  | Room_remove_user _ -> "Room_remove_user"
  | Shared_file_info _ -> "Shared_file_info"
  | Shared_file_upload _ -> "Shared_file_upload"
  | Shared_file_unshared _ -> "Shared_file_unshared"

(* New message for protocol 5 *)
  | Add_section_option _ -> "Add_section_option"

(* New message for protocol 6 *)
  | Add_plugin_option _ -> "Add_plugin_option"
  
  | BadPassword -> "BadPassword"
      
  | CleanTables _ -> "CleanTables"
  | Uploaders _ -> "Uploaders"
  | Pending _ -> "Pending"
  | Search _ -> "Search"
  | Version _ -> "Version"
      
  | GiftServerAttach _ -> "GiftServerAttach"
  | GiftServerStats _ -> "GiftServerStats"
  | Stats _ -> "Stats"

  | SysInfo _ -> "SysInfo"
      
type gui_record = {
    mutable gui_num : int;
    mutable gui_search_nums : int list;
    mutable gui_searches : (int * search) list;
    mutable gui_sock : TcpBufferedSocket.t option;
    mutable gui_proto_to_gui_version : int array;
    mutable gui_proto_from_gui_version : int array;
    mutable gui_auth : bool;
    mutable gui_poll : bool;

    mutable gui_send : (gui_record -> to_gui -> unit);
    mutable gui_result_handler : (int -> result -> unit);

    mutable gui_id_counter : int;
    mutable gui_initialized : bool;
    mutable gui_identifiers : (int * int, int) Hashtbl.t;
    mutable gui_identifiers_rev : (int, int * int) Hashtbl.t;
    
    gui_events : gui_events;
    gui_conn : ui_conn;
  }
