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

open CommonGlobals
open CommonTypes
open GuiTypes
    
exception UnsupportedGuiMessage
  
type from_gui =
(* These two messages are protocol independant: they MUST be sent to
  establish the connection. *)
| GuiProtocol of int
| Password of string
  
| ConnectMore_query
| CleanOldServers
| KillServer
| ExtendedSearch
| Search_query of query_entry search_request
| Download_query of string list * int 
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
| ModifyMp3Tags of int * Mp3tag.tag
| ForgetSearch of int
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
  
(* New messages from protocol 3 *)
| GuiExtensions of (int * bool) list
| GetConnectedServers
| GetDownloadFiles
| GetDownloadedFiles
| MessageToClient of int * string
| SetRoomState of int * room_state
  
(* New messages from protocol 4  *)
| RefreshUploadStats

  
let gui_extension_poll = 1
  
type to_gui =
(* This message is the first message sent by the core *)
| CoreProtocol of int
  
| Options_info of (string * string) list (*  options *)
| DefineSearches of (string * CommonTypes.query_entry) list

| Result_info of result_info
  
| Search_result of int * int
| Search_waiting of int * int
  
| File_info of file_info
| File_downloaded of int * int32 * float
| File_availability of int * string * string
| File_source of int * int
  
| Server_busy of int * int * int
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
| Add_section_option of
(* section *) string * 
(* message *) string *
(* option_name *) string *
(* option_type *) option_widget
  
let from_gui_to_string t = 
  match t with
  | GuiProtocol _ -> "GuiProtocol"
  | ConnectMore_query -> "ConnectMore_query"
  | CleanOldServers -> "CleanOldServers"
  | KillServer -> "KillServer"
  | ExtendedSearch -> "ExtendedSearch"
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
  | ForgetSearch _ -> "ForgetSearch"
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
      