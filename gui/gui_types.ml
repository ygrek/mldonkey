open Mftp

let version = 14
  
module Mp3tag = Mp3tag.Id3v1
  
type avi_info = {
    mutable avi_codec : string;
    mutable avi_width : int;
    mutable avi_height : int;
    mutable avi_fps : int;
    mutable avi_rate : int;
  }

  
type format =
  AVI of avi_info
| Mp3 of Mp3tag.tag
| FormatType of string * string
| Unknown_format

and result = {
    mutable result_names : string list;
    result_md4 : Md4.t;
    mutable result_size : int32;
    mutable result_filtered_out : int;
    result_tags : Mftp.tag list;
  }

and history_result = {
    mutable hresult_names : string list;
    hresult_md4 : Md4.t;
    mutable hresult_size : int32;
    hresult_tags : Mftp.tag list;
  }

and file_state =
  FileDownloading
| FileCancelled
| FilePaused
| FileDownloaded
| FileRemoved

    
type connection_state =
  NotConnected
| Connecting
| Connected_initiating
| Connected_busy
| Connected_idle
| Connected_queued
| Removed

and location_kind = 
  Known_location of Ip.t * int
| Indirect_location

and friend_kind =
  NotAFriend
| Friend
| FriendRemoved
