(*
Download the server list from:
  
http://www.soulseek.org/slskinfo

135
www.soulseek.org/slsk135.exe
135: THIS UPDATE IS NECESSARY TO BE ABLE TO LOG ON! Fixed faulty firewall detection that could cause undue browsing and file transfer difficulties. Changed protocol to conserve bandwidth. Fixed transfer looping bug.
--servers
Soulseek Classic - Electronic music:for the sharing of electronic music:sk.nikita.cx:2240
Test Server:Offline 99% of the time:sk.nikita.cx:2242


  
 for listenport in range(2234,2240):

""" Server and peers send each other small binary messages, that start
    with length and message code followed by the actual messsage data. 
    These are the codes."""
    servercodes = {Login:1,SetWaitPort:2,
                   GetPeerAddress:3,AddUser:5,GetUserStatus:7,SayChatroom:13,
                   JoinRoom:14,LeaveRoom:15,UserJoinedRoom:16,UserLeftRoom:17,
                   ConnectToPeer:18,MessageUser:22,MessageAcked:23,
                   FileSearch:26,GetUserStats:36,QueuedDownloads:40,
                   PlaceInLineResponse:60,RoomAdded:62,RoomRemoved:63,
                   RoomList:64,ExactFileSearch:65,AdminMessage:66, 
                   GlobalUserList:67,TunneledMessage:68,PrivilegedUsers:69,
                   CantConnectToPeer:1001}
    peercodes = {GetSharedFileList:4, SharedFileList:5, FileSearchResult:9,
                UserInfoRequest:15,UserInfoReply:16, FolderContentsRequest:36,
                FolderContentsResponse:37, TransferRequest:40,
                TransferResponse:41,TransferSomething:42,PlaceInLine:44,
                TransferSomething2:46}
*)
