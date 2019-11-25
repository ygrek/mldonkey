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

open Printf2
open BasicSocket
open TcpBufferedSocket
open Options
open CommonOptions
open CommonUser
open CommonServer
open CommonTypes
open CommonGlobals

open DcTypes
open DcOptions
open DcGlobals
open DcProtocol

let log_prefix = "[dcSer]"
            
let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt
    
(*let rec remove_short list list2 =
  match list with
    [] -> List.rev list2
  | s :: list -> 
      if String.length s < 5 then 
        remove_short list list2
      else
        remove_short list (s :: list2) *)
          
(*let stem s =
  let s = String.lowercase (String.copy s) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'a'..'z' | '0' .. '9' -> ()
    | _ -> s.[i] <- ' ';
  done;
  remove_short (String2.split s ' ') [] *)
  
let server_addr s = Ip.string_of_addr s.server_addr

(* disconnect from DC hub *) 
let disconnect_server s reason =
  (match s.server_sock with
  | NoConnection -> ()
  | ConnectionWaiting token -> 
      cancel_token token;
      s.server_sock <- NoConnection
  | Connection sock ->
      if !verbose_msg_servers then
        lprintf_nl "Server (%s:%d) CLOSED connection for reason (%s)"
            (server_addr s) s.server_port (BasicSocket.string_of_reason reason); 
      (try TcpBufferedSocket.close sock reason with _ -> () );      
      connection_failed (s.server_connection_control);
      s.server_sock <- NoConnection;
      s.server_hub_state <- Waiting;
      set_server_state s (NotConnected (reason, -1));
      remove_connected_server s )
  
(* Server connection handler *) 
let server_handler s sock event = 
  (match event with
  | BASIC_EVENT (CLOSED r) ->
      disconnect_server s r     
  | _ -> () )

(* Get MyInfo Hubs information of connected servers atm *)
let get_myhubs_info () =
  let n_hubs = ref 0 in (* how many servers has sent $Hello *)
  let r_hubs = ref 0 in (* how many servers we have sent $MyPass *)
  let o_hubs = ref 0 in (* how many servers has sent $LogedIn *)
  List.iter (fun server ->
    (match server.server_sock with
    | Connection _ ->
      (match server.server_hub_state with
      | Opped -> incr o_hubs
      | Vipped -> incr r_hubs
      | User -> incr n_hubs
      | _ -> () )
    | _ -> () )
  ) !connected_servers;
  !n_hubs,!r_hubs,!o_hubs

(* MyInfo record sending *)
let create_myinfo s = (* every server is sent its own info (nick) (uptime) *) 
  (* <++ V:x,M:x,H:x/y/z,S:x[,O:x]>
     V(ersion) x = client version
     M(ode) x = mode (A = Active, P = Passive, 5 = SOCKS5)
     H(ubs) x = number of hubs connected to where you're not a registered user
            y = number of hubs you're registered in
            z = number of hubs you're registered as an operator
     S(lots) x = number of upload slots you have open (note that they may be in use already)
     O(pen an extra slot if speed is below)
             x = if total upload is below this value DC++ will open another slot
                 This part of the tag is only shown when the option for it is enabled.

     <flag> User status as ascii char (byte)
        1 normal - 2,3 away - 4,5 server - 6,7 server away - 8,9 fireball - 10,11 fireball away 
        * The server icon is used when the client has uptime > 2 hours, > 2 GB shared, upload > 200 MB.
        * The fireball icon is used when the client has had an upload > 100 kB/s. *)
   let version = Autoconf.current_version in
   let uptime = current_time () -. s.server_connection_time in
   let mode = ref ( if (!!firewalled = true) then 'P' else 'A') in
   let own_brand = "MLDC" in 
   let norm_hubs,reg_hubs,opped_hubs = get_myhubs_info () in
   let time_flag =  
     if (uptime > 7200.) && 
        (!nshared_bytes > (Int64.mul (Int64.of_int 2) int64_gbyte)) &&
        (!dc_total_uploaded > (Int64.mul (Int64.of_int 200) int64_mbyte)) then 5 
     else 1
   in
   {
     dest = "$ALL";
     nick = s.server_last_nick;
     client_brand = own_brand;
     version = version;
     mode = !mode;
     hubs = norm_hubs, reg_hubs, opped_hubs ; 
     slots = open_slots (); 
     open_upload_slot = 0; (*TODO Automatically open an extra slot if speed is below kB/s*)
     description = 
       Printf.sprintf "<%s V:%s,M:%c,H:%d/%d/%d,S:%d>" own_brand version !mode
         norm_hubs reg_hubs opped_hubs (open_slots ());
     conn_speed = !!client_speed;
     flag = time_flag;
     email = "";
     sharesize = DcShared.dc_updatesharesize ();
     bwlimit = !!max_hard_upload_rate;
   }

(* Send to all connected servers *)
let send_myinfo_connected_servers () =
  List.iter (fun s -> (* send myinfo to all servers *)
    (match s.server_sock with
    | Connection sock ->
        let dc_myinfo = create_myinfo s in
        dc_send_msg sock (MyInfoReq (dc_myinfo))
    | _ -> () )
  ) !connected_servers
      
(* Server message handler *)
let client_to_server s m sock = 
      
  (match m with
  
  | BadPassReq ->
      if !verbose_msg_servers then 
        lprintf_nl "Bad password for %S on %s" s.server_last_nick (Ip.string_of_addr s.server_addr);
      s.server_hub_state <- User

  | ConnectToMeReq t ->  (* client is unknown at this moment until $MyNick is received *)
      (try
        if !verbose_msg_clients then
          lprintf_nl "$ConnectToMe (%s:%d) (%s) (%s:%d)" 
            (server_addr s) s.server_port t.ConnectToMe.nick (Ip.to_string t.ConnectToMe.ip)
            t.ConnectToMe.port; 
        let c = new_client () in
        c.client_name <- Some t.ConnectToMe.nick; (* unknown *) 
        c.client_addr <- Some (t.ConnectToMe.ip, t.ConnectToMe.port);
        c.client_state <- DcConnectionStyle (ClientActive (Upload 0)); (* level is set later*)
        DcClients.connect_client c
      with e -> 
          if !verbose_unexpected_messages then lprintf_nl "%s in ConnectToMe sending" (Printexc2.to_string e) )
  
  | ForceMoveReq t ->
      lprintf_nl "Received ForceMove(%S) from %s" t (Ip.string_of_addr s.server_addr);
      disconnect_server s (Closed_for_error "Forcemove command received")
      
  | GetPassReq -> (* After password request from hub ... *)
      let addr = Ip.string_of_addr s.server_addr in
      let nick = s.server_last_nick in
      let rec loop i =
        (match i with
        | [] -> dc_send_msg sock ( MyPassReq "IDontKnowThePass" ) (* What to do when have no pass ? *)
        | (a , n, p) :: tl -> 
            if (a = addr) && (n = nick) then begin (* Send pass from .ini if present for this hub *)
              s.server_hub_state <- Vipped;  
              dc_send_msg sock ( MyPassReq p );
            end else loop tl )
      in
      loop !!hubs_passwords   
  
  | HelloReq n -> 
      if n = s.server_last_nick then begin (* if the $Hello is for me :) *)
        (match s.server_hub_state with
        | Waiting | Opped | Vipped ->    
          if !verbose_msg_servers then lprintf_nl "Connected to Hub: %s" s.server_name;
          add_connected_server s;
          set_server_state s (Connected (-1));
          dc_send_msg sock (VersionReq Autoconf.current_version);
          dc_send_msg sock (GetNickListReq); (* Send even if we don't need *)  
          set_rtimeout sock (float_of_int Date.half_day_in_secs);        (* set read socket timeout big enough *)
          if s.server_hub_state = Waiting then s.server_hub_state <- User; (* set state with hub to User *)
          ignore (new_user (Some s) s.server_last_nick);                   (* add myself to this serverlist for sure *)   
          let dc_myinfo = create_myinfo s in
          dc_send_msg sock (MyInfoReq (dc_myinfo))
        | User -> (* We have already passed servers negotiations once and are inside... *)
            set_rtimeout sock (float_of_int Date.half_day_in_secs))
      end else    (* $Hello from hub is not for me so it is a new user *)
        ignore (new_user (Some s) n) 
        
  | HubIsFullReq -> 
     disconnect_server s (Closed_for_error (Printf.sprintf "Hub %s is full" (Ip.string_of_addr s.server_addr) ) )
  
  | HubNameReq name ->
      s.server_name <- name;
      server_must_update s

  | HubTopicReq topic ->
      s.server_topic <- topic
  
  | LockReq lock -> (* After $Lock from hub, answer with possible $Supports followed by $Key and $ValidateNick *)
      if (lock.Lock.extended_protocol = true) then (* if EXTENDEDPROTOCOL sent from hub, send own $Supports *)
        dc_send_msg sock ( SupportsReq (HubSupports mldonkey_dc_hub_supports) );

      dc_send_msg sock ( KeyReq { Key.key = DcKey.calculate_key lock.Lock.key } ); (* Send $Key *)
      let addr = Ip.string_of_addr s.server_addr in (* current server ip as string *)
        let rec loop passline =
          match passline with
          | [] ->                  (* end of !!hubs_passwords option list *)
              let my_nick = local_login () in (* use global or local nick if nick/pass pair not set in .ini *)
              s.server_last_nick <- my_nick; 
              dc_send_msg sock (ValidateNickReq my_nick )
          | (a , n, _) :: tl -> (* on every line in !!hubs_passwords list check for match *)
              if (a = addr) then begin
                s.server_last_nick <- n;
                dc_send_msg sock ( ValidateNickReq n ) (* send a nick that has password match in .ini *)
              end else loop tl
        in
        loop !!hubs_passwords
  
  | LogedInReq n -> 
      if n = s.server_last_nick then begin (* if we are really opped *) 
        if !verbose_msg_servers then lprintf_nl "Opped to server %s" (Ip.string_of_addr s.server_addr);
        s.server_hub_state <- Opped;
        (*let dc_myinfo = create_myinfo s in
        dc_send_msg sock (MyInfoReq (dc_myinfo)) *)
      end
          
  | MessageReq t ->
      s.server_messages <- s.server_messages @ [
        (int_of_float (current_time ()), t.Message.from, PublicMessage (0,t.Message.message))];
  
  | MyInfoReq t ->
      let u = new_user (Some s) t.nick in
      (* some hubs send empty info fields, so son't update user if already info filled *)
      if (t.description = empty_string) && (u.user_myinfo.description <> empty_string) then ()
      else u.user_myinfo <- t; 
      (*if u.user_myinfo.conn_speed = "" then u.user_type <- Bot;*)
      user_must_update (as_user u.user_user);
      ignore (DcClients.ask_user_for_download u) (* start downloading pending load immediately *)
  
  | NickListReq list ->
      List.iter (fun nick ->
        let u = new_user (Some s) nick in
          user_must_update (as_user u.user_user)
      ) list
  
  | OpListReq list ->
      List.iter (fun nick ->
        let u = new_user (Some s) nick in
        (*u.user_type <- if u.user_myinfo.conn_speed = "" then Bot*)
        u.user_type <- Op;
          user_must_update (as_user u.user_user)
      ) list
  
  | QuitReq t ->
      remove_user s (search_user_by_name t);
  
  | RevConnectToMeReq t ->
      let orig = t.RevConnectToMe.orig in
      if !verbose_msg_clients then lprintf_nl "Received RevConnectToMe (%s)" orig;
      if not !!firewalled then begin (* we are in active mode, send $ConnectToMe *)
        (match !dc_tcp_listen_sock with 
        | Some lsock -> (* our listenin socket is active, so send ConnectToMe *)
            (try 
              let u = search_user_by_name orig in
              if (u.user_state = UserIdle) then begin
                u.user_state <- UserPassiveUserInitiating (current_time() );
                dc_send_msg sock (
                  let module C = ConnectToMe in
                  ConnectToMeReq { 
                    C.nick = orig;
                    C.ip = CommonOptions.client_ip (Some sock);
                    C.port = !!dc_port;
                  }
                )
              end (*else lprintf_nl "Reveived Revconnect: User not UserIdle. Cannot send $ConnectToMe %s - " u.user_nick*)
            with _ -> if !verbose_msg_clients then lprintf_nl "  No user by name: %s" orig )  
        | _ -> () ) 
      end else
        if !verbose_msg_clients then lprintf_nl "  We are in passive mode and client seems to be also"
          
  | SearchReq t ->
      (try
        let find () = 
          let amount = if t.Search.passive then 5 else 10 in
          (match t.Search.filetype with
          | 9 -> 
              let dcsh = Hashtbl.find dc_shared_files_by_hash t.Search.words_or_tth in
              [dcsh]
          | 1 ->
              let results = ref [] in
              let words =
                (match String2.split_simplify t.Search.words_or_tth ' ' with
                | [] -> raise Not_found
                | words -> words ) 
              in
              let all_match = ref true in
              let count = ref 0 in
              (try
                Hashtbl.iter (fun _ dcsh ->
                  all_match := true;
                  let rec search list =
                    (match list with
                    | [] -> ()
                    | word :: tail ->
                        if String2.contains dcsh.dc_shared_searchname word then search tail
                        else all_match := false )
                  in
                  search words;
                  if !all_match then begin
                    results := dcsh :: !results;
                    incr count;
                    if !count = amount then raise BreakIter (* lets stop the search if enough found already *)
                  end
                ) dc_shared_files_by_hash
              with BreakIter -> () );
              !results
          | _ ->
              [] )   
          (*if t.Search.filetype = 9 then begin
            (try
              let dcsh = Hashtbl.find dc_shared_files_by_hash t.Search.words_or_tth in
              let sh = CommonUploads.find_by_name dcsh.dc_shared_codedname in
              let index = Hashtbl.find CommonUploads.infos_by_name dcsh.dc_shared_fullname in
              let info = IndexedSharedFiles.get_result index in 
              [(sh,info)]                     
            with _ -> raise Not_found )
          end else begin     
            CommonUploads.query (
                      let q = 
                (match String2.split_simplify t.Search.words_or_tth ' ' with
                | [] -> raise Not_found
                | s :: tail -> List.fold_left (fun q s -> QAnd (q, (QHasWord s))) (QHasWord s) tail )
                      in
              (match t.Search.sizelimit with
                      | NoLimit -> q
              | AtMost n -> QAnd (QHasMaxVal (Field_Size, n),q)
              | AtLeast n -> QAnd (QHasMinVal (Field_Size, n),q) )
            )
          end *)
        in
        let send_sr_messages files =      (* function to send both active and passive messages *)
          (*lprintf_nl "Results %d" (List.length files);*)
          List.iter (fun dcsh -> 
            let codedname = String2.replace dcsh.dc_shared_codedname '/' (String2.of_char char92) in
            (*let length = String.length codedname in*)
            let directory,filename =
              (try 
                let pos = String.rindex codedname char92 in
                (String2.before codedname pos), (String2.after codedname (pos+1))
              with _ ->
                if !verbose_unknown_messages then lprintf_nl "Codedname was wrong in Search receiving";
                raise Not_found )
            in
            let message =                 (* message structure for both active and passive messages *)
              let module S = SR in {
                S.owner = s.server_last_nick;
                S.directory = directory;
                S.filename = filename;
                S.filesize = dcsh.dc_shared_size;
                S.open_slots = current_slots ();
                S.all_slots = open_slots ();
                S.tth = dcsh.dc_shared_tiger_root; 
                S.server_name = s.server_name;
                S.server_ip = Printf.sprintf "%s" (Ip.to_string (Ip.ip_of_addr s.server_addr));
                S.server_port = Printf.sprintf "%d" s.server_port;
                S.to_nick = if t.Search.passive then Some t.Search.nick else None;
                        }
            in
            if !verbose_msg_clients then begin
              let mode = if t.Search.passive then 'P' else 'A' in
              lprintf_nl "Sending $SR: (%s)(%c)(%s)(%s)(%s)" t.Search.nick mode directory filename 
                dcsh.dc_shared_tiger_root
        end;
            if t.Search.passive then 
              dc_send_msg sock (SRReq ( message ))
            else
              DcClients.udp_send (Ip.of_string t.Search.ip) (int_of_string t.Search.port) (SRReq ( message ))
          ) files
        in

        if t.Search.passive then begin                          (* if passive search received *)
          if not !!firewalled then begin                        (* and we are in active mode  *)
            if (t.Search.nick <> s.server_last_nick) then begin (* if search is not from ourself ... *)
              ignore (new_user (Some s) t.Search.nick);
              let files = find () in
              send_sr_messages files
            end
          end                                                   (* passive search to passive user, do nothing *)
        end else begin                                          (* active search received ... *)
          if (t.Search.ip <> Ip.to_string (CommonOptions.client_ip (Some sock))) ||
             (t.Search.port <> string_of_int(!!dc_port)) then begin (* check that ip & port is not ours *)
                let files = find () in
                send_sr_messages files
      end
        end
      with _ -> () (*lprintf_nl "Exception %s in SEARCH receiving" (Printexc2.to_string e)*) )

  | SRReq t -> (* download is resumed automatically, if file is found already to be in file list *)
      DcClients.received_new_search_result s t

  | SupportsReq t -> (* After EXTENDEDPROTOCOL support list from hub ... *)
      (match t with
      | HubSupports t -> s.server_supports <- Some t (* Save supports into serverdata *)
      | _ -> () )

  | ToReq t ->
      if !verbose_msg_clients then lprintf_nl "Received $To: from %s (%s)" t.To.from t.To.message;
      let u = new_user (Some s) t.To.from in
      u.user_messages <- u.user_messages @ [
        (int_of_float (current_time ()), t.To.from, PrivateMessage (0, t.To.message))];

  | UnknownReq m -> 
      if m <> "" then
        if !verbose_unexpected_messages || !verbose_msg_servers then
          let l = String.length m in
          let txt = Printf.sprintf "Unknown server message: (%s)" (shorten_string s.server_name 20)in
          if l > 50 then lprintf_nl "%s (%s...%d chars)" txt (shorten_string m 50) l
          else lprintf_nl "%s (%s)" txt m

  | UserCommandReq -> () (* Not supported atm *)

  | UserIPReq st -> (* CHECK *)
      if !verbose_msg_servers then lprintf_nl "Received $UserIP";
      let st = UserIP.parse_nameip st in
      List.iter begin fun (name,addr) ->
        lprintf_nl "UserIP: %s %s" name (Ip.string_of_addr addr); 
        try
          if name = s.server_last_nick then
          begin
            match addr with 
            | Ip.AddrIp ip -> lprintf_nl "Received self IP: %s" (Ip.to_string ip); last_high_id := ip
            | Ip.AddrName _ -> ()
          end;
          let u = search_user_by_name name in
          u.user_ip <- addr;
          lprintf_nl "Added ip %s to user %s" (Ip.string_of_addr u.user_ip) u.user_nick
        with _ ->
          if !verbose_unexpected_messages then lprintf_nl "No user by name %s" name
      end st

  | ValidateDenideReq n ->
      let errortxt = Printf.sprintf "Nick %s is already in use" n in
      if !verbose_unexpected_messages || !verbose_msg_servers then
        lprintf_nl "%s" errortxt;
      disconnect_server s (Closed_for_error errortxt )

  | VersionReq v -> ()

  | _ -> 
    lprintf_nl "--> Unhandled server message. Implement ?:";
    DcProtocol.dc_print m )

(* connect to DC server *)
let connect_server s =
  if can_open_connection connection_manager then
    (match s.server_sock with
    | NoConnection ->
        let token = add_pending_connection connection_manager (fun token ->
          (*s.server_sock <- NoConnection;*)
          (try
            connection_try s.server_connection_control;
            (*printf_char 's'; *)
            let sock = TcpBufferedSocket.connect token "directconnect to server" 
                       (Ip.to_inet_addr (Ip.ip_of_addr s.server_addr)) s.server_port (server_handler s) in
              set_server_state s Connecting;
              TcpBufferedSocket.set_read_controler sock download_control; (* CommonGlobals.download_control *)
              TcpBufferedSocket.set_write_controler sock upload_control;
              (*s.server_search_timeout <- last_time () + 30;*)
              TcpBufferedSocket.set_reader sock (dc_handler_server (client_to_server s));
              TcpBufferedSocket.set_rtimeout sock !!server_connection_timeout (*60.*);
              TcpBufferedSocket.set_handler sock (BASIC_EVENT RTIMEOUT) (fun s -> close s Closed_for_timeout);
                    s.server_sock <- Connection sock;
              s.server_ip <- Ip.ip_of_addr s.server_addr;
              s.server_connection_time <- current_time ();
                  with e -> 
            disconnect_server s (Closed_for_exception e) )
        ) in  
            s.server_sock <- ConnectionWaiting token
    | _ -> () )
  
let try_connect_server s =
  if connection_can_try s.server_connection_control && 
    s.server_sock = NoConnection then
    connect_server s
    
(* Make hublist from file f, return hublist *)
let make_hublist_from_file f =
  let s = File.to_string f in
  let hublist = ref [] in
  let counter = ref 0 in
  let lines = String2.split s '\n' in
  List.iter (fun l ->
    (match String2.split l '|' with
    | server_name :: server_addr :: server_info :: server_nusers :: _ ->
        let ap = String2.split server_addr ':' in
        let port = ref 411 in
        let addr = ref "" in
        let ll = List.length ap in
        if ll > 0 then begin
          addr := List.hd ap;
          if ll = 2 then begin
            (try
              port := int_of_string (List.nth ap 1)
            with e -> () )
          end;
          if (ll < 3) && ((String.length !addr) > 2) && (!port > 0) && (!port < 65536) then begin
            let nusers = ref 0 in begin
              try
                nusers := int_of_string server_nusers 
              with _ -> ()
            end;
            incr counter;
            let r = {
              dc_name = Charset.Locale.to_utf8 server_name;
              dc_ip = Ip.addr_of_string !addr;
              dc_port = !port;
              dc_info = Charset.Locale.to_utf8 server_info;
              dc_nusers = !nusers;
            } in
            hublist := r :: !hublist
          end
        end
    | _ -> () )
  ) lines;
  if !verbose_msg_servers then lprintf_nl "Found %d valid servers from hublist" !counter;
  !hublist

let xml_tag name = let name = String.lowercase name in fun x -> String.lowercase (Xml.tag x) = name

let rec xml_select names xs =
  match names with
  | [] -> xs
  | [name] -> List.filter (xml_tag name) xs
  | name::t ->
    let l = List.filter (xml_tag name) xs in
    xml_select t (List.concat (List.map Xml.children l))

let ssplit s sub =
  try
    let n = String2.search_from s 0 sub in
    Some (String2.before s n, String2.after s (n + String.length sub))
  with
    _ -> None

exception ADC_not_supported

let parse_address s =
  let s = match ssplit (String.lowercase s) "://" with
  | Some ("dchub",s) -> s
  | None -> s
  | Some (("adc"|"adcs"),_) -> raise ADC_not_supported
  | Some (proto,_) -> failwith (Printf.sprintf "Unsupported protocol in %S" s)
  in
  try Scanf.sscanf s "%s@:%u" (fun s n -> s,n) with _ -> s,411

let make_hublist_from_xml x =
  let make_hub x =
    let name = Charset.Locale.to_utf8 (Xml.attrib x "Name") in
    let (address,port) = parse_address (Xml.attrib x "Address") in
    let info = Charset.Locale.to_utf8 (Xml.attrib x "Description") in
    let nusers = int_of_string (Xml.attrib x "Users") in
    {
      dc_name = name;
      dc_ip = Ip.addr_of_string address;
      dc_port = port;
      dc_info = info;
      dc_nusers = nusers;
    }
  in
  let l = xml_select ["hublist";"hubs";"hub"] [x] in
  let adc = ref 0 in
  let add_hub acc x =
    try 
      make_hub x :: acc
    with 
    | ADC_not_supported -> incr adc; acc
    | exn -> lprintf_nl "Skipping hublist entry : %s" (Printexc2.to_string exn); acc
  in
  let l' = List.fold_left add_hub [] l in
  if !verbose_msg_servers then
    lprintf_nl "Servers in hublist : %u total, %u valid, %u adc" (List.length l) (List.length l') !adc;
  l'

(* Connect to all autoconnect servers once *)
let autoconnect_to_servers () =
  Hashtbl.iter (fun _ s ->
    if s.server_autoconnect then begin                    (* only if server is marked as autoconnect *) 
      if not (List.memq s !connected_servers) then begin  (* and not already connected               *)
        if s.server_sock = NoConnection then begin        (* and not in connection state             *)
          try_connect_server s;
        end
      end
    end  
 ) servers_by_ip;

module P = GuiTypes
  
(* register server operations *)
let _ =
  server_ops.op_server_connect <- (fun s -> connect_server s);
  server_ops.op_server_disconnect <- (fun s -> disconnect_server s Closed_by_user);
  server_ops.op_server_query_users <- (fun s ->
    do_if_connected s.server_sock (fun sock ->
      dc_send_msg sock (GetNickListReq)
    )
  );
  server_ops.op_server_users <- (fun s ->
    let list = ref [] in
    List.iter (fun u ->
       list := (as_user u.user_user) :: !list
    ) s.server_users;
    !list
  );
  server_ops.op_server_remove <- (fun s ->
    disconnect_server s Closed_by_user;
    server_remove s
  );
  server_ops.op_server_info <- (fun s ->
        { (impl_server_info s.server_server) with
          P.server_num = (server_num s);
          P.server_network = network.network_num;
          P.server_addr = s.server_addr;
          P.server_port = s.server_port;
          P.server_realport = 0;
          P.server_score = 0;
          P.server_tags = [];
          P.server_nusers = Int64.of_int (List.length s.server_users);
          P.server_state = server_state s;
          P.server_name = s.server_name;
          P.server_description = s.server_info;
          P.server_preferred = false;
        }
  );
  server_ops.op_server_set_preferred <- (fun s preferred ->
    s.server_autoconnect <- preferred;
    if !verbose_msg_servers then lprintf_nl "Server autoconnection state set to (%s)" (if preferred then "true" else "false")
)

(*
    mutable op_server_network : network;
    mutable op_server_find_user : ('a -> string -> unit);
    mutable op_server_cid : ('a -> Ip.t);
    mutable op_server_low_id : ('a -> bool);
    mutable op_server_rename : ('a -> string -> unit);
*)

  
  
