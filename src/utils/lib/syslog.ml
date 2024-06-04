(* syslog(3) routines for ocaml (RFC 3164)

   This library is based on Shawn Wagner's original syslog
   library as included in annexlib, with significant modifications
   by by Eric Stokes <eric.stokes@csun.edu>.

   Copyright (C) 2002 Shawn Wagner <raevnos@pennmush.org>
   Copyright (C) 2005 Eric Stokes <eric.stokes@csun.edu>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Unix

type facility =
  [ `LOG_KERN | `LOG_USER | `LOG_MAIL | `LOG_DAEMON | `LOG_AUTH
  | `LOG_SYSLOG | `LOG_LPR | `LOG_NEWS | `LOG_UUCP | `LOG_CRON
  | `LOG_AUTHPRIV | `LOG_FTP | `LOG_NTP | `LOG_SECURITY
  | `LOG_CONSOLE | `LOG_LOCAL0 | `LOG_LOCAL1 | `LOG_LOCAL2
  | `LOG_LOCAL3 | `LOG_LOCAL4 | `LOG_LOCAL5 | `LOG_LOCAL6
  | `LOG_LOCAL7 ]

type flag = [ `LOG_CONS | `LOG_PERROR | `LOG_PID ]

type level = [ `LOG_EMERG | `LOG_ALERT | `LOG_CRIT | `LOG_ERR | `LOG_WARNING
	     | `LOG_NOTICE | `LOG_INFO | `LOG_DEBUG ]

exception Syslog_error of string

let facility_of_string s =
  match String.lowercase_ascii s with
  | "kern" -> `LOG_KERN
  | "user" -> `LOG_USER
  | "mail" -> `LOG_MAIL
  | "daemon" -> `LOG_DAEMON
  | "auth" -> `LOG_AUTH
  | "syslog" -> `LOG_SYSLOG
  | "lpr" -> `LOG_LPR
  | "news" -> `LOG_NEWS
  | "uucp" -> `LOG_UUCP
  | "cron" -> `LOG_CRON
  | "authpriv" -> `LOG_AUTHPRIV
  | "ftp" -> `LOG_FTP
  | "ntp" -> `LOG_NTP
  | "security" -> `LOG_SECURITY
  | "console" -> `LOG_CONSOLE
  | "local0" -> `LOG_LOCAL0
  | "local1" -> `LOG_LOCAL1
  | "local2" -> `LOG_LOCAL2
  | "local3" -> `LOG_LOCAL3
  | "local4" -> `LOG_LOCAL4
  | "local5" -> `LOG_LOCAL5
  | "local6" -> `LOG_LOCAL6
  | "local7" -> `LOG_LOCAL7
  | x -> raise (Syslog_error ("facility_of_string: invalid facility, " ^ x))

let facility_to_num fac =
  Int32.of_int @@ match fac with
  | `LOG_KERN -> 0 lsl 3
  | `LOG_USER -> 1 lsl 3
  | `LOG_MAIL -> 2 lsl 3
  | `LOG_DAEMON -> 3 lsl 3
  | `LOG_AUTH -> 4 lsl 3
  | `LOG_SYSLOG -> 5 lsl 3
  | `LOG_LPR -> 6 lsl 3
  | `LOG_NEWS -> 7 lsl 3
  | `LOG_UUCP -> 8 lsl 3
  | `LOG_CRON -> 9 lsl 3
  | `LOG_AUTHPRIV -> 10 lsl 3
  | `LOG_FTP -> 11 lsl 3
  | `LOG_NTP -> 12 lsl 3
  | `LOG_SECURITY -> 13 lsl 3
  | `LOG_CONSOLE -> 14 lsl 3
  | `LOG_LOCAL0 -> 16 lsl 3
  | `LOG_LOCAL1 -> 17 lsl 3
  | `LOG_LOCAL2 -> 18 lsl 3
  | `LOG_LOCAL3 -> 19 lsl 3
  | `LOG_LOCAL4 -> 20 lsl 3
  | `LOG_LOCAL5 -> 21 lsl 3
  | `LOG_LOCAL6 -> 22 lsl 3
  | `LOG_LOCAL7 -> 23 lsl 3

let level_to_num lev =
  Int32.of_int @@ match lev with
  | `LOG_EMERG -> 0
  | `LOG_ALERT -> 1
  | `LOG_CRIT -> 2
  | `LOG_ERR -> 3
  | `LOG_WARNING -> 4
  | `LOG_NOTICE -> 5
  | `LOG_INFO -> 6
  | `LOG_DEBUG -> 7

type t = {
  mutable fd: Unix.file_descr;
  mutable connected: bool;
  mutable flags: flag list;
  mutable tag: string;
  mutable fac: int32;
  mutable logpath: string;
}

let open_connection loginfo =
  match loginfo.logpath with
  | "" -> raise (Syslog_error "unable to find the syslog socket or pipe, is syslogd running?")
  | logpath ->
    match (Unix.stat logpath).Unix.st_kind with
    | Unix.S_SOCK ->
      let logaddr = Unix.ADDR_UNIX logpath in
      loginfo.fd <-
        begin
          try Unix.socket Unix.PF_UNIX SOCK_DGRAM 0
          with Unix.Unix_error (Unix.EPROTOTYPE, _, _) ->
            Unix.socket Unix.PF_UNIX SOCK_STREAM 0
        end ;
      Unix.connect loginfo.fd logaddr ;
      loginfo.connected <- true;
    | Unix.S_FIFO ->
      loginfo.fd <- Unix.openfile logpath [Unix.O_WRONLY] 0o666;
      loginfo.connected <- true;
    | _ -> raise (Syslog_error "invalid log path, not a socket or pipe")

let openlog
    ?(logpath=
      if Sys.file_exists "/dev/log" then "/dev/log"
      else if Sys.file_exists "/var/run/syslog" then "/var/run/syslog"
      else "")
    ?(facility=`LOG_USER)
    ?(flags=[])
    ident =
  let loginfo = {fd = Unix.stderr;
		 connected = false;
		 flags = flags;
		 tag = (if String.length ident > 32
                        then String.sub ident 0 32
                        else ident);
		 fac = facility_to_num facility;
		 logpath = logpath}
  in
  open_connection loginfo;
  loginfo

let log_fd fd msg =
  try
    ignore (Unix.write fd msg 0 (Bytes.length msg));
    ignore (Unix.write fd (Bytes.unsafe_of_string "\n") 0 1)
  with _ -> ()

let ascdate {tm_sec=sec;tm_min=min;tm_hour=hour;
	     tm_mday=mday;tm_mon=mon;_} =
  let asc_mon =
    match mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> raise (Syslog_error "invalid month")
  in
  (Printf.sprintf "%s %2d %02d:%02d:%02d" asc_mon mday hour min sec)

let protected_write loginfo str =
  let fallback _ =
    (try close loginfo.fd with _ -> ());
    loginfo.connected <- false;
    (try open_connection loginfo with _ -> ());
    if List.mem `LOG_CONS loginfo.flags then log_fd Unix.stdout str
  in
  let prev = Sys.signal Sys.sigpipe (Sys.Signal_handle fallback) in
  try
    ignore (write loginfo.fd str 0 (Bytes.length str));
    Sys.set_signal Sys.sigpipe prev
  with Unix_error (_, _, _) ->
    (* on error, attempt to reconnect *)
    fallback ();
    Sys.set_signal Sys.sigpipe prev

let syslog ?fac loginfo lev str =
  let msg = Buffer.create 64 in
  let realfac = match fac with
    | Some f -> facility_to_num f
    | None -> loginfo.fac in
  let levfac = Int32.logor realfac (level_to_num lev)
  and now = ascdate (localtime (Unix.time ())) in
  Printf.bprintf msg "<%ld>%s " levfac now;
  Buffer.add_string msg loginfo.tag ;
  if List.mem `LOG_PID loginfo.flags then
    Printf.bprintf msg "[%d]" (Unix.getpid());
  if String.length loginfo.tag > 0 then
    Buffer.add_string msg ": ";
  Buffer.add_string msg str;
  let msg =
    if Buffer.length msg > 1024
    then begin
      let m = Bytes.unsafe_of_string @@ Buffer.sub msg 0 1024 in
      Bytes.blit_string "..." 0 m 1021 3 ;
      m
    end else Buffer.to_bytes msg
  in
  protected_write loginfo msg;
  if List.mem `LOG_PERROR loginfo.flags then log_fd Unix.stderr msg

let closelog loginfo =
  if loginfo.connected then
    begin
      Unix.close loginfo.fd;
      loginfo.connected <- false
    end;
  loginfo.flags <- [];
  loginfo.tag <- "";
  loginfo.fac <- facility_to_num `LOG_USER