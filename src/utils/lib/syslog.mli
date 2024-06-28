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

(** Syslog routines *)

(** These are loosely based on the unix syslog(3) function and
    relatives. *)

(** The assorted logging facilities. The default is [`LOG_USER]. You
    can set a new default with openlog, or give a specific facility per
    syslog call. *)
type facility =
  [ `LOG_KERN | `LOG_USER | `LOG_MAIL | `LOG_DAEMON | `LOG_AUTH
  | `LOG_SYSLOG | `LOG_LPR | `LOG_NEWS | `LOG_UUCP | `LOG_CRON
  | `LOG_AUTHPRIV | `LOG_FTP | `LOG_NTP | `LOG_SECURITY
  | `LOG_CONSOLE | `LOG_LOCAL0 | `LOG_LOCAL1 | `LOG_LOCAL2
  | `LOG_LOCAL3 | `LOG_LOCAL4 | `LOG_LOCAL5 | `LOG_LOCAL6
  | `LOG_LOCAL7 ]

(** Flags to pass to openlog. LOG_NDELAY is mandatory and implied *)
type flag = [ `LOG_CONS | `LOG_PERROR | `LOG_PID ]

(** The priority of the error. *)
type level = [ `LOG_EMERG | `LOG_ALERT | `LOG_CRIT | `LOG_ERR | `LOG_WARNING
	     | `LOG_NOTICE | `LOG_INFO | `LOG_DEBUG ]

(** the type of a syslog connection *)
type t

(** given a string descibing a facility, return the facility. The
    strings consist of the name of the facility with the LOG_ chopped
    off. They are not case sensitive.
    @raise Syslog_error when given an invalid facility *)
val facility_of_string: string -> facility

(** [openlog ?(logpath=AUTODETECTED) ?(facility=`LOG_USER) ?(flags=[]) tag]
    Similar to openlog(3).
    You MUST define [tag] as 32 ABNF alphanumeric characters maximum.
    @raise Syslog_error on error *)
val openlog: ?logpath:string -> ?facility:facility -> ?flags:flag list -> string -> t

(** Same as syslog(3), except there's no formats.
    @raise Syslog_error on error (very rare) *)
val syslog: ?fac:facility -> t -> level -> string -> unit

(** Close the log.
    @raise Syslog_error on error *)
val closelog: t -> unit