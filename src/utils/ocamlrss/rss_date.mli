(* This code from ocamlnet, by Gerd Stolpman, Patrick Doane and
   Nicolas George.

Copyright (c) 2001 Patrick Doane and Gerd Stolpmann

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must
not claim that you wrote the original software. If you use this
software in a product, an acknowledgment in the product documentation
would be appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must
not be misrepresented as being the original software.

3. This notice may not be removed or altered from any source
distribution.

*)

(* Note by Maxence Guesdon: I changed the code of format* not to use
   Netchannels.
*)

(** Support for common date/time parsing and formatting.
   Many routines refer to the epoch, which for Unix is 
   00:00:00 UTC, January 1, 1970.
*)

type t = {
  year : int;		(** complete year *)
  month : int;		(** 1..12 *)
  day : int;		(** 1..31 *)
  hour : int;
  minute : int;
  second : int;
  zone : int;		(** in minutes; 60 = UTC+0100 *)
  week_day : int	(** 0 = sunday; -1 if not given *)
}


val localzone : int
  (* The offset in minutes for the local time zone from the UTC *)

val create : ?zone:int -> float -> t
  (** Convert the time (seconds since the epoch) to a date/time record *)

val parse : string -> t
  (** Parse a string and return a date/time record *)

val since_epoch : t -> float
  (** Convert a date/time record into the time (seconds since the epoch) *)

val parse_epoch : string -> float
  (** Parse a string and return the time (seconds since the epoch *)


val format_to : Buffer.t -> fmt:string -> t -> unit
  (* Format a date/time record according to the format string and outputs
   * the resulting string to the channel.
   *
   * The format string consists of zero or more conversion specifications
   * and ordinary characters.  All ordinary characters are output directly
   * to the channel.  A conversion specification consists of the '%'
   * character and one other character.
   *
   * The conversion specifications are:
   *
   *  %A    full weekday name.
   *
   *  %a    abbreviated weekday name.
   *
   *  %B    full month name.
   *
   *  %b    abbreviated month name.
   *
   *  %C    (year / 100) as an integer;
   *        single digits are preceded by a zero.
   *
   *  %c    equivalent to "%a %b %e %T %Y".
   *
   *  %D    equivalent to "%m/%d/%y".
   *
   *  %d    day of the month as an integer (01-31);
   *        single digits are preceded by a zero.
   *
   *  %e    day of the month as an integer (1-31).
   *
   *  %H    hour (24-hour clock) as an integer (00-23).
   *
   *  %h    the same as %b.
   *
   *  %I    hour (12-hour clock) as an integer (01-12).
   *
   *  %j    day of the year as an integer (001-366).
   *
   *  %k    hour (24-hour clock) as an integer (0-23);
   *        single digits are preceded by a blank.
   *
   *  %l    hour (12-hour clock) as an integer (1-12);
   *        single digits are preceded by a blank.
   *
   *  %M    minute as an integer (00-59).
   *
   *  %m    month as an integer (01-12).
   *
   *  %n    a newline.
   *
   *  %p    either "AM" or "PM" as appropriate.
   *
   *  %P    either "am" or "pm" as appropriate.
   *
   *  %R    equivalent to "%H:%M".
   *
   *  %r    equivalent to "%I:%M:%S %p".
   *
   *  %S    second as an integer (00-60).
   *
   *  %T    equivalent to "%H:%M:%S".
   *
   *  %t    a tab.
   *
   *  %U    week number of the year (Sunday as the first day
   *        of the week) as an integer (00-53).
   *
   *  %u    weekday (Monday as the first day of the week) as
   *        an integer (1-7).
   *
   *  %w    weekday (Sunday as the first day of the week) as
   *        an integer (0-6).
   *
   *  %X    representation of the time.
   *
   *  %x    representation of the date.
   *
   *  %Y    year with century as an integer.
   *
   *  %y    year without century as an integer (00-99).
   *
   *  %z    time zone offset from UTC; a leading plus sign
   *        stands for east of UTC, a minus sign for west of UTC, hours and
   *        minutes follow with two digits each and no delimiter between them
   *        (common form for RFC 822 date headers).
   *
   *  %%    a `%' character.
   * 
   *)

val format : fmt:string -> t -> string
  (* Format a date/time record as a string *)

val mk_mail_date : ?zone:int -> float -> string
  (* Convert the time (seconds since the epoch) to a date string that
   * conforms to RFC 1123 (which updates RFC 822).
   *
   * Example: "Sun, 06 Nov 1994 08:49:37 -0500".
   *)

val mk_usenet_date : ?zone:int -> float -> string
  (* Convert the time (seconds since the epoch) to a date string that
   * conforms to RFC 1036 (which obsoletes RFC 850).
   *
   * Example: "Sunday, 06-Nov-94 08:49:37 -0500".
   *
   * Note that this format has only two digits for the year.
   *)

