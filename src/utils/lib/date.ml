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

type date_format =
  Second
| Minute
| Hour
| Day
| WeekDay
| Month
| MonthNumber
| Year
| Comma
| Space
| Colon
| Dot
| Minus
| Zone
| Gmt

let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let month = ref (fun n -> months.(n))
let day = ref (fun n -> days.(n))
  
let string_of_date formats tm date =
  List.fold_left (fun s format ->
      match format with
        Second -> Printf.sprintf "%s%02d" s tm.Unix.tm_sec
      | Minute -> Printf.sprintf "%s%02d" s tm.Unix.tm_min
      | Hour -> Printf.sprintf "%s%02d" s tm.Unix.tm_hour
      | Day -> Printf.sprintf "%s%02d" s tm.Unix.tm_mday
      | WeekDay  -> Printf.sprintf "%s%s" s (!day tm.Unix.tm_wday)
      | Month -> Printf.sprintf "%s%s" s (!month tm.Unix.tm_mon)
      | MonthNumber -> Printf.sprintf "%s%02d" s (tm.Unix.tm_mon+1)
      | Year -> Printf.sprintf "%s%04d" s (1900+tm.Unix.tm_year)
      | Comma -> s ^ ","
      | Space -> s ^ " "
      | Colon -> s ^ ":"
      | Dot -> s ^ "."
      | Minus -> s ^ "-"
      | Zone -> Printf.sprintf "%s%s" s (Rss_date.mk_timezone date)
      | Gmt -> s ^ "GMT"
  ) "" formats

  
let to_string date =
  string_of_date [Hour;Colon;Minute;Space; Space; WeekDay; Space; Day; Space;Month;]
    (Unix.localtime date) date
    
let to_full_string date =
  string_of_date [Hour;Colon;Minute;Space; Space; WeekDay; Space; Day; Space;Month; Space;Year]
    (Unix.localtime date) date

let simple date = 
  string_of_date [Hour;Colon;Minute;Colon;Second;Space; Space; WeekDay]
    (Unix.localtime date) date

let reverse date = 
  string_of_date [Year;MonthNumber;Day;Minus;Hour;Minute;Second]
    (Unix.localtime date) date
  
let mail_string date =
    string_of_date [WeekDay;Comma;Space;Day;Space;Month;Space;Year;Space;Hour;Colon;Minute;Colon;Second;Space;Zone]
      (Unix.localtime date) date

let apache_string date =
    string_of_date [WeekDay;Comma;Space;Day;Space;Month;Space;Year;Space;Hour;Colon;Minute;Colon;Second;Space;Gmt]
      (Unix.localtime date) date

let time_of_string date =
  let month =
    match String.sub date 8 3 with
      "Jan" -> 0
    | "Feb" -> 1
    | "Mar" -> 2
    | "Apr" -> 3
    | "May" -> 4
    | "Jun" -> 5
    | "Jul" -> 6
    | "Aug" -> 7
    | "Sep" -> 8
    | "Oct" -> 9
    | "Nov" -> 10
    | "Dec" -> 11
    | _ -> 0
  in
  begin
    try
      let time = fst(Unix.mktime{ 
      Unix.tm_isdst = false;
      Unix.tm_wday = 0;
      Unix.tm_yday = 0;
      Unix.tm_sec = int_of_string (String.sub date 23 2);
      Unix.tm_min = int_of_string (String.sub date 20 2);
      Unix.tm_hour = int_of_string (String.sub date 17 2);
      Unix.tm_mday = int_of_string (String.sub date 5 2);
      Unix.tm_mon = month;
      Unix.tm_year = int_of_string (String.sub date 12 4) - 1900}) in time
    with e -> failwith (Printf.sprintf "date error %s" (Printexc2.to_string e))
  end

let minute_in_secs = 60
let hour_in_minutes = 60
let day_in_hours = 24
let half_hour_in_secs = 30 * minute_in_secs
let hour_in_secs = 60 * minute_in_secs
let half_day_in_secs = (day_in_hours / 2) * hour_in_secs
let day_in_secs = day_in_hours * hour_in_secs
let day_in_minutes = day_in_hours * hour_in_minutes
let year_in_secs = 365 * day_in_secs

let time_to_string time print_format =
  let days = time / 60 / 60 / 24 in
  let rest = time - days * 60 * 60 * 24 in
  let hours = rest / 60 / 60 in
  let rest = rest - hours * 60 * 60 in
  let minutes = rest / 60 in
  let seconds = rest - minutes * 60 in
  match print_format with
    "long" ->

  if days > 0
    then Printf.sprintf " %dd " days
  else if hours > 0
    then Printf.sprintf " %d:%02d:%02d " hours minutes seconds
    else Printf.sprintf " %d:%02d " minutes seconds

  | "verbose" ->
        Printf.sprintf "%s%s%dm %ds"
          (if days > 0 then (string_of_int days) ^ "d " else "")
          (if hours > 0 then (string_of_int hours) ^ "h " else "")
          minutes seconds
  | _ -> ""
