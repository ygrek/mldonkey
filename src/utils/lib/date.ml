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
| Zone
  
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let month = ref (fun n -> months.(n))
let day = ref (fun n -> days.(n))
  
let string_of_date formats tm =
  List.fold_left (fun s format ->
      match format with
        Second -> Printf.sprintf "%s%02d" s tm.Unix.tm_sec
      | Minute -> Printf.sprintf "%s%02d" s tm.Unix.tm_min
      | Hour -> Printf.sprintf "%s%02d" s tm.Unix.tm_hour
      | Day -> Printf.sprintf "%s%02d" s tm.Unix.tm_mday
      | WeekDay  -> Printf.sprintf "%s%s" s (!day tm.Unix.tm_wday)
      | Month -> Printf.sprintf "%s%s" s (!month tm.Unix.tm_mon)
      | MonthNumber -> Printf.sprintf "%s%d" s (tm.Unix.tm_mon)
      | Year -> Printf.sprintf "%s%04d" s (1900+tm.Unix.tm_year)
      | Comma -> s ^ ","
      | Space -> s ^ " "
      | Colon -> s ^ ":"
      | Dot -> s ^ "."
      | Zone -> s ^ "-0000" (* BUG: RFC 2822: Though "-0000" also indicates Universal Time, it is
         used to indicate that the time was generated on a system that may be
	    in a local time zone other than Universal Time and therefore
	       indicates that the date-time contains no information about the local
	          time zone. *)
  ) "" formats

  
let to_string date =
  string_of_date [Hour;Colon;Minute;Space; Space; WeekDay; Space; Day; Space;Month;]
    (Unix.localtime date)
    
let to_full_string date =
  string_of_date [Hour;Colon;Minute;Space; Space; WeekDay; Space; Day; Space;Month; Space;Year]
    (Unix.localtime date)

let simple date = 
  string_of_date [Hour;Colon;Minute;Colon;Second;Space; Space; WeekDay]
    (Unix.localtime date)
  
let mail_string date =
    string_of_date [WeekDay;Comma;Space;Day;Space;Month;Space;Year;Space;Hour;Colon;Minute;Colon;Second;Space;Zone]
      (Unix.localtime date)

let hour_in_secs = 3600
let day_in_secs = 24 * hour_in_secs
let year_in_secs = 365 * day_in_secs
