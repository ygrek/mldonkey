(* Copyright 2005 b8_bavard, INRIA *)
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

(* the graph base of the GUI. *)

open Options
open CommonTypes
open GraphTypes

module O = GuiOptions

let verbose = O.gtk_verbose_graphbase

let lprintf' fmt =
  Printf2.lprintf ("GuiGraphBase: " ^^ fmt)

let last_time = BasicSocket.current_time ()

let dummy_graph () =
  {
   quarter = [last_time, 0.];
   hour    = [last_time, 0.];
   halfday = [last_time, 0.];
   day     = [last_time, 0.];
   week    = [last_time, 0.];
   month   = [last_time, 0.];
   year    = [last_time, 0.];
  }

let quarter = 15. *. 60.
let hour = 4. *. quarter
let halfday = 12. *. hour
let day = 2. *. halfday
let week = 7. *. day
let month = 4. *. week
let year = 52. *. week

(*************************************************************************)
(*                                                                       *)
(*                         GraphOption                                   *)
(*                                                                       *)
(*************************************************************************)

module GraphOption = struct

    let value_to_floats =
      value_to_tuple2 (fun (v1, v2) ->
        let f1 = value_to_float v1 in
        let f2 = value_to_float v2 in
        (f1, f2)
      )

    let floats_to_value =
      tuple2_to_value (fun (f1, f2) ->
        let v1 = float_to_value f1 in
        let v2 = float_to_value f2 in
        (v1, v2)
      )

    let check_graph_time l time_ref =
      let last_graph_time, _ = List.hd (List.rev l) in
      if last_time -. last_graph_time > time_ref
        then [last_time, 0.] else l

    let value_to_graph v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let graph_quarter =
            try
              let l = get_value "graph_quarter" (value_to_list value_to_floats) in
              check_graph_time l quarter
            with _ -> [last_time, 0.]
          in
          let graph_hour =
            try
              let l = get_value "graph_hour" (value_to_list value_to_floats) in
              check_graph_time l hour
            with _ -> [last_time, 0.]
          in
          let graph_halfday =
            try
              let l = get_value "graph_halfday" (value_to_list value_to_floats) in
              check_graph_time l halfday
            with _ -> [last_time, 0.]
          in
          let graph_day =
            try
              let l = get_value "graph_day" (value_to_list value_to_floats) in
              check_graph_time l day
            with _ -> [last_time, 0.]
          in
          let graph_week =
            try
              let l = get_value "graph_week" (value_to_list value_to_floats) in
              check_graph_time l week
            with _ -> [last_time, 0.]
          in
          let graph_month =
            try
              let l = get_value "graph_month" (value_to_list value_to_floats) in
              check_graph_time l month
            with _ -> [last_time, 0.]
          in
          let graph_year =
            try
              let l = get_value "graph_year" (value_to_list value_to_floats) in
              check_graph_time l year
            with _ -> [last_time, 0.]
          in
          {
             quarter = graph_quarter;
             hour    = graph_hour;
             halfday = graph_halfday;
             day     = graph_day;
             week    = graph_week;
             month   = graph_month;
             year    = graph_year;
          }

      | _ -> assert false

    let graph_to_value graph =
      Options.Module (
        ("graph_quarter", list_to_value floats_to_value graph.quarter) ::
        ("graph_hour",    list_to_value floats_to_value graph.hour)    ::
        ("graph_halfday", list_to_value floats_to_value graph.halfday) ::
        ("graph_day",     list_to_value floats_to_value graph.day)     ::
        ("graph_week",    list_to_value floats_to_value graph.week)    ::
        ("graph_month",   list_to_value floats_to_value graph.month)   ::
        ("graph_year",    list_to_value floats_to_value graph.year)    ::
        []
      )

    let t = define_option_class "Graph" value_to_graph graph_to_value

end


let uid_option  = define_option_class "TypeUid"
    (fun v -> uid_of_string (value_to_string v))
    (fun uid -> string_to_value (string_of_uid uid))

let graph_ini = create_options_file
    (Filename.concat GuiMessages.gui_config_dir "mlgui_graph.ini")

let graph_section = file_section graph_ini [] ""

let global_graph =
  define_option graph_section ["global_graph"] "gobal records"
    (tuple2_option (GraphOption.t, GraphOption.t))
    (dummy_graph (), dummy_graph ())

let files_graph =
  define_option graph_section ["files_graph"] "files records"
    (list_option (tuple2_option (uid_option, tuple2_option (GraphOption.t, GraphOption.t))))
    []

let _ =
  (try Options.load graph_ini with _ -> ());
  (try Options.save graph_ini with _ -> ())

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

let (global_queue : (graph_record, (float * float) list ref) Hashtbl.t) = Hashtbl.create 63

let _ =
  Hashtbl.add  global_queue GraphDownloads (ref [List.hd (List.rev (fst !!global_graph).quarter)]);
  Hashtbl.add  global_queue GraphUploads (ref [List.hd (List.rev (snd !!global_graph).quarter)]);
  List.iter (fun (file_uid, (down, up)) ->
    Hashtbl.add  global_queue (GraphFile (file_uid, GraphDownloads)) (ref [List.hd (List.rev down.quarter)]);
    Hashtbl.add  global_queue (GraphFile (file_uid, GraphUploads)) (ref [List.hd (List.rev up.quarter)]);
  ) !!files_graph

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

let max_points = 200.

let quarter_step = quarter /. max_points
let hour_step = hour /. max_points
let halfday_step = halfday /. max_points
let day_step = day /. max_points
let week_step = week /. max_points
let month_step = month /. max_points
let year_step = year /. max_points

(*************************************************************************)
(*                                                                       *)
(*                         r_avg                                         *)
(*                                                                       *)
(*************************************************************************)

let r_avg l0 l1 =
  let rx = ref 0. in
  let min_t = ref (fst (List.hd l0)) in
  let max_t = ref (fst (List.hd l1)) in
  List.iter2 (fun (t0, r0) (t1, r1) ->
    rx := !rx +. (r0 +. r1) *. (t1 -. t0) /. 2.;
    min_t := min !min_t t0;
    max_t := max !max_t t1;
  ) l0 l1;
  if (!max_t -. !min_t) > 0.
    then !rx /. (!max_t -. !min_t)
    else 0.

(*************************************************************************)
(*                                                                       *)
(*                         vect_from                                     *)
(*                                                                       *)
(*************************************************************************)

let vect_from previous current =
  if !!verbose then
    begin
      lprintf' "In vect_from : current length %d\n" (List.length current);
      List.iter (fun (a, b) ->
        lprintf' "     * [%f, %f]\n" a b
      ) current;
      lprintf' "In vect_from : previous length %d\n" (List.length previous);
      List.iter (fun (a, b) ->
        lprintf' "     * [%f, %f]\n" a b
      ) previous
    end;
  let (t_current, r_current) = List.nth current (List.length current - 1) in
  (if !!verbose then lprintf' "In vect_from : (t_current, r_current) = (%f, %f)\n" t_current r_current);
  let l = ref [] in
  let store = ref true in
  let n = ref (List.length previous - 1) in
  let (t, _) = List.nth previous !n in
  (if !!verbose then lprintf' "In vect_from : t = %f\n" t);
  while !store && (!n >= 0) do
    let (t_n, r_n) = List.nth previous !n in
    if t_n > t_current
      then begin
        l := (t_n, r_n) :: !l;
        decr n
      end else store := false
  done;
  l := (t_current, r_current) :: !l;
  let l2 = List.tl !l in
  let l1 = List.rev (List.tl (List.rev !l)) in
  if !!verbose
    then begin
      lprintf' "In vect_from : l1 length %d\n" (List.length l1);
      List.iter (fun (a, b) ->
        lprintf' "     * [%f, %f]\n" a b
      ) l1;
      lprintf' "In vect_from : l2 length %d\n" (List.length l2);
      List.iter (fun (a, b) ->
        lprintf' "     * [%f, %f]\n" a b
      ) l2
    end;
  let r_avg = r_avg l1 l2 in
  (if !!verbose then lprintf' "In vect_from : r_avg %f\n" r_avg);
  (t, r_avg)


(*************************************************************************)
(*                                                                       *)
(*                         record_year                                   *)
(*                                                                       *)
(*************************************************************************)

let record_year graph =
  (if !!verbose then lprintf' "calculating record_year\n");
  let vect = vect_from graph.month graph.year in
  (if List.length graph.year < int_of_float max_points
     then graph.year <- graph.year @ [vect]
     else graph.year <- (List.tl graph.year) @ [vect]);
  if !!verbose then lprintf' "record_year calculated successfully\n"

(*************************************************************************)
(*                                                                       *)
(*                         record_month                                  *)
(*                                                                       *)
(*************************************************************************)

let record_month graph =
  (if !!verbose then lprintf' "calculating record_month\n");
  let (t, r) = vect_from graph.week graph.month in
  (if List.length graph.month < int_of_float max_points
     then graph.month <- graph.month @ [t, r]
     else graph.month <- (List.tl graph.month) @ [t, r]);
  (if !!verbose then lprintf' "record_month calculated successfully\n");
  let (t_h, _) = List.nth graph.year (List.length graph.year - 1) in
  if (t -. t_h) > year_step
    then record_year graph

(*************************************************************************)
(*                                                                       *)
(*                         record_week                                   *)
(*                                                                       *)
(*************************************************************************)

let record_week graph =
  (if !!verbose then lprintf' "calculating record_week\n");
  let (t, r) = vect_from graph.day graph.week in
  (if List.length graph.week < int_of_float max_points
     then graph.week <- graph.week @ [t, r]
     else graph.week <- (List.tl graph.week) @ [t, r]);
  (if !!verbose then lprintf' "record_week calculated successfully\n");
  let (t_h, _) = List.nth graph.month (List.length graph.month - 1) in
  if (t -. t_h) > month_step
    then record_month graph

(*************************************************************************)
(*                                                                       *)
(*                         record_day                                    *)
(*                                                                       *)
(*************************************************************************)

let record_day graph =
  (if !!verbose then lprintf' "calculating record_day\n");
  let (t, r) = vect_from graph.halfday graph.day in
  (if List.length graph.day < int_of_float max_points
     then graph.day <- graph.day @ [t, r]
     else graph.day <- (List.tl graph.day) @ [t, r]);
  (if !!verbose then lprintf' "record_day calculated successfully\n");
  let (t_h, _) = List.nth graph.week (List.length graph.week - 1) in
  if (t -. t_h) > week_step
    then record_week graph

(*************************************************************************)
(*                                                                       *)
(*                         record_halfday                                *)
(*                                                                       *)
(*************************************************************************)

let record_halfday graph =
  (if !!verbose then lprintf' "calculating record_halfday\n");
  let (t, r) = vect_from graph.hour graph.halfday in
  (if List.length graph.halfday < int_of_float max_points
     then graph.halfday <- graph.halfday @ [t, r]
     else graph.halfday <- (List.tl graph.halfday) @ [t, r]);
  (if !!verbose then lprintf' "record_halfday calculated successfully\n");
  let (t_h, _) = List.nth graph.day (List.length graph.day - 1) in
  if (t -. t_h) > day_step
    then record_day graph


(*************************************************************************)
(*                                                                       *)
(*                         record_hour                                   *)
(*                                                                       *)
(*************************************************************************)

let record_hour graph =
  (if !!verbose then lprintf' "calculating record_hour\n");
  let (t, r) = vect_from graph.quarter graph.hour in
  (if List.length graph.hour < int_of_float max_points
     then graph.hour <- graph.hour @ [t, r]
     else graph.hour <- (List.tl graph.hour) @ [t, r]);
  (if !!verbose then lprintf' "record_hour calculated successfully\n");
  let (t_h, _) = List.nth graph.halfday (List.length graph.halfday - 1) in
  if (t -. t_h) > halfday_step
    then record_halfday graph

(*************************************************************************)
(*                                                                       *)
(*                         record_quarter                                *)
(*                                                                       *)
(*************************************************************************)

let record_quarter queue graph =
  (if !!verbose then lprintf' "calculating record_quarter\n");
  let (t, _) = List.hd !queue in
  let l1 = List.tl !queue in
  let l2 = List.rev (List.tl (List.rev !queue)) in
  let r_avg = r_avg l1 l2 in
  let vect = (t, r_avg) in
  queue := [vect];
  (if List.length graph.quarter < int_of_float max_points
     then graph.quarter <- graph.quarter @ [vect]
     else graph.quarter <- (List.tl graph.quarter) @ [vect]);
  (if !!verbose then lprintf' "record_quarter calculated successfully\n");
  let (t_h, _) = List.nth graph.hour (List.length graph.hour - 1) in
  if (t -. t_h) > hour_step
    then record_hour graph

(*************************************************************************)
(*                                                                       *)
(*                         save_record                                   *)
(*                                                                       *)
(*************************************************************************)

let print_graph q t s =
  let n = List.length !q in
  let str =
    match t with
      GraphDownloads -> Printf.sprintf "Global Downloads %d %s" n s
    | GraphUploads -> Printf.sprintf "Global Uploads %d %s" n s
    | GraphFile (file_uid, GraphDownloads) -> Printf.sprintf "%s Downloads %d %s" (string_of_uid file_uid) n s
    | GraphFile (file_uid, GraphUploads) ->  Printf.sprintf "%s Uploads %d %s" (string_of_uid file_uid) n s
    | _ -> "Unknown"
  in
  str

let save_record rate (graph_type : graph_record) =
  let rate = float_of_int rate /. 1024. in
  let t = BasicSocket.current_time () in
  let last_vect = (t, rate) in
  let queue =
    try
      let q = Hashtbl.find global_queue graph_type in
      (if !!verbose then lprintf' "Record %s\n" (print_graph q graph_type "old"));
      q
    with _ ->
      let q = ref [last_time, 0.] in
      Hashtbl.add global_queue graph_type q;
       (if !!verbose then lprintf' "Record %s\n" (print_graph q graph_type "new"));
      q
  in
  queue := last_vect :: !queue;
  let first_vect = List.hd (List.rev !queue) in
  let t_i = fst first_vect in
  if (t -. t_i) > quarter_step
    then match graph_type with
             GraphDownloads ->
               begin
                 let graph = fst (!!global_graph) in
                 record_quarter queue graph
               end

           | GraphUploads ->
               begin
                 let graph = snd (!!global_graph) in
                 record_quarter queue graph
               end

           | GraphFile (file_uid, GraphDownloads) ->
               begin
                 let records =
                   try
                     List.assoc file_uid !!files_graph
                   with _ ->
                     begin
                       (if !!verbose then lprintf'
                          "In save_record / Downloads : Adding file %s\n" (string_of_uid file_uid));
                       files_graph =:= (file_uid, (dummy_graph (), dummy_graph ())) :: !!files_graph;
                       snd (List.hd !!files_graph)
                     end
                 in
                 let graph = fst records in
                 (if !!verbose then lprintf'
                    "Found fst of %s GraphDownloads in !!files_graph\n" (string_of_uid file_uid));
                 record_quarter queue graph;
                 (if !!verbose then lprintf'
                    "Found record_quarter of %s GraphDownloads in !!files_graph\n" (string_of_uid file_uid));
               end

           | GraphFile (file_uid, GraphUploads) ->
               begin
                 let records =
                   try
                     List.assoc file_uid !!files_graph
                   with _ ->
                     begin
                       (if !!verbose then lprintf'
                          "In save_record / Downloads : Adding file %s\n" (string_of_uid file_uid));
                       files_graph =:= (file_uid, (dummy_graph (), dummy_graph ())) :: !!files_graph;
                       snd (List.hd !!files_graph)
                     end
                 in
                 let graph = snd records in
                 (if !!verbose then lprintf'
                    "Found fst of %s GraphUploads in !!files_graph\n" (string_of_uid file_uid));
                 record_quarter queue graph;
                 (if !!verbose then lprintf'
                    "Found record_quarter of %s GraphUploads in !!files_graph\n" (string_of_uid file_uid));
               end

           | _ -> ()

(*************************************************************************)
(*                                                                       *)
(*                         cancel_file                                   *)
(*                                                                       *)
(*************************************************************************)

let cancel_file file_uid =
  if List.mem_assoc file_uid !!files_graph
    then begin
      (if !!verbose
         then lprintf' "In cancel_file : Cancelling file %s\n" (string_of_uid file_uid));
      files_graph =:= List.filter (fun (fi_uid, _) -> fi_uid <> file_uid) !!files_graph;
      Hashtbl.remove global_queue (GraphFile (file_uid, GraphDownloads));
      Hashtbl.remove global_queue (GraphFile (file_uid, GraphUploads))
    end

(*************************************************************************)
(*                                                                       *)
(*                         add_file                                      *)
(*                                                                       *)
(*************************************************************************)

let add_file file_uid =
  if not (List.mem_assoc file_uid !!files_graph)
    then begin
      (if !!verbose then lprintf' "In add_file : Adding file %s\n" (string_of_uid file_uid));
      files_graph =:= (file_uid, (dummy_graph (), dummy_graph ())) :: !!files_graph;
      let q = ref [last_time, 0.] in
      Hashtbl.add global_queue (GraphFile (file_uid, GraphDownloads)) q;
      Hashtbl.add global_queue (GraphFile (file_uid, GraphUploads)) q
    end

(*
let _ =
  Random.init 3;
  ignore (GMain.Timeout.add ~ms:1200 ~callback:
    (fun _ -> 
       let rate = Random.int 10000 in
       Printf2.lprintf "random rate = %d\n" rate;
       flush stdout;
       save_record rate GraphDownloads;
       save_record (rate / 4) GraphUploads;
       save_record (rate / 2) (GraphFile ("AAAAAAAAAAAAAAAAAAA", GraphDownloads));
       save_record (rate / 5) (GraphFile ("AAAAAAAAAAAAAAAAAAA", GraphUploads));
       save_record (rate / 3) (GraphFile ("BBBBBBBBBBBBBBBBBBB", GraphDownloads));
       save_record (rate / 7) (GraphFile ("BBBBBBBBBBBBBBBBBBB", GraphUploads));
       true
  ))
*)

let _ =
  ignore (GMain.Timeout.add ~ms:60000 ~callback:
    (fun _ ->
       (try Options.save graph_ini with _ -> ());
       (if !!verbose then lprintf' "Save graph_ini\n");
       true
  ))

