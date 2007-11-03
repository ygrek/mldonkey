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
module Mi = GuiMisc

let verbose = O.gtk_verbose_graphbase

let lprintf' fmt =
  Printf2.lprintf ("GuiGraphBase: " ^^ fmt)

let last_time () = BasicSocket.current_time ()

let max_points = 200.
let max_points_int = int_of_float max_points

let init_queue () =
  let q = GStack.make max_points_int in
  GStack.add 0. q;
  q

let dummy_graph () = {
  quarter = init_queue ();
  hour    = init_queue ();
  halfday = init_queue ();
  day     = init_queue ();
  week    = init_queue ();
  month   = init_queue ();
  year    = init_queue ();
}

let quarter = 15. *. 60.
let hour = 4. *. quarter
let halfday = 12. *. hour
let day = 2. *. halfday
let week = 7. *. day
let month = 4. *. week
let year = 52. *. week


let get_t (v : float * float) = fst v
let get_r (v : float * float) = snd v
let get_d v = fst v
let get_u v = snd v

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

    let hook_graph_time l time_ref =
      let t_ref = last_time () in
      let l = List.filter (fun (t, _) -> t_ref -. t < time_ref) l in
      match l with
        [] -> [t_ref, 0.]
      | _ -> l

    let value_to_graph v =
      let _t = last_time () in
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let graph_quarter =
            try
              let l = get_value "graph_quarter" (value_to_list value_to_floats) in
              let l = hook_graph_time l quarter in
              GStack.from_list max_points_int l
            with _ -> init_queue ()
          in
          let graph_hour =
            try
              let l = get_value "graph_hour" (value_to_list value_to_floats) in
              let l = hook_graph_time l hour in
              GStack.from_list max_points_int l
            with _ -> init_queue ()
          in
          let graph_halfday =
            try
              let l = get_value "graph_halfday" (value_to_list value_to_floats) in
              let l = hook_graph_time l halfday in
              GStack.from_list max_points_int l
            with _ -> init_queue ()
          in
          let graph_day =
            try
              let l = get_value "graph_day" (value_to_list value_to_floats) in
              let l = hook_graph_time l day in
              GStack.from_list max_points_int l
            with _ -> init_queue ()
          in
          let graph_week =
            try
              let l = get_value "graph_week" (value_to_list value_to_floats) in
              let l = hook_graph_time l week in
              GStack.from_list max_points_int l
            with _ -> init_queue ()
          in
          let graph_month =
            try
              let l = get_value "graph_month" (value_to_list value_to_floats) in
              let l = hook_graph_time l month in
              GStack.from_list max_points_int l
            with _ -> init_queue ()
          in
          let graph_year =
            try
              let l = get_value "graph_year" (value_to_list value_to_floats) in
              let l = hook_graph_time l year in
              GStack.from_list max_points_int l
            with _ -> init_queue ()
          in {
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
        ("graph_quarter", list_to_value floats_to_value (GStack.to_list_fif graph.quarter)) ::
        ("graph_hour",    list_to_value floats_to_value (GStack.to_list_fif graph.hour))    ::
        ("graph_halfday", list_to_value floats_to_value (GStack.to_list_fif graph.halfday)) ::
        ("graph_day",     list_to_value floats_to_value (GStack.to_list_fif graph.day))     ::
        ("graph_week",    list_to_value floats_to_value (GStack.to_list_fif graph.week))    ::
        ("graph_month",   list_to_value floats_to_value (GStack.to_list_fif graph.month))   ::
        ("graph_year",    list_to_value floats_to_value (GStack.to_list_fif graph.year))    ::
        []
      )

    let t = define_option_class "Graph" value_to_graph graph_to_value

end


let uid_option  = define_option_class "TypeUid"
    (fun v -> Mi.uid_to_common_uid (uid_of_string (value_to_string v)))
    (fun uid -> string_to_value (Mi.ustring_of_uid uid))

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

let (global_queue : (graph_record, (float * float) Queue.t) Hashtbl.t) = Hashtbl.create 63

let _ =
  let q = Queue.create () in
  Queue.add (GStack.last (get_d !!global_graph).quarter) q;
  Hashtbl.add global_queue GraphDownloads q;
  let q = Queue.create () in
  Queue.add (GStack.last (get_u !!global_graph).quarter) q;
  List.iter (fun (file_uid, (down, up)) ->
    let q_d = Queue.create () in
    Queue.add (GStack.last down.quarter) q_d;
    let q_u = Queue.create () in
    Queue.add (GStack.last up.quarter) q_u;
    Hashtbl.add  global_queue (GraphFile (file_uid, GraphDownloads)) q_d;
    Hashtbl.add  global_queue (GraphFile (file_uid, GraphUploads)) q_u;
  ) !!files_graph

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

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

let r_avg l =
  match l with
    [] -> 0.
  | (x, y) :: _ ->
      begin
        let (r, tmin, tmax, _) =
          List.fold_left (fun (s, tmin, tmax, (x0, y0)) (x1, y1) ->
            if x1 = x0
              then (s, tmin, tmax, (x1, y1))
              else (s +. (y1 +. y0) *. (x0 -. x1) /. 2., min tmin x1, max tmax x1, (x1, y1))
          ) (0., x, x, (x, y))  l
        in
        if tmax = tmin then r else r /. (tmax -. tmin)
      end

(*************************************************************************)
(*                                                                       *)
(*                         vect_from                                     *)
(*                                                                       *)
(*************************************************************************)

let vect_from previous current =
  let t_ref, _ = GStack.last current in
  let last_t, _ = GStack.last previous in
  let index =
    match (GStack.get_index_last previous) with
      Some i -> i
    | None -> raise Exit (* all stacks are init with dummy_graph so can't be empty *)
  in
  let rec iter l =
    let (t, r) = GStack.get_at_index index previous in
    if t > t_ref
    then if (GStack.iter_next index previous)
      then iter ((t, r) :: l)
      else ((t, r) :: l)
    else l
  in
  let pts = iter [] in
  let r_avg = r_avg (List.rev pts) in
  (last_t, r_avg)

(*************************************************************************)
(*                                                                       *)
(*                         record_year                                   *)
(*                                                                       *)
(*************************************************************************)

let record_year graph =
  let (t, r) = vect_from graph.month graph.year in
  GStack.put (t, r) graph.year

(*************************************************************************)
(*                                                                       *)
(*                         record_month                                  *)
(*                                                                       *)
(*************************************************************************)

let record_month graph =
  let (t, r) = vect_from graph.week graph.month in
  GStack.put (t, r) graph.month;
  let next_t, _ = GStack.last graph.year in
  if (t -. next_t) > year_step
    then record_year graph

(*************************************************************************)
(*                                                                       *)
(*                         record_week                                   *)
(*                                                                       *)
(*************************************************************************)

let record_week graph =
  let (t, r) = vect_from graph.day graph.week in
  GStack.put (t, r) graph.week;
  let next_t, _ = GStack.last graph.month in
  if (t -. next_t) > month_step
    then record_month graph

(*************************************************************************)
(*                                                                       *)
(*                         record_day                                    *)
(*                                                                       *)
(*************************************************************************)

let record_day graph =
  let (t, r) = vect_from graph.halfday graph.day in
  GStack.put (t, r) graph.day;
  let next_t, _ = GStack.last graph.week in
  if (t -. next_t) > week_step
    then record_week graph

(*************************************************************************)
(*                                                                       *)
(*                         record_halfday                                *)
(*                                                                       *)
(*************************************************************************)

let record_halfday graph =
  let (t, r) = vect_from graph.hour graph.halfday in
  GStack.put (t, r) graph.halfday;
  let next_t, _ = GStack.last graph.day in
  if (t -. next_t) > day_step
    then record_day graph

(*************************************************************************)
(*                                                                       *)
(*                         record_hour                                   *)
(*                                                                       *)
(*************************************************************************)

let record_hour graph =
  let (t, r) = vect_from graph.quarter graph.hour in
  GStack.put (t, r) graph.hour;
  let next_t, _ = GStack.last graph.halfday in
  if (t -. next_t) > halfday_step
    then record_halfday graph

(*************************************************************************)
(*                                                                       *)
(*                         record_quarter                                *)
(*                                                                       *)
(*************************************************************************)

let record_quarter queue graph =
  if not (Queue.is_empty queue)
  then begin
    let l = Queue.fold (fun accu v -> v :: accu) [] queue in
    Queue.clear queue;
    let (t, _) = List.hd l in
    let r = r_avg l in
    Queue.add (t, r) queue;
    GStack.put (t, r) graph.quarter;
    let next_t, _ = GStack.last graph.hour in
    if (t -. next_t) > hour_step
    then record_hour graph
  end

(*************************************************************************)
(*                                                                       *)
(*                         save_record                                   *)
(*                                                                       *)
(*************************************************************************)

let print_graph q t s =
  let n = Queue.length q in
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
      let q = Queue.create () in
      Hashtbl.add global_queue graph_type q;
       (if !!verbose then lprintf' "Record %s\n" (print_graph q graph_type "new"));
      q
  in
  Queue.add last_vect queue;
  let first_vect = Queue.peek queue in
  let t_i = get_t first_vect in
  if (t -. t_i) > quarter_step
    then begin
      try
        match graph_type with
             GraphDownloads ->
               begin
                 let graph = get_d (!!global_graph) in
                 record_quarter queue graph
               end

           | GraphUploads ->
               begin
                 let graph = get_u (!!global_graph) in
                 record_quarter queue graph
               end

           | GraphFile (file_uid, GraphDownloads) ->
               begin
                 let records =
                   try
                     List.assoc file_uid !!files_graph
                   with _ ->
                     begin
                       try
                         let suid = string_of_uid file_uid in
                         let _uid = uid_of_string suid in
                         (if !!verbose then lprintf'
                            "In save_record / Downloads : Adding file %s\n" suid);
                         files_graph =:= (file_uid, (dummy_graph (), dummy_graph ())) :: !!files_graph;
                         snd (List.hd !!files_graph)
                       with _ -> assert false
                     end
                 in
                 let graph = get_d records in
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
                       try
                         let suid = string_of_uid file_uid in
                         let _uid = uid_of_string suid in
                         (if !!verbose then lprintf'
                            "In save_record / Downloads : Adding file %s\n" suid);
                         files_graph =:= (file_uid, (dummy_graph (), dummy_graph ())) :: !!files_graph;
                         snd (List.hd !!files_graph)
                       with _ -> assert false
                     end
                 in
                 let graph = get_u records in
                 (if !!verbose then lprintf'
                    "Found fst of %s GraphUploads in !!files_graph\n" (string_of_uid file_uid));
                 record_quarter queue graph;
                 (if !!verbose then lprintf'
                    "Found record_quarter of %s GraphUploads in !!files_graph\n" (string_of_uid file_uid));
               end

           | _ -> ()
      with _ -> ()
    end

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
      try
        let suid = string_of_uid file_uid in
        let _uid = uid_of_string suid in
        (if !!verbose then lprintf' "In add_file : Adding file %s\n" suid);
        files_graph =:= (file_uid, (dummy_graph (), dummy_graph ())) :: !!files_graph;
        let q = Queue.create () in
        Hashtbl.add global_queue (GraphFile (file_uid, GraphDownloads)) q;
        Hashtbl.add global_queue (GraphFile (file_uid, GraphUploads)) q
      with _ -> ()
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

