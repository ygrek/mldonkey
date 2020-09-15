(* Copyright 2001, 2002 Simon, INRIA *)
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

open Queues
open Printf2
open Options
open BasicSocket

open TcpBufferedSocket
open CommonFile
open CommonGlobals
open CommonOptions
open CommonTypes

(* BUGS:

* From mlnet.log: not all sources in [new_sources] are ready. More
sources for a file than total sources in [sources_by_uid]. Probably the
same source entered several times inside the same queue. After a connection
failure, we should not only change the request_time but also the
  request_score in case it is initial_new_source_score.

*)

(* TODO:

* Limit total number of indirect connections
* Implement need_new_sources
* The concept of 'source' should replace the concept of 'client' for the
  interface. The old version of source management used to keep good clients
  so that useful information about them would not be lost.

*)

(* A source might be in the following states:

(1) Not Connected: the source appears in the queues between
    'new_sources_queue' and 'old_sources_queue2'.

(2) Connecting: 'source_last_attempt' is not zero. The source appears in the
    queue 'connecting_sources_queue' for all files.

(3) Connected: The source appears either in the queue
  'connected_sources_queue' or 'busy_sources_queue' for every files.
  If 'r.request_time' is 0, it means we can query the source whenever we
  want, but we are already too busy, so the source is in
  'busy_sources_queue'. Otherwise, we are not ready to query, and the source
  has to be in 'connected_sources_queue'.

*)


type request_result =
| File_possible   (* we asked, but didn't know *)
| File_not_found  (* we asked, the file is not there *)
(* | File_expected    we asked, because it was announced *)
| File_new_source (* we never asked, but we should *)
| File_found      (* the file was found *)
| File_chunk      (* the file has chunks we want *)
| File_upload     (* we uploaded from this client *)
(* | File_unknown     We don't know anything *)

let not_found_score = -5
let possible_score = -3
let new_source_score = 1 (* after the first request *)
let found_score = 3
let chunk_score = 5
let upload_score = 7
let initial_new_source_score = 10 (* before the first request *)

let outside_queue = -1
let new_sources_queue = 0
let good_sources_queue = 1
let ready_saved_sources_queue = 2
let waiting_saved_sources_queue = 3
let old_sources1_queue = 4
let old_sources2_queue = 5
let old_sources3_queue = 6
let do_not_try_queue = 7
let connected_sources_queue = 8
let connecting_sources_queue = 9
let busy_sources_queue = 10

let queue_name = [|
  "new_sources";
  "good_sources";
  "ready_saved_sources";
  "waiting_saved_sources";
  "old_sources1";
  "old_sources2";
  "old_sources3";
  "do_not_try_queue";
  "connected_sources";
  "connecting_sources";
  "busy_sources";
|]


let nqueues = Array.length queue_name

let queue_period = Array.make nqueues 600

let () =
  queue_period.(new_sources_queue) <- 0;
  queue_period.(connected_sources_queue) <- 0;
  queue_period.(connecting_sources_queue) <- 0;
  queue_period.(busy_sources_queue) <- 0;
  queue_period.(good_sources_queue) <- 0;
  queue_period.(ready_saved_sources_queue) <- 0;
  queue_period.(waiting_saved_sources_queue) <- 0;
  queue_period.(old_sources1_queue) <- 0;
  queue_period.(old_sources2_queue) <- 0;
  queue_period.(old_sources3_queue) <- 0

module Make(M:


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Argument                              *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

    sig

      val module_name : string

      type source_uid
      val dummy_source_uid : source_uid
      val source_uid_to_value: source_uid -> Options.option_value
      val value_to_source_uid: Options.option_value -> source_uid

      type source_brand
      val dummy_source_brand : source_brand
      val source_brand_to_value: source_brand -> Options.option_value
      val value_to_source_brand: Options.option_value -> source_brand

      val direct_source : source_uid -> bool
      val indirect_source : source_uid -> bool
    end) =
  (struct


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*                                                                       *)
(*                         FUNCTOR Body                                  *)
(*                                                                       *)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)

(*************************************************************************)
(*                                                                       *)
(*                         Types                                         *)
(*                                                                       *)
(*************************************************************************)

     type source = {
       source_uid : M.source_uid;
       mutable source_files : file_request list;

(* the 'source_score' increases with failures in connections *)
       mutable source_score : int;

(* the 'source_num' that should be used to create the client corresponding to
  this source *)
       mutable source_num : int;

(* the 'source_age' is the time of the last successful connection *)
       mutable source_age : int;

(* the 'source_connecting' indicates that this source is currently in the
  process of being connected. *)
       mutable source_last_attempt : int;
       mutable source_sock : tcp_connection;

       mutable source_brand : M.source_brand;
       mutable source_country_code : int option;
     }

     and file_request = {
       request_file : file_sources_manager;
       mutable request_queue : int;
       mutable request_time : int;
       mutable request_score : int;
     }

     and file_sources_manager = {
       manager_uid : string;
       mutable manager_sources : source Queues.Queue.t array;
       mutable manager_active_sources : int;
       mutable manager_all_sources : int;
       mutable manager_file : (unit -> file);
     }
         
     and functions = {
       mutable function_connect: (M.source_uid -> int option -> unit);
       mutable function_query: (M.source_uid -> string -> unit);
       
       mutable function_string_to_manager: (string -> file_sources_manager);
       
       mutable function_max_connections_per_second : (unit -> int);
       mutable function_max_sources_per_file : (unit -> int);
       
       mutable function_add_location :
         (M.source_uid -> string -> int option -> unit);
       mutable function_remove_location :
         (M.source_uid -> string -> unit);
     }

(*************************************************************************)
(*                                                                       *)
(*                         Modules                                       *)
(*                                                                       *)
(*************************************************************************)

     module HS = Weak.Make(struct
       type t = source
       let hash s = Hashtbl.hash s.source_uid
       let equal x y = x.source_uid = y.source_uid
     end)

     module H = Weak.Make(struct
       type t = source
       let hash s = Hashtbl.hash s.source_num
       let equal x y = x.source_num = y.source_num
     end)

     module SourcesQueueCreate = Queues.Make(struct
       type t = source
       let compare s1 s2 = compare s1.source_uid s2.source_uid
     end)

(*************************************************************************)
(*                                                                       *)
(*                         Global variables                              *)
(*                                                                       *)
(*************************************************************************)

     let dummy_source = {
       source_uid = M.dummy_source_uid;
       source_files = [];

       source_num = 0;
       source_score = 0;
       source_age = 0;
       source_last_attempt = 0;
       source_sock = NoConnection;

       source_brand = M.dummy_source_brand;
       source_country_code = None;
     }

     let last_refill = ref 0

     let not_implemented s _ =
       failwith (Printf.sprintf "CommonSources.%s not implemented" s)

     let functions = {
       function_connect = not_implemented "function_connect";
       function_query = not_implemented "function_query";
       function_string_to_manager = not_implemented
         "function_string_to_manager";

       function_max_connections_per_second = (fun _ ->
                                                !!max_connections_per_second);
       function_max_sources_per_file = (fun _ -> 10);

       function_add_location = not_implemented "function_add_location";
       function_remove_location = not_implemented "function_remove_location";
     }

     let indirect_connections = ref 0

(*************************************************************************)
(*                                                                       *)
(*                         Global tables                                 *)
(*                                                                       *)
(*************************************************************************)

     let sources_by_uid = HS.create 13557
     let sources_by_num = H.create 13557

     let file_sources_managers = ref []

     let connecting_sources = Fifo.create ()

     let next_direct_sources = Fifo.create ()
     let next_indirect_sources = ref []
        

     let active_queue q =
       q >= connected_sources_queue && q <= busy_sources_queue

(*************************************************************************)
(*                                                                       *)
(*                         request_score                                 *)
(*                                                                       *)
(*************************************************************************)

     let request_score r = r.request_score

     let set_score_part r score =
       r.request_score <- score


(*************************************************************************)
(*                                                                       *)
(*                         other helper functions                        *)
(*                                                                       *)
(*************************************************************************)

     let rec find_throttled_queue queue =
       if queue_period.(queue) > 0 || queue = old_sources3_queue then
         queue
       else
         find_throttled_queue (queue + 1)

     let get_throttle_delay m q throttled =
       if throttled then
         (max 0    
            (queue_period.(q) 
             - (file_priority (m.manager_file ())) 
             + Queue.length m.manager_sources.(connected_sources_queue))
         )
       else 0
  
(*
 * determine the number of (throttled) ready sources for a manager queue
 *)

(* I know it's evil to break out of an X.iter using an exception...
   But that function really needs to be fast.
   Also, this works because Queues are based on Sets, and that Set.iter
   gives elements in increasing keys order *)
     exception BreakOutOfLoop

     let count_file_ready_sources m q throttled =
       let ready_count = ref 0 in
       let throttle_delay = get_throttle_delay m q throttled in
       let ready_threshold = 
         last_time () - !!min_reask_delay - throttle_delay in
       (try
          Queue.iter
            (fun (time, s) ->
               if time >= ready_threshold then raise BreakOutOfLoop;
               incr ready_count
            ) m.manager_sources.(q)
        with BreakOutOfLoop -> ());
       !ready_count

(*
 * determine the total number of ready sources for all downloading files per queue
 *)
     let count_ready_sources queue throttled =
       List.fold_left (fun ready_count m ->
            let f = m.manager_file () in
            if file_state f = FileDownloading then
              ready_count + count_file_ready_sources m queue throttled
            else ready_count
         ) 0 !file_sources_managers


     let find_max_overloaded q managers =
       let _, remaining_managers =
         List.fold_left (fun ((current_max, remaining_managers) as acc) m ->
            let ready_sources = count_file_ready_sources m q true in
            if ready_sources > current_max then
              (ready_sources, [m])
            else if ready_sources = current_max then
              (current_max, m :: remaining_managers)
            else acc
         ) (-1, []) managers in
       remaining_managers


(*************************************************************************)
(*                                                                       *)
(*                         print_source                                  *)
(*                                                                       *)
(*************************************************************************)

     let print_source buf s =
       Printf.bprintf buf "Source %d:\n" s.source_num;
       Printf.bprintf buf "   score: %d\n" s.source_score;
       if s.source_age <> 0 then
         Printf.bprintf buf "   age: %d\n" s.source_age;
       if s.source_last_attempt <> 0 then
         Printf.bprintf buf "   last_attemps: %d" s.source_last_attempt;
       List.iter (fun r ->
         Printf.bprintf buf "     File %s\n" 
           (file_best_name (r.request_file.manager_file ()));
         Printf.bprintf buf "       Score: %d\n" r.request_score;
         if r.request_time <> 0 then
           Printf.bprintf buf "       Time: %d\n" r.request_time;
       ) s.source_files


(*
 *
 *                         need_new_sources
 *
 *)

     let need_new_sources file =
       let ready_threshold = last_time () - !!min_reask_delay in
       let ready_count = ref 0 in
       for i = good_sources_queue to old_sources1_queue do
         let lookin = file.manager_sources.(i) in
         try
           Queue.iter (fun (time, s) ->
              if time >= ready_threshold then raise BreakOutOfLoop;
              incr ready_count
           ) lookin
         with BreakOutOfLoop -> ()
       done;
       (* let work_count = !ready_count +
          (Queue.length ( file.manager_sources.( new_sources_queue ) )) +
          (Queue.length ( file.manager_sources.( connected_sources_queue ) ))
          in *)
       let f = file.manager_file () in
       (* lprintf "commonSources: need_new_source: ready= %d new= %d con= %d prio= %d %s\n"
          !readyCount
          (Queue.length ( file.manager_sources.( new_sources_queue ) ) )
          (Queue.length ( file.manager_sources.( connected_sources_queue ) ) )
          (file_priority f)
          (if (file_priority f) + 20 > workCount then "we need" else "have enough");
       *)
       (* (file_priority f) + 20 > work_count *)
       (* let max_s = functions.function_max_sources_per_file () in
          (file_priority f)*(max_s/20) + max_s > !all_ready_s + new_s *)
       (file_priority f) + 20 > !ready_count


(*************************************************************************)
(*                                                                       *)
(*                         print                                         *)
(*                                                                       *)
(*************************************************************************)


     let print buf output_type =
       let pos_to_string v =
          if v > 0 then string_of_int v else "-" in

       html_mods_cntr_init ();
       let mycntr = ref 1 in

       let html_tr () =
         mycntr := html_mods_cntr ();
         Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" !mycntr in

       let html_tr_same () = 
         Printf.bprintf buf "\\<tr class=\\\"dl-%d\\\"\\>" !mycntr in

       (* Header *)
       if output_type = HTML then
         let header = Printf.sprintf "File sources per manager queue (%d)" 
           (List.length !file_sources_managers) in
         
         Printf.bprintf buf "\\<div class=results\\>";
         html_mods_table_header buf "sourcesTable" "sources" [];
         Printf.bprintf buf "\\<tr\\>";
         html_mods_td buf [
           ("", "srh", "Statistics on sources ");
           ("", "srh", "@ " ^ log_time ());
           ("", "srh", header); ];
         Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\n";
         
         html_mods_table_header buf "sourcesTable" "sources" [
           ( Str, "srh br", "New sources", 
             Printf.sprintf "New(%d)" new_sources_queue );
           ( Str, "srh br", "Good sources", 
             Printf.sprintf "Good(%d)" good_sources_queue );
           ( Str, "srh br", "Ready saved sources", 
             Printf.sprintf "Ready(%d)" ready_saved_sources_queue);
           ( Str, "srh br", "Waiting saved sources", 
             Printf.sprintf "Wait(%d)" waiting_saved_sources_queue);
           ( Str, "srh br", "Old sources 1", 
             Printf.sprintf "Old1(%d)" old_sources1_queue );
           ( Str, "srh br", "Old sources 2", 
             Printf.sprintf "Old2(%d)" old_sources2_queue );
           ( Str, "srh br", "Old sources 3", 
             Printf.sprintf "Old3(%d)" old_sources3_queue );
           ( Str, "srh br", "Do not try sources", 
             Printf.sprintf "nTry(%d)" do_not_try_queue );
           ( Str, "srh br", "Connected sources", 
             Printf.sprintf "Conn(%d)" connected_sources_queue );
           ( Str, "srh br", "Connecting sources", 
             Printf.sprintf "Cing(%d)" connecting_sources_queue );
           ( Str, "srh br", "Busy sources", 
             Printf.sprintf "Busy(%d)" busy_sources_queue );
           ( Str, "srh br", "Total sources", "All" );
           ( Str, "srh br", "Filename", "Name" ); ];
       else begin
         Printf.bprintf buf "Statistics on sources: time %d\n" (last_time ());
         Printf.bprintf buf "File sources per manager queue(%d):\n" 
           (List.length !file_sources_managers);
         Printf.bprintf buf "new  good redy wait old1 old2 old3 ntry conn cing busy all\n";
         (* "9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999"
            11*5 chars
            one row each: all,indirect,ready *)
       end;

       let list_sum = List.fold_left (+) 0 in

       let nsources_per_queue = Array.make nqueues 0 in
       let nready_per_queue = Array.make nqueues 0 in
       let nindirect_per_queue = Array.make nqueues 0 in
       let ninvalid_per_queue = Array.make nqueues 0 in
       let nall = ref 0 in
       let naact = ref 0 in
       let naneed = ref 0 in
       let downloading_managers = 
         List.filter (fun m -> 
           file_state (m.manager_file ()) = FileDownloading
         ) !file_sources_managers in
       let my_file_sources_managers =
         List.sort (fun f1 f2 ->
           let best_name1 = file_best_name (f1.manager_file ()) in
           let best_name2 = file_best_name (f2.manager_file ()) in
           String.compare best_name1 best_name2
         ) downloading_managers in
       (* Files *)
       let ready_threshold = last_time () - !!min_reask_delay in
       List.iter (fun m ->
         let name = file_best_name (m.manager_file ()) in
         let need_sources = need_new_sources m in
         if need_sources then incr naneed;

         if m.manager_all_sources <> 0 then begin
           let slist = ref [] in
           let sreadylist = ref [] in
           let streadylist = ref [] in
           let sindirectlist = ref [] in
           let sinvalidlist = ref [] in
           (* Queues *)
           Array.iteri (fun i q ->
             let nready = ref 0 in
             let ntready = count_file_ready_sources m i true in
             let nindirect = ref 0 in
             let ninvalid = ref 0 in
             let nsources = ref 0 in
             (* Sources *)
             Queue.iter (fun (time, s) ->
               incr nsources;
               if M.indirect_source s.source_uid then incr nindirect
               else if not (M.direct_source s.source_uid) then incr ninvalid;
               if time < ready_threshold then incr nready
               else if i = new_sources_queue then begin
                 Printf.bprintf buf "ERROR: Source is not ready in new_sources_queue !\n";
                 print_source buf s
               end
             ) q;

             slist := Queue.length q :: !slist;
             sreadylist := !nready :: !sreadylist;
             streadylist := ntready :: !streadylist;
             sindirectlist := !nindirect :: !sindirectlist;
             sinvalidlist := !ninvalid :: !sinvalidlist;

             nready_per_queue.(i) <- nready_per_queue.(i) + !nready;
             nindirect_per_queue.(i) <- nindirect_per_queue.(i) + !nindirect;
             ninvalid_per_queue.(i) <- ninvalid_per_queue.(i) + !ninvalid;
             nsources_per_queue.(i) <- nsources_per_queue.(i) + !nsources;
           ) m.manager_sources; (* end Queues *)

           let slist = List.rev !slist in
           let sreadylist = List.rev !sreadylist in
           let streadylist = List.rev !streadylist in
           let sindirectlist = List.rev !sindirectlist in
           let sinvalidlist = List.rev !sinvalidlist in

           if output_type = HTML then begin
             html_tr ();
             html_mods_td buf (
               (List.map (fun qlength -> 
                  ("", "sr ar br", pos_to_string qlength)) slist) @
                 [ ("", "sr ar br", string_of_int m.manager_all_sources);
                   ("Filename", "sr", shorten name !!max_name_len); ] );

             Printf.bprintf buf "\\</tr\\>\n";

             html_tr_same ();
             html_mods_td buf (
               (List.map (fun sready -> 
                 ("", "sr ar br", pos_to_string sready)) sreadylist) @
                 [ ("", "sr ar br", Printf.sprintf "%d" (list_sum sreadylist));
                   ("", "sr", Printf.sprintf "ready with %d active%s" 
                      m.manager_active_sources
                      (if need_sources then " and needs sources"
                       else "")) ] );
             Printf.bprintf buf "\\</tr\\>\n";

             html_tr_same ();
             html_mods_td buf (
               (List.map (fun sready ->
                  ("", "sr ar br", pos_to_string sready)) streadylist) @
                 [ ("", "sr ar br", string_of_int (list_sum streadylist)); 
                   ("", "sr", "throttled ready"); ] );
             Printf.bprintf buf "\\</tr\\>\n";

             let anindirect = list_sum sindirectlist in
             if anindirect <> 0 then begin
               html_tr_same ();
               html_mods_td buf (
                 (List.map (fun sready ->
                    ("", "sr ar br", pos_to_string sready)) sindirectlist) @
                   [ ("", "sr ar br", string_of_int anindirect); 
                     ("", "sr", "indirect"); ] );
               Printf.bprintf buf "\\</tr\\>\n";
             end;

             let aninvalid = list_sum sinvalidlist in
             if aninvalid <> 0 then begin
               html_tr_same ();
               html_mods_td buf (
                 (List.map (fun sready ->
                    ("", "sr ar br", pos_to_string sready)) sinvalidlist) @
                   [ ("", "sr ar br", string_of_int aninvalid);
                     ("", "sr", "invalid"); ] );
               Printf.bprintf buf "\\</tr\\>\n";
             end;
           end
           else begin
             List.iter (Printf.bprintf buf "%4d ") slist;
             Printf.bprintf buf "%4d %s\n" m.manager_all_sources name;
             List.iter (Printf.bprintf buf "%4d ") sreadylist;
             Printf.bprintf buf "%4d     ready  %d active%s\n" 
               (list_sum sreadylist) m.manager_active_sources
               (if need_sources then "  needs sources"
                else "");
             List.iter (Printf.bprintf buf "%4d ") streadylist;
             Printf.bprintf buf "%4d     throttled ready\n" 
               (list_sum streadylist);
             let anindirect = list_sum sindirectlist in
             if anindirect <> 0 then begin
               List.iter (Printf.bprintf buf "%4d ") sindirectlist;
               Printf.bprintf buf "%4d     indirect\n" anindirect;
             end;
             let aninvalid = list_sum sinvalidlist in
             if aninvalid <> 0 then begin
               List.iter (Printf.bprintf buf "%4d ") sinvalidlist;
               Printf.bprintf buf "%4d     invalid\n" aninvalid;
             end
           end;

           nall := !nall + m.manager_all_sources;
           naact := !naact + m.manager_active_sources;
         end
         else begin (* m.manager_all_sources = 0 *)
           if output_type = HTML then begin
             html_tr ();

             html_mods_td buf [
               ("", "sr ar br", "-"); ("", "sr ar br", ""); 
               ("", "sr ar br", ""); ("", "sr ar br", ""); 
               ("", "sr ar br", ""); ("", "sr ar br", "");
               ("", "sr ar br", ""); ("", "sr ar br", ""); 
               ("", "sr ar br", ""); ("", "sr ar br", ""); 
               ("", "sr ar br", ""); ("", "sr ar br", "");
               ("", "sr br", shorten name !!max_name_len); ];
             Printf.bprintf buf "\\</tr\\>\n";
           end
           else Printf.bprintf buf "None %55s%s\n" "" name;
         end
       ) my_file_sources_managers; (* end Files *)

       (* next Header *)
       if output_type = HTML then begin
         Printf.bprintf buf "\\</table\\>\\</div\\>\n";

         html_mods_table_header buf "sourcesTable" "sources" [
           ( Str, "srh", "New sources", "New" );
           ( Str, "srh", "Good sources", "Good" );
           ( Str, "srh", "Ready sources", "Ready" );
           ( Str, "srh", "Waiting sources", "Wait" );
           ( Str, "srh", "Old sources 1", "Old1" );
           ( Str, "srh", "Old sources 2", "Old2" );
           ( Str, "srh", "Old sources 3", "Old3" );
           ( Str, "srh", "Do not try", "nTry" );
           ( Str, "srh", "Connected sources", "Conn" );
           ( Str, "srh", "Connecting sources", "Cing" );
           ( Str, "srh", "Busy sources", "Busy" );
           ( Str, "srh", "Total sources", "All" );
           ( Str, "srh", "Type", "Type" ); ];

       end
       else
         Printf.bprintf buf "new  good redy wait old1 old2 old3 ntry conn cing busy all\n";

       let slist = ref [] in
       let sreadylist = ref [] in
       let streadylist = ref [] in
       let sindirectlist = ref [] in
       let sinvalidlist = ref [] in
       let speriodlist = ref [] in
       (* Queues *)
       for i = 0 to nqueues - 1 do
         slist := nsources_per_queue.(i) :: !slist;
         sreadylist := nready_per_queue.(i) :: !sreadylist;
         streadylist := count_ready_sources i true :: !streadylist;
         sindirectlist := nindirect_per_queue.(i) :: !sindirectlist;
         sinvalidlist := ninvalid_per_queue.(i) :: !sinvalidlist;
         speriodlist := queue_period.(i) :: !speriodlist;
       done; (* end Queues *)

       let nsources = ref 0 in
       let nroq = ref 0 in
       HS.iter (fun s ->
         incr nsources;
         List.iter (fun r ->
           if r.request_queue = outside_queue then
             incr nroq;
         ) s.source_files;
       ) sources_by_uid;

       let slist = List.rev !slist in
       let sreadylist = List.rev !sreadylist in
       let streadylist = List.rev !streadylist in
       let sindirectlist = List.rev !sindirectlist in
       let sinvalidlist = List.rev !sinvalidlist in
       let speriodlist = List.rev !speriodlist in

       if output_type = HTML then begin
         html_tr ();
         html_mods_td buf (
           (List.map (fun q ->
             ("", "sr ar", pos_to_string q)) slist) @
             [ ("", "sr ar", Printf.sprintf "%d" !nall); 
               ("", "sr", 
                Printf.sprintf "all source managers (%d by UID) (%d ROQ)" 
                  !nsources !nroq);] );
         Printf.bprintf buf "\\</tr\\>\n";

         html_tr ();
         html_mods_td buf (
           (List.map (fun sready ->
             ("", "sr ar", pos_to_string sready)) sreadylist) @
             [ ("", "sr ar", Printf.sprintf "%d" (list_sum sreadylist)); 
               ("", "sr", 
                Printf.sprintf "ready with %d active and %i need sources" 
                  !naact !naneed); ] );
         Printf.bprintf buf "\\</tr\\>\n";

         html_tr ();
         html_mods_td buf (
           (List.map (fun sready ->
             ("", "sr ar", pos_to_string sready)) streadylist) @
           [ ("", "sr ar", Printf.sprintf "%d" (list_sum streadylist)); 
             ("", "sr", "throttled ready"); ] );
         Printf.bprintf buf "\\</tr\\>\n";

         let anindirect = list_sum sindirectlist in
         if anindirect <> 0 then begin
           html_tr ();
           html_mods_td buf (
             (List.map (fun sready ->
               ("", "sr ar", pos_to_string sready)) sindirectlist) @
             [ ("", "sr ar", Printf.sprintf "%d" anindirect); 
               ("", "sr", "indirect"); ] );
           Printf.bprintf buf "\\</tr\\>\n";
         end;

         let aninvalid = list_sum sinvalidlist in
         if aninvalid <> 0 then begin
           html_tr ();
           html_mods_td buf (
             (List.map (fun sready ->
               ("", "sr ar", pos_to_string sready)) sinvalidlist) @
               [ ("", "sr ar", Printf.sprintf "%d" aninvalid); 
                 ("", "sr", "invalid"); ] );
           Printf.bprintf buf "\\</tr\\>\n";
         end;

         html_tr ();
         html_mods_td buf (
           (List.map (fun sready ->
             ("", "sr ar", pos_to_string sready)) speriodlist) @
            [ ("", "sr", "");
              ("", "sr", "period"); ] );
         Printf.bprintf buf "\\</tr\\>\n";

         Printf.bprintf buf "\\</table\\>\\</div\\>\n";
       end
       else begin
         List.iter (Printf.bprintf buf "%4d ") slist;
         Printf.bprintf buf "%4d all source managers (%d by UID) (%d ROQ)\n" 
           !nall !nsources !nroq;
         List.iter (Printf.bprintf buf "%4d ") sreadylist;
         Printf.bprintf buf "%4d     ready  %d active  %i need sources\n" 
           (list_sum sreadylist) !naact !naneed;
         List.iter (Printf.bprintf buf "%4d ") streadylist;
         Printf.bprintf buf "%4d     throttled ready\n" (list_sum streadylist);
         let anindirect = list_sum sindirectlist in
         if anindirect <> 0 then begin
           List.iter (Printf.bprintf buf "%4d ") sindirectlist;
           Printf.bprintf buf "%4d     indirect\n" anindirect;
         end;
         let aninvalid = list_sum sinvalidlist in
         if aninvalid <> 0 then begin
           List.iter (Printf.bprintf buf "%4d ") sinvalidlist;
           Printf.bprintf buf "%4d     invalid\n" aninvalid;
         end;
         List.iter (Printf.bprintf buf "%4d ") speriodlist;
         Printf.bprintf buf "     period\n";
       end;

       let nconnected = ref 0 in
       Fifo.iter (fun (_, s) ->
         if s.source_last_attempt = 0 then incr nconnected;
       ) connecting_sources;
       if output_type = HTML then begin
         html_mods_table_header buf "sourcesTable" "sources" [
           ( Str, "srh", "Connecting sources", "Connecting sources" );
           ( Str, "srh", "Next direct sources", "Next direct sources" );
           ( Str, "srh", "Next indirect sources", "Next indirect sources" ); ];
         Printf.bprintf buf "\\<tr class=\\\"dl-1\\\"\\>";
         html_mods_td buf [
           ("", "sr", (Printf.sprintf "%d entries" 
                         (Fifo.length connecting_sources)) ^
              (if !nconnected > 0 then 
                 Printf.sprintf " (connected: %d)" !nconnected else ""));
           ("", "sr", Printf.sprintf "%d entries" 
              (Fifo.length next_direct_sources));
           ("", "sr", Printf.sprintf "%d entries" 
              (List.length !next_indirect_sources)); ];
         Printf.bprintf buf "\\</tr\\>\\</table\\>\\</div\\>\n\\</div\\>"
       end
       else begin
         Printf.bprintf buf "Connecting Sources: %d entries"
           (Fifo.length connecting_sources);
         if !nconnected > 0 then 
           Printf.bprintf buf " (connected: %d)" !nconnected;
         Printf.bprintf buf "\n";
         Printf.bprintf buf "Next Direct Sources: %d entries\n"
           (Fifo.length next_direct_sources);
         Printf.bprintf buf "Next Indirect Sources: %d entries\n"
           (List.length !next_indirect_sources)
       end


(*************************************************************************)
(*                                                                       *)
(*                         reschedule_source_for_file                    *)
(*                                                                       *)
(*************************************************************************)

     let reschedule_source_for_file saved s r =
       if r.request_queue = outside_queue then
         let queue =
           if r.request_score = not_found_score then do_not_try_queue
           else if s.source_last_attempt <> 0 then connecting_sources_queue
           else
             match s.source_sock with
             | (NoConnection | ConnectionWaiting _)  ->
                 (* State (1) *)
                 (* Two things matter: the global score and the local score *)
                 if s.source_score < 1 then
                   (* 2.5.25, replaced expected_score by
                      found_score, so that sources which
                      only have the file are not put in
                      good_sources_queue, unless they have
                      an interesting chunk AND not a bad
                      rank. *)
                   if r.request_score > found_score then
                     if saved then
                       if r.request_time + !!min_reask_delay < last_time () then
                         ready_saved_sources_queue
                       else waiting_saved_sources_queue
                     else if r.request_score = initial_new_source_score then
                       new_sources_queue
                     else good_sources_queue
                   else if r.request_score >= new_source_score then 
                     old_sources1_queue
                   else old_sources2_queue
                 else if s.source_score < 5 then old_sources3_queue
                 else do_not_try_queue

             | Connection _ ->
                 (* State (3) *)
                 if r.request_time = 0 then busy_sources_queue
                 else connected_sources_queue
            in
            let m = r.request_file in
            if !verbose_sources > 1 then
              lprintf_nl "[cSrc] Put source %d in queue %s"
                s.source_num queue_name.(queue);
            Queue.put m.manager_sources.(queue) (r.request_time, s);
            if active_queue queue then
              m.manager_active_sources <- m.manager_active_sources + 1;
            m.manager_all_sources <- m.manager_all_sources + 1;
            r.request_queue <- queue

(*************************************************************************)
(*                                                                       *)
(*                         iter_all_sources                              *)
(*                                                                       *)
(*************************************************************************)

     let iter_all_sources f m =
       Array.iter (fun q ->
         Queue.iter (fun (_, s) -> f s) q
       ) m.manager_sources

(*************************************************************************)
(*                         iter_qualified_sources                        *)
(*            Only these sources should be used in sourceexchage         *)
(*************************************************************************)
     let iter_qualified_sources f m =
       let q = m.manager_sources.(good_sources_queue) in
       Queue.iter (fun (_, s) -> f s) q

(*************************************************************************)
(*                                                                       *)
(*                         iter_active_sources                           *)
(*                                                                       *)
(*************************************************************************)

     let iter_active_sources f m =
       for i = connected_sources_queue to busy_sources_queue do
         let q = m.manager_sources.(i) in
         Queue.iter (fun (_, s) -> f s) q
       done

(*************************************************************************)
(*                                                                       *)
(*                         iter_relevant_sources                         *)
(*                                                                       *)
(*************************************************************************)
     let iter_relevant_sources f m =
       List.iter (fun i ->
         if i < nqueues then
           let q = m.manager_sources.(i) in
           Queue.iter (fun (_, s) -> f s) q
       ) !!relevant_queues

(*************************************************************************)
(*                                                                       *)
(*                         set_source_brand                              *)
(*                                                                       *)
(*************************************************************************)

     let set_source_brand s brand =
       s.source_brand <- brand

(*************************************************************************)
(*                                                                       *)
(*                         source_brand                                  *)
(*                                                                       *)
(*************************************************************************)

     let source_brand s = s.source_brand

(*************************************************************************)
(*                                                                       *)
(*                         remove_from_queue                             *)
(*                                                                       *)
(*************************************************************************)

     let remove_from_queue s r =
       if r.request_queue <> outside_queue then begin
         if !verbose_sources > 1 then
           lprintf_nl "[cSrc] Remove source %d from queue %s" s.source_num
             queue_name.(r.request_queue);

         let m = r.request_file in
         if active_queue r.request_queue then
           m.manager_active_sources <- m.manager_active_sources - 1;
         Queue.remove r.request_file.manager_sources.(r.request_queue)
           (r.request_time, s);
         r.request_queue <- outside_queue;
         m.manager_all_sources <- m.manager_all_sources - 1
       end

(*************************************************************************)
(*                                                                       *)
(*                         source_connecting                             *)
(*                                                                       *)
(*************************************************************************)

(* From state (1) to state (2) *)
     let source_connecting s =
       s.source_last_attempt <- last_time ();
       Fifo.put connecting_sources (s.source_last_attempt, s);
       List.iter (fun r ->
         if r.request_queue <> outside_queue then begin
           remove_from_queue s r;
           reschedule_source_for_file false s r;
         end
       ) s.source_files


(*************************************************************************)
(*                                                                       *)
(*                         source_query                                  *)
(*                                                                       *)
(*************************************************************************)

     let source_query s r =
       remove_from_queue s r;
       if r.request_score > not_found_score then
         (* query_files will query all files for a source, check that we are
            really downloading! example source s has file f1 and file f2,
            file f2 is paused we connect because of f1 and then query both
            files f1 and f2 ... and yes, we do a cleanup ... but a timed one,
            so we can't be sure *)
         if r.request_score > not_found_score &&
           file_state (r.request_file.manager_file ()) = FileDownloading
          then begin
            r.request_time <- 0; (* The source is ready for this request *)
            reschedule_source_for_file false s r; (* put it in busy_sources_queue *)
            (try
               functions.function_query s.source_uid r.request_file.manager_uid
             with e ->
               lprintf_nl "[cSrc] Exception %s in functions.function_query" (Printexc2.to_string e)
            )
          end

(*************************************************************************)
(*                                                                       *)
(*                         source_connected                              *)
(*                                                                       *)
(*************************************************************************)

(* From state (2) to state (3) *)
     let source_connected s =
       s.source_score <- 0;
       s.source_age <- last_time ();
       s.source_last_attempt <- 0;
       List.iter (fun r ->
(*            lprintf "SOURCE> request: "; *)
         if r.request_queue <> outside_queue then begin
(*                lprintf "score %d/%d last query %s\n"
                  r.request_score possible_score
                  (if r.request_time = 0 then "never" else
                    Printf.sprintf "%d secs"
                      (last_time () - r.request_time));                *)
           remove_from_queue s r;
           if r.request_score > possible_score &&
             r.request_time + !!min_reask_delay < last_time () then
               source_query s r;
           (try
              let m = r.request_file in
              functions.function_add_location s.source_uid
                m.manager_uid s.source_country_code with _ -> ());
           reschedule_source_for_file false s r
         end 
           (* else lprintf "outside queue\n" *)
       ) s.source_files

(*************************************************************************)
(*                                                                       *)
(*                         source_disconnected                           *)
(*                                                                       *)
(*************************************************************************)

(* From states (1) or (2) to state (3) *)
     let source_disconnected s =
       (match s.source_sock with
        | NoConnection -> ()
        | ConnectionWaiting token ->
            cancel_token token;
            s.source_sock <- NoConnection
        | Connection sock ->
            close sock Closed_for_timeout
       );
       let connecting = s.source_last_attempt <> 0 in
       (* source_last_attempt set to time, on connect_reply set
          to zero. if we never reached connect_reply, the ip is
          dead. Then we think we were *not* trying to connect
          later on ...
       *)
       s.source_last_attempt <- 0;
       List.iter (fun r ->
         if r.request_queue <> outside_queue then begin
           remove_from_queue s r;
           if connecting then begin
             r.request_time <- last_time ();
             if r.request_score = initial_new_source_score then
               set_score_part r new_source_score
           end
           else begin
             if r.request_time = 0 then
               (* we think we were not connecting,
                  but in some cases we were! and
                  now we imidiately reconnect for
                  that file, on a dead IP??
                  r.request_time <- last_time () - 600;
                  try this instead:
               *)
               r.request_time <- last_time ();
             (try
                let m = r.request_file in
                functions.function_remove_location s.source_uid
                  m.manager_uid
              with _ -> ())
           end;
           reschedule_source_for_file false s r;
         end;
       ) s.source_files

(*************************************************************************)
(*                                                                       *)
(*                         connect_source                                *)
(*                                                                       *)
(*************************************************************************)

     let connect_source s =
       if !verbose_sources > 1 then
         lprintf_nl "[cSrc] connect_source";
       s.source_score <- s.source_score + 1;
       functions.function_connect s.source_uid s.source_country_code

(*************************************************************************)
(*                                                                       *)
(*                         create_queues                                 *)
(*                                                                       *)
(*************************************************************************)

     let create_queues () =
       let queues = [|
         (* New sources *)
         (* We should change this to 'oldest_last' to improve Queue.remove *)
         (* instead of lifo *)
         SourcesQueueCreate.oldest_last ();
         (* Good sources *)
         (* We should change this to 'oldest_first' to improve Queue.remove *)
         (* instead of fifo *)
         SourcesQueueCreate.oldest_first ();
         (* Ready saved sources *)
         SourcesQueueCreate.oldest_last ();
         (* Waiting saved sources *)
         SourcesQueueCreate.oldest_first ();
         (* Old sources *)
         (* We should change this to 'oldest_first' to improve Queue.remove *)
         (* instead of fifo *)
         SourcesQueueCreate.oldest_first ();
         SourcesQueueCreate.oldest_first ();
         SourcesQueueCreate.oldest_first ();
         (* do_not_try *)
         SourcesQueueCreate.oldest_first ();
         (* Connected Sources *)
         SourcesQueueCreate.oldest_first ();
         (* Connecting Sources *)
         SourcesQueueCreate.oldest_first ();
         (* Busy Sources *)
         SourcesQueueCreate.oldest_first ();
       |] in
       if Array.length queues <> Array.length queue_name then begin
         lprintf_nl "[cSrc] Fatal error in CommonSources.create_queues";
         exit 2;
       end;
       queues

(*************************************************************************)
(*                                                                       *)
(*                         create_file_sources_manager                   *)
(*                                                                       *)
(*************************************************************************)

     let create_file_sources_manager file_uid =
       let m = {
         manager_uid = file_uid;
         manager_file = not_implemented "manager_file";
         manager_all_sources = 0;
         manager_active_sources = 0;
         manager_sources = create_queues ();
       } in
       file_sources_managers := m :: !file_sources_managers;
       m

(*************************************************************************)
(*                                                                       *)
(*                         remove_file_sources_manager                   *)
(*                                                                       *)
(*************************************************************************)

     let remove_file_sources_manager m =
       iter_all_sources (fun s ->
         s.source_files <- 
           List.filter (fun r -> r.request_file != m) s.source_files;
       ) m;
       m.manager_sources <- create_queues ();
       file_sources_managers := List2.removeq m !file_sources_managers


(*************************************************************************)
(*                                                                       *)
(*                        number_of_sources                              *)
(*                                                                       *)
(*************************************************************************)
     (* get number of sources for a file*)
     let number_of_sources f =
       f.manager_all_sources

(*************************************************************************)
(*                                                                       *)
(*                         create_source_by_uid                          *)
(*                                                                       *)
(*************************************************************************)

     let create_source_by_uid uid cc =
       try
         let finder =  { dummy_source with source_uid = uid } in
         HS.find sources_by_uid finder

       with Not_found ->
         if !verbose_sources > 1 then
           lprintf_nl "[cSrc] Creating new source";
         let n = CommonClient.book_client_num () in
         let s = { dummy_source with
                     source_uid = uid;
                     source_age = 0;
                     source_num = n;
                     source_files = [];
                     source_country_code = cc;
                 }  in
         HS.add sources_by_uid s;
         H.add sources_by_num s;
         s

(*************************************************************************)
(*                                                                       *)
(*                         find_source_by_uid                            *)
(*                                                                       *)
(*************************************************************************)

     let find_source_by_uid uid =
         let finder =  { dummy_source with source_uid = uid } in
         HS.find sources_by_uid finder

(*************************************************************************)
(*                                                                       *)
(*                         find_source_by_num                            *)
(*                                                                       *)
(*************************************************************************)

     let find_source_by_num num =
       let finder =  { dummy_source with source_num = num } in
       H.find sources_by_num finder

(*************************************************************************)
(*                                                                       *)
(*                         find_request                                  *)
(*                                                                       *)
(*************************************************************************)

     let rec iter_has_request rs file =
       match rs with
       | [] -> raise Not_found
       | r :: tail ->
           if r.request_file == file then r 
           else iter_has_request tail file

     let find_request s file =
       iter_has_request s.source_files file

(*************************************************************************)
(*                                                                       *)
(*                         find_request_result                           *)
(*                                                                       *)
(*************************************************************************)

     let find_request_result s file =
       let r = find_request s file in
       let score =  r.request_score in
       if score <= not_found_score then File_not_found 
       else if score <= possible_score then File_possible 
       else if score <= found_score then File_found 
       else if score <= chunk_score then File_chunk 
       else if score <= initial_new_source_score then File_new_source 
       else assert false

(*************************************************************************)
(*                                                                       *)
(*                         add_request                                   *)
(*                                                                       *)
(*************************************************************************)

     let check_time time =
       if time = 0 then last_time () - 650
       else time (* changed 2.5.24 *)

     let add_request s file time =
       let r =
         try
           let r = find_request s file in
           remove_from_queue s r;
           set_score_part r (if r.request_score = initial_new_source_score then
                               new_source_score
                             else r.request_score - 1);
           r.request_time <- check_time time;
           r
         with Not_found ->
           let r = {
             request_file = file;
             request_time = check_time time;
             request_score = possible_score;
             request_queue = outside_queue;
           } in
           s.source_files <- r :: s.source_files;
           r in
       reschedule_source_for_file false s r;
       r

(*************************************************************************)
(*                                                                       *)
(*                         set_request_score                            *)
(*                                                                       *)
(*************************************************************************)

     let set_request_score s file score =
       try
         let r = find_request s file in
         if (not (
(* If a request has been done in the last half-hour, and the source is
  announced as new, just forget it.  : why half-hour? - trying min_reask_delay *)
               score = initial_new_source_score &&
                 r.request_time + !!min_reask_delay > last_time ()
             )) ||
(* If a file has been paused, and resumed, it is flagged outside_queue / not_found_score in 
  clean_sources, but really should be re-added to the queues as soon as possible (while retaining 
  its request_time) or it is skipped for far too long (if it is even found again) - reschedule 
  now puts new_source_score in old1 *)
           (score = initial_new_source_score &&
               r.request_queue = outside_queue) then
             let score =
               if score = initial_new_source_score 
               then new_source_score
               else score in
             if r.request_queue < connected_sources_queue then
               remove_from_queue s r;
             set_score_part r score;
             reschedule_source_for_file false s r;
       with Not_found ->
         let r = {
           request_file = file;
           request_time = check_time 0;
           request_score = possible_score;
           request_queue = outside_queue;
         } in
         set_score_part r score;
         s.source_files <- r :: s.source_files;
         reschedule_source_for_file false s r

(*************************************************************************)
(*                                                                       *)
(*                         set_request_result                            *)
(*                                                                       *)
(*************************************************************************)

     let set_request_result s file result =
       set_request_score s file 
         (match result with
          | File_not_found -> not_found_score
          | File_found -> found_score
          | File_chunk -> chunk_score
          | File_upload -> upload_score
          | File_new_source -> initial_new_source_score
          | File_possible -> possible_score + 1)

(*************************************************************************)
(*                                                                       *)
(*                         source_to_value                               *)
(*                                                                       *)
(*************************************************************************)

     let source_to_value s assocs =
       let requests = ref [] in
       List.iter (fun r ->
         if r.request_score > possible_score then
           requests :=
             (SmallList
                [once_value (string_to_value r.request_file.manager_uid);
                 int_to_value r.request_score;
                 int_to_value r.request_time]
             ) :: !requests
       ) s.source_files;
       if !requests = [] then raise Exit;
       (
         ("sscore", int_to_value s.source_score ) ::
           ("addr", M.source_uid_to_value s.source_uid ) ::
           ("brand", M.source_brand_to_value s.source_brand ) ::
           ("files", smalllist_to_value (fun s -> s)
              !requests) ::
           ("age", int_to_value s.source_age ) ::
           assocs
       )


(*************************************************************************)
(*                                                                       *)
(*                         query_file                                    *)
(*                                                                       *)
(*************************************************************************)

     let query_file s file =
       if file_state (file.manager_file ()) = FileDownloading then
         let r = find_request s file in
         if r.request_time + !!min_reask_delay <= last_time () then

           (* There is really no need to query a not found source again
              for the file ... not even after an hour! *)
           if r.request_score > not_found_score then
             source_query s r


(*************************************************************************)
(*                                                                       *)
(*                         query_files                                   *)
(*                                                                       *)
(*************************************************************************)
     (* Query a source for all of its known files*)
     let query_files s =
       List.iter (fun f ->
         query_file s f.request_file;
       ) s.source_files


(*************************************************************************)
(*                                                                       *)
(*                         add_saved_source_request                      *)
(*                                                                       *)
(*************************************************************************)

     let add_saved_source_request s uid score time =
       if !verbose_sources > 1 then
         lprintf_nl "[cSrc] Request %s %d %d" uid score time;
       let file =
         try
           functions.function_string_to_manager uid
         with e ->
           if !verbose_sources > 0 then
             lprintf_nl "[cSrc] CommonSources: add_saved_source_request -> %s not found" uid;
           raise e
       in
       let r = add_request s file time in
       set_score_part r score;
       reschedule_source_for_file true s r;
       if !verbose_sources > 1 then
         lprintf_nl "[cSrc] Put saved source %d in queue %s" s.source_num
           queue_name.(r.request_queue)

(*************************************************************************)
(*                                                                       *)
(*                         value_to_source                               *)
(*                                                                       *)
(*************************************************************************)

     let value_to_source assocs =
(*        lprintf "(1) value_to_source\n";  *)
       let get_value name conv = conv (List.assoc name assocs) in

       let addr = get_value "addr" M.value_to_source_uid in
       let files = get_value "files"
         (value_to_list (fun s -> s)) in

       let last_conn =
         try get_value "age" value_to_int with _ -> 0 in

       let score = try get_value "sscore" value_to_int with _ -> 0 in
       let brand = try get_value "brand" M.value_to_source_brand with _ ->
         M.dummy_source_brand in

       if !verbose_sources > 1 then
         lprintf_nl "[cSrc] New source from value";
       let s = create_source_by_uid addr None in
       s.source_score <- score;
       s.source_age <- last_conn;
       s.source_brand <- brand;

(*        lprintf "(2) value_to_source \n"; *)

       let rec iter v =
         match v with
         | OnceValue v -> iter v
         | List [uid; score; time] | SmallList [uid; score; time] ->
             (try
                let uid = value_to_string uid in
                let score = value_to_int score in
                let time = value_to_int time in

(* added in 2.5.27 to fix a bug introduced in 2.5.25 *)
                let score =
                  if score land 0xffff = 0 then score asr 16 else score in

(*                  lprintf "(3) value_to_source \n"; *)

                add_saved_source_request s uid score time

              with e ->
                if !verbose_sources > 1 then
                  lprintf_nl "[cSrc] CommonSources.value_to_source: exception %s in iter request"
                    (Printexc2.to_string e))

         | (StringValue _) as uid ->
             (try
                let uid = value_to_string uid in
(*                  lprintf "(4) value_to_source \n"; *)

                let score = 0 in
                let time = 0 in
                add_saved_source_request s uid score time

              with e ->
                if !verbose_sources > 1 then
                  lprintf_nl "[cSrc] CommonSources.value_to_source: exception %s in iter request"
                    (Printexc2.to_string e))

         | _ -> assert false
             
       in
(*        lprintf "(5) value_to_source \n"; *)
       List.iter iter files;
(*        lprintf "(6) value_to_source \n"; *)
       raise SideEffectOption

(*************************************************************************)
(*                                                                       *)
(*                         refill_sources                                *)
(*                                                                       *)
(*************************************************************************)

     let refill_sources () =

(* Wait for 9 seconds before refilling, since we put at least 10 seconds
  of clients in the previous bucket.

  wrong assumption for me :
  we may have failed to fill the queue with what was available
        if !last_refill + 8 < last_time () then
*)
       try
         last_refill := last_time ();
         if !verbose_sources > 0 then begin
           lprintf_nl "[cSrc] CommonSources.refill_sources BEFORE:";
           let buf = Buffer.create 100 in
           print buf TEXT;
           lprintf "%s\n" (Buffer.contents buf);
         end;

         (*
           how much consecutive sources in the queue a file can have
           source_f1|source_f1|source_f1|source_f2...
           <- - - - - - - 3 - - - - - ->
           10 for finer priority scaling
         *)
         let max_consecutive = 10 in

         (*
           get at most nsources direct sources from a file
           return number of sources found,new queue position
         *)
         let rec get_sources nsource m queue took =
           (* do_not_try == avoid source bounceback, i.e. a dustbin *)
           if queue >= do_not_try_queue || nsource <= 0 then
             (* we tried all queue or found enough sources, good bye!*)
             took
           else
             let q = m.manager_sources.(queue) in
             if Queue.length q > 0 then
               let (request_time, s) = Queue.head q in
               let throttled = queue_period.(queue) > 0 && nsource > 1 in
               let throttle_delay = get_throttle_delay m queue throttled in
               if request_time + !!min_reask_delay + throttle_delay < last_time () then begin
                 if !verbose_sources > 1 then
                   lprintf_nl "[cSrc] Sources: take source from Queue[%s] for %s"
                     queue_name.(queue)
                     (file_best_name (m.manager_file ()));
                 (* put in the connecting queue*)
                 source_connecting s;
                 if M.direct_source s.source_uid then begin
                   Fifo.put next_direct_sources s;
                   (* we found a direct source try again in the _same_ queue *)
                   get_sources (nsource-1) m queue (took+1)
                 end
                 else begin
                   next_indirect_sources := s :: !next_indirect_sources;
                   (* we found an indirect source try again in the _same_
                      queue. indirect sources are "for free". *)
                   get_sources nsource m queue took
                 end
               end
               else begin
                 if !verbose_sources > 1 then
                   lprintf_nl "[cSrc] Source of queue %s is not ready for %s"
                     queue_name.(queue) (file_best_name (m.manager_file ()));
                 (* too early to take sources in this queue try again in the _next_ queue*)
                 if queue_period.(queue) = 0 then
                   (* queue not throttled, try next queue *)
                   let to_take =
                     (* a maximum of just one source from old3 queue *)
                     if queue+1 >= old_sources3_queue then min 1 nsource
                     else nsource in
                   get_sources to_take m (queue+1) took
                 else
                   (* throttled queue, and no ready sources ... *)
                   if nsource = 1 then
                     (* nsource = 1 not even a ready source without throttle-delay *)
                     get_sources 0 m (queue) took
                       (* exit here *)
                   else
                     (* finaly try to take at least one source, regardless of throttles *)
                     get_sources 1 m (queue) took
               end
             else begin
               if !verbose_sources > 1 then
                 lprintf_nl "[cSrc] Queue %s is empty for %s"
                   queue_name.(queue) (file_best_name (m.manager_file ()));
               (* no sources in this queue try again in the _next_ queue *)
               let to_take =
                 (* a maximum of just one source from old3 queue *)
                 if queue+1 >= old_sources3_queue then min 1 nsource
                 else nsource in
               get_sources to_take m (queue+1) took
             end in

         (* recalc list if there's no new file*)
         (* Fill only with sources from files being downloaded *)

         let nfiles = ref 0 in
         let files = ref [] in
         let min_priority = ref 0 in
         let sum_priority = ref 0 in
         List.iter (fun m ->
           match file_state (m.manager_file ()) with
           | FileDownloading ->
               let priority = file_priority (m.manager_file ()) in
               min_priority := min !min_priority priority;
               sum_priority := !sum_priority + priority;
               files := (priority, m ) :: !files;
               incr nfiles
           | _ -> () ) !file_sources_managers;

         if !files <> [] then begin

           (* 'normalize' to 0 priorities*)
           sum_priority := !sum_priority + (!nfiles * (-(!min_priority)));
           (* update priorities to be > 0 *)
           files := List.map (fun (p, f) ->
             let np = p - (!min_priority) in
             if np = 0 then begin
               incr sum_priority;
               (1, f)
             end
             else (np, f) ) !files;

           (*sort by highest priority*)
           files := List.sort (fun (p1,_) (p2,_) -> compare p2 p1) !files;

           (* calc sources queue size
              at least 3 sources per file*)
           let nsources = max (!nfiles * 3)
             (functions.function_max_connections_per_second () * 10) in

           (* calc how much sources a file can get according to its priority*)
           let sources_per_prio =  
             (float_of_int nsources) /. (float_of_int !sum_priority) in


           (*
             iter through files to queue sources
             flist_todo : next files to test
             assigned : number of sources already queued
             looped : number of times we allow to loop try to fill queue of sources
                        (how hard we try to fill queue)
           *)
           let rec iter_files assigned looped =

             (* throw in new sources at high pace and do not care
                about them in get_sources, this avoids "locking" a
                file's queue sources with thousands of new sources
                from SE *)
             let try_some_new_sources () =
               let extr = ref 0 in
               List.iter (fun m ->
                 let f = m.manager_file () in
                 let q = m.manager_sources.(new_sources_queue) in
                 if file_state f = FileDownloading && Queue.length q > 0 then
                   let (request_time, s) = Queue.head q in
                   source_connecting s;
                   if M.direct_source s.source_uid then begin
                     incr extr;
                     Fifo.put next_direct_sources s
                   end
                   else
                     next_indirect_sources := s :: !next_indirect_sources
               ) !file_sources_managers;
               !extr in

             let cleanup_some_old_sources () =
               (* Cleanup some sources *)
               List.iter (fun m ->
                 let f = m.manager_file () in
                 if file_state f = FileDownloading then
                   let remove_old q t =
                     if Queue.length q > 0 then
                       let (request_time, s) = Queue.head q in
                       if request_time + t  < last_time () then
                         remove_from_queue s (find_request s m) in
                       
                   remove_old m.manager_sources.(do_not_try_queue) 14400;
                   remove_old m.manager_sources.(old_sources3_queue) 2400;
                   remove_old m.manager_sources.(old_sources2_queue) 1200
               ) !file_sources_managers in

             let rec aux flist_todo assigned =
               if assigned >= nsources then cleanup_some_old_sources ()
               else
                 match flist_todo with
                 | (prio, file) :: t ->
                     let tt = 
                       min (truncate (sources_per_prio *. (float_of_int prio)))
                         max_consecutive in
                     let to_take = max tt 1 in
                     (* allow at least one source per file :
                        we will overflow a bit the expected next_direct_sources length
                        but it's for the good cause : not 'starving' some files
                     *)
                     let took = get_sources to_take file good_sources_queue 0 in
                     aux t (assigned + took)
                        
                 | [] ->
                     cleanup_some_old_sources ();

                     (* more power to the "runaway" (most overloaded) file, pick extra sources *)
                     let em =
                       let q = find_throttled_queue good_sources_queue in
                       if queue_period.(q) > 0 then
                         let max_overloaded = 
                           List.hd (find_max_overloaded q !file_sources_managers) in
                         let overhead = 
                           count_file_ready_sources max_overloaded q  true in
                         if overhead > 0 then
                           get_sources max_consecutive max_overloaded good_sources_queue 0
                         else 0
                       else 0 in
                      
                     if looped > 0 then
                       (* allow at most looped re-iter of list to not
                          loop endlessly *)
                       iter_files (assigned + em) (looped - 1) 
             in 
             let extr = try_some_new_sources () in
             aux !files (assigned + extr)

           in
           iter_files 0 3;

           (* adjust queue throttling *)
           let all_ready = ref 0 in
           List.iter (fun q ->
             let queue_throttled_ready = count_ready_sources q true in
             let queue_ready = count_ready_sources q false in
             all_ready := !all_ready + queue_throttled_ready;
             if !all_ready > nsources && queue_throttled_ready > 0 then begin
               (* no need, to increase period on a queue without ready sources *)
               (* lprintf "commonSources: increasing queue throttling for (ar=%d rc=%d qr=%d) %s\n" !allReady nsources queueReady queue_name.(q); *)
               queue_period.( q ) <- queue_period.( q ) + 1
             end
             else begin
               if queue_ready = 0 then begin
                 (* lprintf "commonSources: resetting queue throttling to 0 (ar=%d rc=%d qr=%d) %s\n" !allReady nsources queueReady queue_name.(q); *)
                 queue_period.( q ) <- 0
               end
               else begin
                 (* lprintf "commonSources: decreasing queue throttling for (ar=%d rc=%d qr=%d) %s\n" !allReady nsources queueReady queue_name.(q); *)
                 queue_period.( q ) <- max 0 (queue_period.( q ) - 1)
               end
             end
           ) [ good_sources_queue; old_sources1_queue; old_sources2_queue; old_sources3_queue ];

         end;

         if !verbose_sources > 0 then begin
           lprintf_nl "[cSrc] CommonSources.refill_sources AFTER:";
           let buf = Buffer.create 100 in
           print buf TEXT;
           lprintf "%s\n" (Buffer.contents buf);
         end;
       with e ->
         lprintf_nl "[cSrc] Exception %s in refill_sources"
           (Printexc2.to_string e)


(*************************************************************************)
(*                                                                       *)
(*                         clean_sources helper                          *)
(*                                                                       *)
(*************************************************************************)
     let put_all_outside_queue m q queue =
       let _, s = Queue.take q in
       m.manager_all_sources <- m.manager_all_sources - 1;
       if active_queue queue then
         m.manager_active_sources <- m.manager_active_sources - 1;
       List.iter (fun r ->
         if r.request_file == m then begin
           r.request_queue <- outside_queue;
           set_score_part r not_found_score
         end
       ) s.source_files

(*************************************************************************)
(*                                                                       *)
(*                         clean_sources                                 *)
(*                                                                       *)
(*************************************************************************)

     let clean_sources () =
       (* Maybe this should be dependant on the file (priority, state,...) ? *)
       let max_sources_per_file = functions.function_max_sources_per_file () in
       List.iter (fun m ->
         match file_state (m.manager_file ()) with
         | FileDownloading ->
             let nsources =  m.manager_all_sources in
             if nsources > max_sources_per_file then
               let rec iter nsources q queue =
                 if nsources > 0 then
                   if Queue.length q > 0 && 
                     queue <> good_sources_queue then begin
                       put_all_outside_queue m q queue;
                       iter (nsources-1) q queue
                     end
                   else
                     let do_iter q = iter nsources m.manager_sources.(q) q in
  
                     if queue = old_sources1_queue then do_iter do_not_try_queue
                     else if queue = do_not_try_queue then do_iter new_sources_queue 
                     else if queue = new_sources_queue then do_iter waiting_saved_sources_queue 
                     else if queue > good_sources_queue then do_iter (queue-1)

               in
               iter (nsources - max_sources_per_file) (m.manager_sources.(old_sources3_queue)) old_sources3_queue

         | _ ->
             let rec iter q queue =
               if Queue.length q > 0 then begin
                 put_all_outside_queue m q queue;
                 iter q queue
               end
               else
                 if queue > 0 then
                   iter m.manager_sources.(queue-1) (queue-1)
             in
             iter (m.manager_sources.(do_not_try_queue)) do_not_try_queue
       ) !file_sources_managers

(*************************************************************************)
(*                                                                       *)
(*                         connect_sources                               *)
(*                                                                       *)
(*************************************************************************)

     let connect_sources connection_manager =

       if !verbose_sources > 1 then
         lprintf_nl "[cSrc] connect_sources";
(* After 2 minutes, consider that connections attempted should be revoked. *)

       if !verbose_sources > 1 then
         lprintf_nl "[cSrc]   revoke connecting sources...";
       let rec iter () =
         if not (Fifo.empty connecting_sources) then
           let (time, s) = Fifo.head connecting_sources in
           if time <> s.source_last_attempt then begin
             ignore (Fifo.take connecting_sources);
             iter ()
           end 
           else if time + 120 < last_time () then begin
             ignore (Fifo.take connecting_sources);
             if s.source_last_attempt <> 0 then source_disconnected s;
             iter ()
           end
       in
       iter ();

(* First, require !!max_connections_per_second sources to connect to us.
The probability is very high they won't be able to connect to us. *)

       if !verbose_sources > 1 then
         lprintf_nl "[cSrc]   connect indirect sources...";
       let (first_sources, last_sources) =
         List2.cut !!max_connections_per_second !next_indirect_sources in
       next_indirect_sources := last_sources;
       List.iter (fun s ->
         ignore (connect_source s)) first_sources;

(* Second, for every file being downloaded, query sources that are already
connected if needed *)
       if !verbose_sources > 1 then
         lprintf_nl "[cSrc]   query connected sources...";
       List.iter (fun m ->
         match file_state (m.manager_file ()) with
         | FileDownloading ->
             let q = m.manager_sources.(connected_sources_queue) in
             let rec iter () =
               if Queue.length q > 0 then
                 let (time, s) = Queue.head q in
                 if time + !!min_reask_delay < last_time () then begin

                   let r = find_request s m in
                   (* lprintf "commonSources: connect_sources: second place for source_query !?\n"; *)
                   (* isn't that here pretty useless? *)
                   source_query s r;
                   (* After this step, the source is
                      either in 'busy_sources_queue',
                      if for some reason, the request
                      could not be sent, or in
                      'connected_sources_queue' at the
                      tail if the request could be sent.
                      This seems thus safe.
                   *)
                   iter ()
                 end in
             iter ()
         | _ -> () 
       ) !file_sources_managers;

       if !verbose_sources > 1 then
         lprintf_nl "[cSrc]   connect to sources...";
       (* Finally, connect to available sources *)
       try
         let max_sources = functions.function_max_connections_per_second () in
         if !verbose_sources > 1 then
           lprintf_nl "[cSrc] max_sources: %d" max_sources;
         let rec iter nsources refilled =
           if nsources > 0 && can_open_connection connection_manager then
             if Fifo.length next_direct_sources > 0 then
               let s = Fifo.take next_direct_sources in
               connect_source s;
               let nsources = 
                 match s.source_sock with
                 | NoConnection ->
                     if !verbose_sources > 1 then
                       lprintf_nl "[cSrc] not connected"; nsources
                 | _ -> nsources - 1 in
               iter nsources refilled
             else if not refilled then begin
               refill_sources ();
               iter nsources true
             end in
         iter max_sources false;
         if !verbose_sources > 1 then
           lprintf_nl "[cSrc]   done connect_sources";
       with Exit -> ()



(*************************************************************************)
(*                                                                       *)
(*                         attach_sources_to_file                        *)
(*                                                                       *)
(*************************************************************************)

     let value_to_module f v =
       match v with
       | Module list -> f list
       | _ -> failwith "Option should be a module"

     let option = define_option_class "Source"
       (fun v ->
(*            lprintf "(n) source !!\n"; *)
          value_to_module value_to_source v)
       (fun s -> Module (source_to_value s []))

     let file_sources_option = ref None

     let attach_sources_to_file section =
(*        lprintf "attach_sources_to_file\n"; *)
       let sources = match !file_sources_option with
         | None ->
(*              lprintf "attaching sources this time\n"; *)
             let sources = define_option section
               ["sources"] ""  (listiter_option option) [] in
(*              lprintf "done\n"; *)
             file_sources_option := Some sources;
             sources
         | Some sources ->  sources in
       sources =:= [];
       HS.iter (fun s -> sources =:= s :: !!sources) sources_by_uid;

       (fun _ -> sources =:= [])


(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)

     let () =
       Heap.add_memstat M.module_name (fun level buf ->

         let nsources_per_queue = Array.make nqueues 0 in
         let nready_per_queue = Array.make nqueues 0 in
         List.iter (fun m ->
           for i = 0 to nqueues -1 do
             let q = m.manager_sources.(i) in
             let nready = ref 0 in
             let nsources = ref 0 in
             let ready_threshold = last_time () - !!min_reask_delay in
             Queue.iter (fun (time, s) ->
               incr nsources;
               if time < ready_threshold then incr nready
               else if i = new_sources_queue then begin
                 Printf.bprintf buf "ERROR: Source is not ready in new_sources_queue !\n";
                 print_source buf s
               end
             ) q;
             nsources_per_queue.(i) <- nsources_per_queue.(i) + !nsources;
             nready_per_queue.(i) <- nready_per_queue.(i) + !nready;
           done
         ) !file_sources_managers;

         Printf.bprintf buf  "\nFor all managers (%d):\n" (List.length !file_sources_managers);
         for i = 0 to nqueues - 1 do
           Printf.bprintf buf "   Queue[%s]: %d entries (%d ready)\n"
             queue_name.(i) nsources_per_queue.(i) nready_per_queue.(i);
         done;

         let nsources = ref 0 in
         HS.iter (fun _ -> incr nsources) sources_by_uid;
         Printf.bprintf buf "Sources by UID table: %d entries\n" !nsources;
         let a1, a2, a3, a4, a5, a6 = HS.stats sources_by_uid in
         Printf.bprintf buf "Sources by UID table stats: %d %d %d %d %d %d\n" 
           a1 a2 a3 a4 a5 a6;
            
         nsources := 0;
         H.iter (fun _ -> incr nsources) sources_by_num;
         Printf.bprintf buf "Sources by NUM table: %d entries\n" !nsources;
         let a1, a2, a3, a4, a5, a6 = H.stats sources_by_num in
         Printf.bprintf buf "Sources by NUM table stats: %d %d %d %d %d %d\n" 
           a1 a2 a3 a4 a5 a6;

         Printf.bprintf buf "Used indirect connections: %d\n"
           !indirect_connections;

         let nconnected = ref 0 in
         Fifo.iter (fun (_, s) ->
           if s.source_last_attempt = 0 then incr nconnected;
         ) connecting_sources;
         Printf.bprintf buf "Connecting Sources: %d entries"
           (Fifo.length connecting_sources);
         if !nconnected > 0 then 
           Printf.bprintf buf " (connected: %d)" !nconnected;
         Printf.bprintf buf "\n";

         Printf.bprintf buf "Next Direct Sources: %d entries\n"
           (Fifo.length next_direct_sources);

         Printf.bprintf buf "Next Indirect Sources: %d entries\n"
           (List.length !next_indirect_sources)
       )

   end)


