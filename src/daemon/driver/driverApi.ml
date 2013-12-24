
open Options
open CommonComplexOptions
open CommonTypes
open ExtLib
open Prelude

module M = DriverApiTypes
module T = DriverApi_t
module J = DriverApi_j

exception Error of string
let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

let as_int s = try int_of_string s with _ -> error "not a number : %S" s
let str = Yojson.Basic.to_string
let dummy = str (`Assoc [])

let handle o r cmd =
  let args = r.Http_server.get_url.Url.args in
  let user = o.conn_user in
  let with_files spec f =
    let open CommonFile in
    let failed = ref [] in
    begin match spec with
    | "all" -> List.iter (fun file -> try f file user.ui_user with exn -> tuck failed (file,exn)) !!files
    | n -> f (file_find @@ as_int n) user.ui_user
    end;
    let failed = List.map (fun (file,exn) -> { T.info_id = file_num file; info_info = Printexc.to_string exn; }) !failed in
    match failed with
    | [] -> dummy
    | l -> J.string_of_failed { T.failed = l; }
  in
  match cmd with
  | "downloads" :: [] ->
    J.string_of_files @@ List.map M.of_file !!files

  | "downloads" :: "start" :: [] ->
    let urls = List.filter_map (function ("url",s) -> Some s | _ -> None) args in
    let result = List.map (fun url ->
        let (status,results) = DriverInteractive.dllink_start url user.ui_user in
        { T.dl_url = url; dl_status = status; dl_info = results; }
      ) urls
    in
    J.string_of_dlstarts result

  | "downloads" :: spec :: "pause" :: [] -> with_files spec CommonFile.file_pause
  | "downloads" :: spec :: ("resume"|"unpause") :: [] -> with_files spec CommonFile.file_resume
  | "downloads" :: spec :: "cancel" :: [] -> with_files spec CommonInteractive.file_cancel

  | "searches" :: [] ->
    J.string_of_searches @@ List.map M.of_search user.ui_user_searches

  | "searches"::"start"::(["local"] | ["remote"] | [] as typ) ->
    let typ = match typ with ["local"] -> LocalSearch | _ -> RemoteSearch in
    let query = try List.assoc "query" args with Not_found -> error "no query specified" in
    let query, net = CommonSearch.search_of_args @@ String2.tokens query in
    let buf = Buffer.create 10 in
    let s = CommonInteractive.start_search user
      (let module G = GuiTypes in
      { G.search_num = 0;
        G.search_query = query;
        G.search_max_hits = 10000;
        G.search_type = typ;
        G.search_network = net;
      }) buf
    in
    J.string_of_info { T.info_id = s.search_num; info_info = Buffer.contents buf; }

  | "searches"::spec::"forget"::[] ->
    begin
      let forget n = CommonSearch.search_forget user (CommonSearch.search_find n) in
      match spec with
      | "all" -> List.iter (fun s -> forget s.search_num) user.ui_user_searches
      | "last" -> (match user.ui_user_searches with [] -> () | s :: _ -> forget s.search_num)
      | n -> forget @@ as_int n
    end;
    dummy

  | _ ->
    error "unknown api call"
