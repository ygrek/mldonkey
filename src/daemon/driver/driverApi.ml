
open Options
open CommonComplexOptions
open CommonTypes
open ExtLib

module T = DriverApiTypes

let (@@) f x = f x
let (|>) x f = f x

let handle buf o r api =
  let args = r.Http_server.get_url.Url.args in
  let user = o.conn_user in
  match api with
  | "downloads" :: [] ->
    DriverApi_j.string_of_files @@ List.map T.of_file !!files

  | "downloads" :: "start" :: [] ->
    let urls = List.filter_map (function ("url",s) -> Some s | _ -> None) args in
    let result = List.map (fun url ->
        let (status,results) = DriverInteractive.dllink_start url user.ui_user in
        { DriverApi_t.dl_url = url; dl_status = status; dl_info = results; }
      ) urls
    in
    DriverApi_j.string_of_dlstarts result

  | "searches" :: [] ->
    DriverApi_j.string_of_searches @@ List.map T.of_search user.ui_user_searches

  | "searches"::"start"::(["local"] | ["remote"] | [] as typ) ->
    let typ = match typ with ["local"] -> LocalSearch | _ -> RemoteSearch in
    let query = try List.assoc "query" args with Not_found -> "" in
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
    let r = { DriverApi_t.search_start_id = s.search_num; search_start_info = Buffer.contents buf; } in
    DriverApi_j.string_of_search_start r

  | "searches"::spec::"forget"::[] ->
    begin
      let forget n = CommonSearch.search_forget user (CommonSearch.search_find n) in
      match spec with
      | "all" -> List.iter (fun s -> forget s.search_num) user.ui_user_searches
      | "last" -> (match user.ui_user_searches with [] -> () | s :: _ -> forget s.search_num)
      | n ->
        let n = try int_of_string n with _ -> raise Not_found in (* FIXME api error *)
        forget n
    end;
    Yojson.Basic.to_string (`Assoc [])

  | _ ->
    raise Not_found
