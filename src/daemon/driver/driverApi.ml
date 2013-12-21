
open Options
open CommonComplexOptions
open CommonTypes
open ExtLib

module T = DriverApiTypes

let handle buf o r api =
  match api with
  | "downloads.json" :: [] ->
    begin
      let files = List2.tail_map T.of_file !!files in
      DriverApi_j.string_of_files files
    end
  | "searches.json" :: [] ->
    begin
      let searches = List.map T.of_search o.conn_user.ui_user_searches in
      DriverApi_j.string_of_searches searches
    end
  | "downloads" :: "start.json" :: []->
    begin
      let urls = List.filter_map (function ("url",s) -> Some s | _ -> None) r.Http_server.get_url.Url.args in
      let result = List.map (fun url ->
          let (status,results) = DriverInteractive.dllink_start url o.conn_user.ui_user in
          { DriverApi_t.dl_url = url; dl_status = status; dl_info = results; }
        ) urls
      in
      DriverApi_j.string_of_dlstarts result
    end
  | _ ->
    raise Not_found

