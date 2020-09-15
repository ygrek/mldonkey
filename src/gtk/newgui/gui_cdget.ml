(* Copyright 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

      
      (*
POST /Find HTTP/1.1(13)(10)
User-Agent: Mozilla/5.0 (Linux 2.4.19-16mdk i686; U) Opera 6.11  [en](13)(10)
Host: us.imdb.com(13)(10)
Accept: text/html, image/png, image/jpeg, image/gif, image/x-xbitmap, */*(13)(10)
Accept-Charset: windows-1252, utf-8;q=1.0, utf-16;q=1.0, iso-8859-1;q=0.6, *;q=0.1(13)(10)
Accept-Encoding: deflate, gzip, x-gzip, identity, *;q=0(13)(10)
Referer: http://www.imdb.com/(13)(10)Cookie: uu=DSGQesG4lu9Lw2U4LDwHZQblezv1s+179cC+OICAjmhl4P5o58DbKLbD7WtWk61L8=; pc=giA2K9/9hucaPihI6AwmCg5JQs39yjT9/Wonzp0aN+6dajfOqAoXvrhaZ26+XmRcnklU/f5JVN1eGRTN8=(13)(10)Connection: Keep-Alive, TE(13)(10)
TE: deflate, gzip, chunked, identity, trailers(13)(10)
Content-type: application/x-www-form-urlencoded(13)(10)
Content-length: 22(13)(10)(13)(10)select=Titles&for=blow]

http://us.imdb.com/search
  
*)

open Printf2
open Options
open Gettext
open CommonTypes
open GuiProto
module M = Gui_messages


let mldonkey_cache = Filename.concat "." (
    CommonOptions.hidden_dir_prefix ^ "mldonkey_cache")
  
let file_from_url url = 
  try
    if not (Sys.file_exists mldonkey_cache) then
      Unix.mkdir mldonkey_cache 0o755;
    let file = String.copy url in
    for i = 0 to String.length file - 1 do
      match file.[i] with
        '/' -> file.[i] <- ':'
      | '?' | '*' | '&' -> file.[i] <- '_'
      | _ -> ()
    done;
    Filename.concat mldonkey_cache file
  with _ -> ""
      
let wget_string r f progress =
  let module H = Http_client in
  let file = file_from_url (Url.to_string r.H.req_url) in
  try
    let page = File.to_string file in
    BasicSocket.add_timer 0.1 (fun _ -> f page)
  with _ ->

      H.wget_string r (fun page ->
          (try File.from_string file page with _ -> 
                lprintf "Could not be saved"; lprint_newline ();
                ());
          lprintf "DOWNLOAD FINISHED"; lprint_newline ();            
          f page) progress

      
let request_and_parse parse_fun  url wtree = 
    let counter = ref 0 in
    wget_string url
      (fun page ->
        parse_fun wtree page)
    (fun pos max ->
        counter := !counter + pos;
        lprintf "pos: %d/%Ld" !counter max; lprint_newline (); 
    )


let make_url referer url args = 
  
  let module H = Http_client in
  let r = {
      H.basic_request with
      
      H.req_url = Url.of_string ~args: args   url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_user_agent = "Mozilla/5.0 (Linux 2.4.19-16mdk i686; U) Opera 6.11  [en]";
      H.req_referer = referer;
      H.req_request = H.GET;
    } in
  
  r
  
class ['a] tree (selection: 
    (('a -> unit) -> unit) ref) upper_tree label =
  let item = GTree.tree_item ~label: label () in
  let wtree = GTree.tree () in
  let subtrees = ref [] in
  let datas = ref [] in
  object(self)
    
    method subtree label =
      let subtree = new tree selection wtree label in
      subtrees := subtree :: !subtrees;
      subtree
    
    method item data label =
      let item = GTree.tree_item ~label: label () in
      wtree#append item;
      let style = wtree#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME !!Gui_options.color_list_bg))];
      style#set_base [ (`NORMAL, (`NAME !!Gui_options.color_list_bg))];
      style#set_font (Gdk.Font.load_fontset !!Gui_options.font_list);
      wtree#misc#set_style style;
      ignore (item#connect#select
          (fun () ->  
            selection := (fun f -> f data)
        ));
      ignore (item#connect#deselect
        (fun () -> selection := (fun _ -> ())));
      datas := data :: !datas
    
    method tree = wtree
      
    method expand = item#expand
    method connect = item#connect      
    method map f = 
      List.iter f !datas;
      List.iter (fun tree -> tree#map f) !subtrees
    
    initializer
      upper_tree#append item;
      item#set_subtree wtree;
      ignore (item#connect#select
          (fun () -> selection := self#map));
      ignore (item#connect#deselect
          (fun () -> selection := (fun _ -> ())))
      
end


let cached_or_on_expand (item : 'a tree) parse_fun referer url =
    let file = file_from_url url in
    (try
        let page = File.to_string file in
        item#expand ();
        BasicSocket.add_timer 0.1 (fun _ -> 
            parse_fun item page)
      with _ ->
          let expanded = ref false in
          ignore (item#connect#expand
              (fun _ ->
                if !expanded then
                  ()
                else
                  (
                    expanded := true;
                    let r = make_url referer url [] in
                    request_and_parse parse_fun r item
                  )
            )
          ))


let rec color_tree w bg font =
  List.iter (fun b ->
    match b#subtree with
        Some (wt: GTree.tree) ->
          let style = wt#misc#style#copy in
          style#set_bg [ (`NORMAL, bg)];
          style#set_base [ (`NORMAL, bg)];
          style#set_font font;
          wt#misc#set_style style;
          color_tree wt bg font
       | _ ->
          List.iter (fun e ->
            let style = e#misc#style#copy in
            style#set_base [ (`NORMAL, bg)];
            style#set_bg [ (`NORMAL, bg)];
            style#set_font font;
            e#misc#set_style style
          ) b#children
    ) w#children

class type url_results =
  object
    method clear : unit
    method coerce : GObj.widget
    method download : unit -> unit
    method set_tb_style : Gtk.Tags.toolbar_style -> unit
    method vbox_dir : GPack.box
    method hbox : GPack.box
    method wpane : GPack.paned
    method evbox : GBin.event_box
    method wscroll : GBin.scrolled_window
    method wtool1 : GButton.toolbar
    method wtool2 : GButton.toolbar
    method wtree : GTree.tree      
    method set_list_bg : GDraw.color -> Gdk.font -> unit
end
    

(***********************************************************************


                       FreeDB Bot
  

***********************************************************************)

module FreeDB = struct

    

let album_line = Str.regexp_case_fold
    "<tr><td><a href=\"\\([^\"]*\\)\">\\([^/]*\\) / \\([^<]*\\)</a>"

let track_line = Str.regexp_case_fold
  "<tr><td valign=top>[^0-9]*\\([0-9]*\\).</td><td valign=top>[^0-9]*\\([0-9]*:[0-9]*\\)</td><td><b>\\([^<]*\\)</b>"
    
type freedb_record = {
    mutable freedb_nresults : int;
    mutable freedb_result : (string -> string -> string -> unit);
  }

    
let freedb_parse record page = 
  try
    let rec iter start =
(*      lprintf "search %d/%d" start (String.length page); 
      lprint_newline (); *)
      let found = Str.search_forward album_line page start in
(*      lprintf "found %d" found; lprint_newline (); *)
      let url = Str.matched_group 1 page in
      let artist = Str.matched_group 2 page in
      let album = Str.matched_group 3 page in
      record.freedb_result url artist album;
      record.freedb_nresults <- record.freedb_nresults + 1;
      if record.freedb_nresults > 1000 then raise End_of_file;
      iter (found+1)
    in
    iter 0
  with _ -> ()

let referer = Some (Url.of_string "http://www.freedb.com/")

let fields = [ "Artist"; "Title/Track"; "Title"; "Track"; "Rest"]

let categories = ["All"; "Blues"; "Classical"; "Data";
    "Folk"; "Rock"; "Soundtrack"]
  
let query = 
  Q_AND [ 
    Q_KEYWORDS ("Album", ""); 
    Q_COMBO ("Fields", List.hd fields, fields);
    Q_COMBO ("Categories", List.hd categories, categories);
    ]
    
let freedb_url words fields genres =
  make_url referer
    "http://www.freedb.org/freedb_search.php"
    (
    ("words", words) ::
    
    (match fields with
        [] -> [ "allfields", "YES" ]
      | _ ->
          ("allfields", "NO") ::
          List.map (fun s -> 
              ("fields",  s)) fields
    ) @
      (match genres with
        [] -> [ "allcats" , "YES" ]
      | _ -> (* blues classical country data folk jazzs misc newage reggae rock soundtrack    *)
          ("allcats", "NO") ::
          List.map (fun s -> 
              "cats", s) genres
    ) @
      ["grouping", "none" ]
  )

      
let submit_freedb_search qe = 
  match qe with
    Q_AND [
      Q_KEYWORDS (_ , words);
      Q_COMBO (_, field, _);
      Q_COMBO (_, category, _);
    ]
    ->
      let field = String.lowercase field in
      let category = String.lowercase category in
      let fields = if field = "title+track" then 
          ["title"; "track"] else [field] in
      let category = if category = "all" then [] else [category] in
      let counter = ref 0 in
      let url = freedb_url words fields category in
      let record = {
          freedb_nresults = 0;
          freedb_result = (fun url artist album ->
              lprintf "Found %s : %s" artist album; lprint_newline ();
          );
        } in

      request_and_parse freedb_parse url record;
      record
  | _ -> assert false

      (*
type tree = 
  TreeArtist of string * 
| TreeAlbum of string * tree_node
    *)      

let freedb_parse_album wtree page =
(*  lprintf "page: \n%s\n" page; lprint_newline (); *)
  try
    let rec iter start =
(*      lprintf "search %d/%d" start (String.length page);  
      lprint_newline (); *)
      let found = Str.search_forward track_line page start in
(*      lprintf "found %d" found; lprint_newline (); *)
      let num = Str.matched_group 1 page in
      let length = Str.matched_group 2 page in
      
      let name = Str.matched_group 3 page in      
      
      let hbox_labels = GPack.hbox () in
      let wl_count = GMisc.label ~text: num
          ~packing: (hbox_labels#pack ~expand: false ~padding:3) 
        ~width: 100
          () 
      in
      let wl_name = GMisc.label ~text: name
          ~packing: (hbox_labels#pack ~expand: false ~padding:3) 
        ~width: 700
          () 
      in
      let wl_len = GMisc.label ~text: length
          ~packing: (hbox_labels#pack ~expand: false ~padding:3) 
        ~width: 100
          () 
      in
      let item = GTree.tree_item () in
      item#add hbox_labels#coerce;
      wtree#append item;
      iter (found+1)
    in
    iter 0
  with _ -> ()
      
let freedb_parse_album wtree page =
(*  lprintf "page: \n%s\n" page; lprint_newline (); *)
  
  let item = GTree.tree_item () in
  wtree#append item;

  let (wlist : 'a GList.clist) = GList.clist_poly 
      ~columns: 3
      ~titles_show: false
      ~titles: ["Track"; "Title"; "Length"]
    ~selection_mode: `EXTENDED
      ~packing: item#add 
      () 
  in
  try
    let rec iter start =
(*      lprintf "search %d/%d" start (String.length page);  
      lprint_newline (); *)
      let found = Str.search_forward track_line page start in
(*      lprintf "found %d" found; lprint_newline (); *)
      let num = Str.matched_group 1 page in
      let length = Str.matched_group 2 page in
      let name = Str.matched_group 3 page in      
      
      let row = wlist#append [num; name; length] in
      iter (found+1)
    in
    iter 0
  with _ -> 
      GToolbox.autosize_clist wlist;
      let style = wlist#misc#style#copy in
      style#set_bg [(`NORMAL, (`NAME !!Gui_options.color_list_bg))];
      style#set_base [(`NORMAL, (`NAME !!Gui_options.color_list_bg))];
      wlist#misc#set_style style

let request_album url label wt_sub =   
  let counter = ref 0 in
  wget_string url
    (fun page ->
(*      lprintf "DOWNLOAD FINISHED"; lprint_newline (); *)
      label#set_text (label#text ^ "(AVAILABLE)");
      freedb_parse_album wt_sub page)
  (fun pos max ->
      counter := !counter + pos;
(*      lprintf "pos: %d/%d" !counter max; lprint_newline (); *)
  )


class results qe =
  let freedb_record = submit_freedb_search qe in
  
  (object (self)
      inherit Gui_results_base.files ()
      
      val artist_table = Hashtbl.create 13
      
      method set_list_bg bg font =
        let style = wtree#misc#style#copy in
        style#set_bg [ (`NORMAL, bg)];
        style#set_base [ (`NORMAL, bg)];
        style#set_font font;
        wtree#misc#set_style style;
        color_tree wtree bg font

      method private insert url artist album =
(*      lprintf "Insert %s : %s (%s)" artist album url; 
lprint_newline (); *)
        let wtree = try
            Hashtbl.find artist_table artist  
          with _ ->
              let item = GTree.tree_item ~label: artist () in
              wtree#append item;
              let wt_sub = GTree.tree () in
              item#set_subtree wt_sub;
              Hashtbl.add artist_table artist wt_sub;
              item#expand ();
              wt_sub
        in
        let label = GMisc.label ~text: album () in
        label#set_alignment ~x: 0.0 ~y: 0.5 ();
        let item = GTree.tree_item () in
        item#add label#coerce;
        wtree#append item;
        let wt_sub = GTree.tree () in
        item#set_subtree wt_sub;
        let file = file_from_url url in
        (try
            let page = File.to_string file in
            BasicSocket.add_timer 0.1 (fun _ -> 
                label#set_text (label#text ^ "(AVAILABLE)");
                freedb_parse_album wt_sub page)
          with _ ->
              
              let module H = Http_client in
              let url = {
                  H.basic_request with
                  H.req_url = Url.of_string url;
                  H.req_proxy = !CommonOptions.http_proxy;
                } in
              
              let expanded = ref false in
              ignore (item#connect#expand
                  (fun _ ->
                    if !expanded then
                      ()
                    else
                      (
                        expanded := true;
                        request_album url label wt_sub
                      )
                )
              ));
              self#set_list_bg (`NAME !!Gui_options.color_list_bg) (Gdk.Font.load_fontset !!Gui_options.font_list)
      
      method clear =
        List.iter wtree#remove wtree#children;
      
      method set_tb_style st =
        wtool2#set_style st;
        wtool1#set_style st;
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ());
          

    method download () = ()
      
    initializer
      (*
      ignore
        (wtool#insert_button 
           ~text: (gettext M.download)
           ~tooltip: (gettext M.download_selected_dir)
           ~icon: (Gui_options.pixmap M.o_xpm_download_directory)#coerce
           ~callback: self#download
           ()
        );
*)

      freedb_record.freedb_result <- (fun url artist album ->
          self#insert url artist album  
      );

      let style = evbox#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox#misc#set_style style;

      self#set_tb_style !!Gui_options.toolbars_style;

      self#set_list_bg (`NAME !!Gui_options.color_list_bg)
        (Gdk.Font.load_fontset !!Gui_options.font_list)
        
        
      end : url_results)
end

(***********************************************************************


                       IMDB Bot
  

***********************************************************************)

module IMDB = struct

 let movie_line = Str.regexp_case_fold
    "<a href=\"\\(/title[^\"]*\\)\">\\([^<]*\\)</a>"

 let director_line = 
  Str.regexp_case_fold "Directed by[^\"]*\"[^\"]*\">\\([^<]*\\)<"

  (*
let genre_line = 
  Str.regexp_case_fold "Directed by[^\"]*\"[^\"]*\">\\([^<]*\\)<"
    *)

let rating_line = 
  Str.regexp_case_fold
    "User Rating:[^<]*\\(<[^<]+\\)*<b>\\([^<]*\\)</b>[^(]*(\\([^v]*\\)votes"

let begin_summary_line = 
  Str.regexp_case_fold
    "Summary:</b>\\([^<]*\\)</p><blockquote>"
  
let end_summary_line = Str.regexp_case_fold "</blockquote>"
  
type imdb_record = {
    mutable imdb_nresults : int;
    mutable imdb_result : (string -> string ->unit);
  }

let referer = Some (Url.of_string "http://us.imdb.com/find")
  
let imdb_name_url title = 
  make_url referer 
  (Printf.sprintf "http://us.imdb.com/Name?%s" (Url.encode title))
  []  

let imdb_title_url title = 
  let url = 
    Printf.sprintf "http://us.imdb.com/Tsearch" in
  
  let args = [
      "title", title;
      "restrict", "Movie+only";
      "GO.x", "13";
      "GO.y", "8";
    ] in
  
  let module H = Http_client in
  let r = {
      H.basic_request with
      
      H.req_url = Url.of_string ~args: args  url;
      H.req_proxy = !CommonOptions.http_proxy;
      H.req_user_agent = "Mozilla/5.0 (Linux 2.4.19-16mdk i686; U) Opera 6.11  [en]";
      H.req_referer = Some (Url.of_string "http://us.imdb.com/search");
      H.req_request = H.GET;
    } in
  
  assert (r.H.req_url.Url.args <> []);
  r

let imdb_parse record page =
  (try File.from_string "/tmp/imdb.html" page with _ -> ());
  try
    let rec iter start =
      lprintf "search movie_line %d/%d" start (String.length page); 
      lprint_newline ();  
      let found = Str.search_forward movie_line page start in
      lprintf "found %d" found; lprint_newline ();  
      let url = Str.matched_group 1 page in
      let movie = Str.matched_group 2 page in
      record.imdb_result url movie;
      record.imdb_nresults <- record.imdb_nresults + 1;
      if record.imdb_nresults > 1000 then raise End_of_file;
      iter (found+1)
    in
    iter 0
  with _ -> 
      lprintf "movie_line not found"; lprint_newline ()
      
let submit_imdb_search qe = 
  match qe with
    Q_KEYWORDS (_ , words) ->
      let counter = ref 0 in
      let (url : Http_client.request) = imdb_title_url words in
      let record = {
          imdb_nresults = 0;
          imdb_result = (fun url movie ->
              lprintf "Found %s: %s" movie url; lprint_newline ();
          );
        } in
      request_and_parse imdb_parse url record;
      record
  | _ -> assert false

let imdb_parse_movie (wtree : unit tree) page = 
  (try File.from_string "/tmp/imdb.html" page with _ -> ());  
  let director = try
      lprintf "Searching director_line"; lprint_newline ();      
      let found = Str.search_forward director_line page 0 in
      lprintf "director_line found"; lprint_newline ();
      Str.matched_group 1 page
    with _ -> 
        lprintf "director_line not found"; lprint_newline ();        
        "<unknown>"
  in

(*
  let genre = try
      let found = Str.search_forward genre_line page 0 in
      Str.matched_group 1 page
    with _ ->  "<unknown>"
  in
*)
  
  let rating = try
      lprintf "Searching rating_line"; lprint_newline ();
      let found = Str.search_forward rating_line page 0 in
      lprintf "rating_line found"; lprint_newline ();
      let note = Str.matched_group 2 page in
      let votes = Str.matched_group 3 page in
      Printf.sprintf "%s (%s votes)" note votes
    with _ -> 
        lprintf "rating_line not found"; lprint_newline ();
        "<unknown>"
  in
  let summary = try
      lprintf "Searching summary_line"; lprint_newline ();
      let found = Str.search_forward begin_summary_line page 0 in
      lprintf "summary_line found"; lprint_newline ();
      let abstract = Str.matched_group 1 page in
      
      let begin_pos = Str.match_end () in
      let found = Str.search_forward end_summary_line page 0 in
      let end_pos = Str.match_beginning () in
      let resume = String.sub page begin_pos (end_pos - begin_pos) in
      Printf.sprintf "%s \n %s" abstract resume
    
    with _ ->  
        lprintf "summary_line not found"; lprint_newline ();
        ""
  in
  let label = Printf.sprintf   "director %s\nrating %s\n%s" director rating
      summary in
  wtree#item () label

class results qe =
  let imdb_record = submit_imdb_search qe in
  let selection = ref (fun _ -> ()) in
  
  (object (self)
      inherit Gui_results_base.files ()
      
      val movies_table = Hashtbl.create 13
      
      method set_list_bg bg font =
        let style = wtree#misc#style#copy in
        style#set_bg [ (`NORMAL, bg)];
        style#set_base [ (`NORMAL, bg)];
        wtree#misc#set_style style;
        style#set_font font;
        color_tree wtree bg font

      method private insert url movie =
(*      lprintf "Insert %s : %s (%s)" artist album url; 
lprint_newline (); *)
        if not(Hashtbl.mem movies_table movie) then
          let item = new tree selection wtree movie in
          Hashtbl.add movies_table movie item;
          
          let url = Printf.sprintf "http://us.imdb.com%s" url in
          cached_or_on_expand item imdb_parse_movie referer url;
          self#set_list_bg (`NAME !!Gui_options.color_list_bg) (Gdk.Font.load_fontset !!Gui_options.font_list)
      
      method clear =
        List.iter wtree#remove wtree#children;
      
      method set_tb_style st =
        wtool1#set_style st;
        wtool2#set_style st;
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ())
        

    method download () = ()

    initializer
      
      (*
      ignore
        (wtool#insert_button 
           ~text: (gettext M.download)
           ~tooltip: (gettext M.download_selected_dir)
           ~icon: (Gui_options.pixmap M.o_xpm_download)#coerce
           ~callback: self#download
           ()
        ); *)
      imdb_record.imdb_result <- (fun url movie ->
          self#insert url movie 
      );

      let style = evbox#misc#style#copy in
      style#set_bg [ (`NORMAL, (`NAME "#494949"))];
      evbox#misc#set_style style;
        
      self#set_tb_style !!Gui_options.toolbars_style;
      self#set_list_bg (`NAME !!Gui_options.color_list_bg)
        (Gdk.Font.load_fontset !!Gui_options.font_list)
      
  end : url_results)

end

(***********************************************************************


                       ShareReactor Bot
  

***********************************************************************)

module ShareReactor = struct

    
let release_line = Str.regexp_case_fold
    "<a href=\"release.php\\?id=\\([^\"]*\\)\">\\([^<]*\\)</a>"
    
let download_line = Str.regexp_case_fold
    "<a href=\"download.php\\?id=\\([^\"]*\\)\">\\([^<]*\\)</a>"
    
let ed2k_line = Str.regexp_case_fold
    "<a href=\"\\(ed2k:[^\"]*\\)\">\\([^<]*\\)</a>"

    
type shr_record = {
    mutable shr_nresults : int;
    mutable shr_result : (string -> string ->unit);
  }

let referer = Some (Url.of_string "http://www.sharereactor.com/")
  
let categories = [
    
    "All", "0";
    "DVD Rips", "1" ;
    "Screeners", "2";
    "PC Games", "3" ;
    "Software", "4";
    "Anime", "6";
    "Series", "7";
    "Funstuff", "8";
    "Adult", "9";
    "Consoles", "11";
    "Books", "12";
    "XBOX", "14";
    "Hentai", "15";
    "PS2", "16";
    "Gay", "17";  
  ]
  
let query = 
  Q_AND [
    Q_KEYWORDS ("Title", "");
    Q_COMBO ("Category", "All", List.map fst categories);
    ]

let sharereactor_search_url qe = 
  match qe with
    Q_AND [Q_KEYWORDS (_ , title);
      Q_COMBO (_, category, _)] ->
      
      let category = List.assoc category categories in
      
      make_url referer "http://www.sharereactor.com/search.php"  [
        "search", title;
        "category", category;
        "submit", "Search";
      ] 
      
  | _ -> assert false
      
let sharereactor_release_url title = 
  make_url referer "http://www.sharereactor.com/release.php" [
      "id", title;
  ] 
  
let sharereactor_download_url title = 
  make_url referer "http://www.sharereactor.com/download.php"  [
    "id", title;
  ] 
  
let sharereactor_parse_download ((wtree: string tree), name) page =
  let has_found = ref false in
  try
    let rec iter start =
      lprintf "search %d/%d" start (String.length page);
      lprint_newline ();  
      let found = Str.search_forward ed2k_line page start in
      has_found := true;
      lprintf "found ed2k %d" found; lprint_newline ();  
      let url = Str.matched_group 1 page in
      let title = Str.matched_group 2 page in

      let title = wtree#subtree title in
      let ed2k = title#item url url in
      iter (found+1)
    in
    iter 0
  with _ -> 
      if not !has_found then begin
          lprintf "No ed2k link found !!!"; lprint_newline ();
        end

let sharereactor_parse_release (wtree : string tree) page =
  let has_found = ref false in
  try
    let rec iter start =
      lprintf "search %d/%d" start (String.length page);
      lprint_newline ();  
      let found = Str.search_forward download_line page start in
      has_found := true;
      lprintf "found download %d" found; lprint_newline ();  
      let url = Str.matched_group 1 page in
      let movie = Str.matched_group 2 page in

      let url = sharereactor_download_url url in
      request_and_parse sharereactor_parse_download url (wtree, movie);
      (*
      record.shr_result url movie;
      record.shr_nresults <- record.shr_nresults + 1;
if record.shr_nresults > 1000 then raise End_of_file;
  *)
      iter (found+1)
    in
    iter 0
  with _ -> 
      if not !has_found then begin
          lprintf "No ed2k link found !!!"; lprint_newline ();
        end

let sharereactor_parse record page =
  try
    let rec iter start =
(*      lprintf "search %d/%d" start (String.length page); 
lprint_newline ();  *)
      let found = Str.search_forward release_line page start in
(*      lprintf "found %d" found; lprint_newline ();  *)
      let url = Str.matched_group 1 page in
      let movie = Str.matched_group 2 page in
      record.shr_result url movie;
      record.shr_nresults <- record.shr_nresults + 1;
      if record.shr_nresults > 1000 then raise End_of_file;
      iter (found+1)
    in
    iter 0
  with _ -> ()

let submit_sharereactor_search qe = 
  let counter = ref 0 in
  let (url : Http_client.request) = sharereactor_search_url qe in
  let record = {
      shr_nresults = 0;
      shr_result = (fun url movie ->
          lprintf "Found %s: %s" movie url; lprint_newline ();
      );
    } in
  request_and_parse sharereactor_parse url record;
  record


class results qe =
  let sharereactor_record = submit_sharereactor_search qe in
  let selection = ref (fun _ -> ()) in
  (object (self)
      inherit Gui_results_base.files ()
      
      val movies_table = Hashtbl.create 13
      
      method set_list_bg bg font =
        let style = wtree#misc#style#copy in
        style#set_bg [ (`NORMAL, bg)];
        style#set_base [ (`NORMAL, bg)];
        style#set_font font;
        wtree#misc#set_style style;
        color_tree wtree bg font

      method private insert url movie =
(*      lprintf "Insert %s : %s (%s)" artist album url; 
lprint_newline (); *)
        if not(Hashtbl.mem movies_table movie) then
          let item = new tree selection wtree movie in
          Hashtbl.add movies_table movie item;
          
          let url = Printf.sprintf "http://www.sharereactor.com/release.php?id=%s" url in
          lprintf "URL expected: %s" url; lprint_newline ();
          
          cached_or_on_expand item sharereactor_parse_release referer url;
          self#set_list_bg (`NAME !!Gui_options.color_list_bg) (Gdk.Font.load_fontset !!Gui_options.font_list)
      
      method clear =
        List.iter wtree#remove wtree#children;
      
      method set_tb_style st =
        wtool2#set_style st;
        wtool1#set_style st;
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ())
      
      method download () = 
        !selection (fun ed2k -> 
            Gui_com.send (Command (Printf.sprintf "dllink %s" ed2k)))
      
      initializer
        Gui_misc.insert_buttons wtool1 wtool2
        ~text: (M.qT_lb_download_selected_dir)
        ~tooltip: (M.qT_ti_download_selected_dir)
        ~icon: (M.o_xpm_download_directory)
        ~callback: self#download
        ();
                
        sharereactor_record.shr_result <- (fun url movie ->
            self#insert url movie 
        );
        
        let style = evbox#misc#style#copy in
        style#set_bg [ (`NORMAL, (`NAME "#494949"))];
        evbox#misc#set_style style;

        self#set_tb_style !!Gui_options.toolbars_style;
        self#set_list_bg (`NAME !!Gui_options.color_list_bg)
          (Gdk.Font.load_fontset !!Gui_options.font_list)
        
 end :  url_results)

end


(***********************************************************************


                       Jigle Bot
  

***********************************************************************)

module Jigle = struct
    
let release_line = Str.regexp_case_fold
    "<a href=\"\\(search[^\"]*\\)\">\\[details\\]</a><br>"
    
let end_tag = Str.regexp_case_fold
    "\\(<br>\\)\\|\\(<p>\\)\\|\\(</ol>\\)"
      
let ed2k_line = Str.regexp_case_fold
    "href=\"\\(ed2k:[^\"]*\\)\">"

    
type jigle_record = {
    mutable jigle_nresults : int;
    mutable jigle_result : (string -> string ->unit);
  }

let referer = Some (Url.of_string "http://www.jigle.com/")

(*
  http://www.jigle.com/search?p=terminator&ma=10&d=1&a=1&l=50&t=1&x=&sl=123&su=456&v=0
  *)
let query = Q_AND [
    Q_KEYWORDS ("Title", "");
    Q_MINSIZE ("Min Size", "1");
    Q_MAXSIZE ("Max Size", "");
    Q_COMBO ("Number of results", "10", ["10"; "25"; "50"; "75"; "100"]);
    Q_COMBO ("Sort by", "Availability", ["Availability"; "Size"]);
  ]

let jigle_search_url qe = 
  match qe with
    Q_AND [
      Q_KEYWORDS (_ , words);
      Q_MINSIZE (_, min_size);
      Q_MAXSIZE (_, max_size);
      Q_COMBO (_, min_results, _);
      Q_COMBO (_, sort, _);
    ] ->
      
      make_url referer "http://www.jigle.com/search"  [
        "p", words;
        "ma", "1"; (* min avail *)
        "d", "1";
        "a", (if sort = "Availability" then "0" else "1");
        "l", min_results;
        "t", "0";
        "x", "";
        "sl", min_size;
        "su", max_size;
        "v", "0";
      ]
  | _ -> assert false
      
let jigle_release_url title = 
  make_url referer "http://www.jigle.com/release.php" [
      "id", title;
  ] 
  
let jigle_download_url title = 
  make_url referer "http://www.jigle.com/download.php"  [
    "id", title;
  ] 
  
let jigle_parse_download (wtree: string tree) page =
  let has_found = ref false in
  try
    let rec iter start =
      lprintf "search %d/%d" start (String.length page);
      lprint_newline ();  
      let found = Str.search_forward ed2k_line page start in
      has_found := true;
      lprintf "found ed2k %d" found; lprint_newline ();  
      let url = Str.matched_group 1 page in
      let ed2k = wtree#item url url in

      iter (found+1)
    in
    iter 0
  with _ -> 
      if not !has_found then begin
          lprintf "No ed2k link found !!!"; lprint_newline ();
        end

let jigle_parse record page =
  try
    let rec iter start =
(*      lprintf "search %d/%d" start (String.length page); 
lprint_newline ();  *)
      let found = Str.search_forward release_line page start in
      let end_pos = Str.match_end () in
(*      lprintf "found %d" found; lprint_newline ();  *)
      let url = Str.matched_group 1 page in
      
      let found = Str.search_forward end_tag page end_pos in
      let begin_pos = Str.match_beginning () in
      
      let movie = String.sub page end_pos (begin_pos - end_pos) in
      record.jigle_result url movie;
      record.jigle_nresults <- record.jigle_nresults + 1;
      if record.jigle_nresults > 1000 then raise End_of_file;
      iter (found+1)
    in
    iter 0
  with _ -> ()
      
let submit_jigle_search qe = 
  let counter = ref 0 in
  let (url : Http_client.request) = jigle_search_url qe in
  let record = {
      jigle_nresults = 0;
      jigle_result = (fun url movie ->
          lprintf "Found %s: %s" movie url; lprint_newline ();
      );
    } in
  request_and_parse jigle_parse url record;
  record



class results qe =
  let jigle_record = submit_jigle_search qe in
  let selection = ref (fun _ -> ()) in
  (object (self)
      inherit Gui_results_base.files ()
      
      val movies_table = Hashtbl.create 13
      
      method set_list_bg bg font =
        let style = wtree#misc#style#copy in
        style#set_bg [ (`NORMAL, bg)];
        style#set_base [ (`NORMAL, bg)];
        style#set_font font;
        wtree#misc#set_style style;
        color_tree wtree bg font


      method private insert url movie =
(*      lprintf "Insert %s : %s (%s)" artist album url; 
lprint_newline (); *)
        if not(Hashtbl.mem movies_table movie) then
          let item = new tree selection wtree movie in
          Hashtbl.add movies_table movie item;
          
          let url = Printf.sprintf "http://www.jigle.com/%s" url in
          lprintf "URL expected: %s" url; lprint_newline ();
          
          cached_or_on_expand item jigle_parse_download  referer url;
          self#set_list_bg (`NAME !!Gui_options.color_list_bg) (Gdk.Font.load_fontset !!Gui_options.font_list)
      
      method clear =
        List.iter wtree#remove wtree#children;
      
      method set_tb_style st =
        wtool2#set_style st;
        wtool1#set_style st;
        if Options.(!!) Gui_options.mini_toolbars then
          (wtool1#misc#hide (); wtool2#misc#show ()) else
          (wtool2#misc#hide (); wtool1#misc#show ());
      
      method download () = 
        !selection (fun ed2k -> 
            Gui_com.send (Command (Printf.sprintf "dllink %s" ed2k)))
      
      initializer
        Gui_misc.insert_buttons wtool1 wtool2
          ~text: (M.qT_lb_download_selected_dir)
          ~tooltip: (M.qT_ti_download_selected_dir)
          ~icon: (M.o_xpm_download_directory)
          ~callback: self#download
          ();
        
        let style = evbox#misc#style#copy in
        style#set_bg [ (`NORMAL, (`NAME "#494949"))];
        evbox#misc#set_style style;

        self#set_tb_style !!Gui_options.toolbars_style;
        self#set_list_bg (`NAME !!Gui_options.color_list_bg)
          (Gdk.Font.load_fontset !!Gui_options.font_list);

        jigle_record.jigle_result <- (fun url movie ->
          self#insert url movie 
      );


      
end : url_results)

end
