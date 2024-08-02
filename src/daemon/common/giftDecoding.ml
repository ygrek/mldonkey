open Printf2

  
open CommonTypes
open GuiProto
open GuiTypes
open TcpBufferedSocket

let gui_cut_messages f sock nread =
  let b = buf sock in
  try
    let rec iter pos len =
      if len = 0 then raise Not_found;
      if Bytes.get b.buf pos = ';' && (
          pos = b.pos || 
          (pos > b.pos && Bytes.get b.buf (pos-1) <> '\\')) then begin
          let len = pos - b.pos+1 in
          let s = Bytes.sub_string b.buf b.pos len in
          buf_used b len;
          f s;
          iter b.pos b.len
        end
      else
        iter (pos+1) (len-1)
    in
    iter b.pos b.len
  with Not_found -> ()

let print_indent indent =
  for i = 0 to indent do
    lprintf " "
  done
      
let rec gift_print indent m =
  match m with
    GiftCommand (command, arg, keys) ->
      begin
        print_indent indent;
        lprintf "%s" command;
        begin
          match arg with
            None -> ()
          | Some arg ->
              lprintf "(%s)" arg
        end;
        match keys with
          [] -> lprintf "\n";
        | _ -> 
            lprintf "  {\n";
            List.iter (fun key ->
                gift_print (indent+2) key
            ) keys;
            print_indent indent;
            lprintf "  }\n"
      end

let rec find_key name args =
  match args with
    GiftCommand(n, Some arg, _ ) :: _ when n = name ->
      arg
  | _ :: tail -> find_key name tail
  | _ -> raise Not_found
      
let from_gui gui (s : string) = 
  try
    let lexbuf = Lexing.from_string s in
    let m = GiftParser.main GiftLexer.lexer lexbuf in
    gift_print 2 m;
    let GiftCommand(command, opt_arg, args) = m in
    match command, opt_arg with
      "attach", _ ->
        let profile = try find_key "profile" args with _ -> "<no profile>" in
        let version = find_key "version" args in
        let client = find_key "client" args in
        GiftAttach (profile, version, client)
    | "stats", _  ->
        GiftStats
    | "search", Some num ->
        Search_query {
          search_num = int_of_string num;
          search_query = Q_KEYWORDS("", find_key "query" args);
          search_max_hits = 2000;
          search_type = RemoteSearch;
          search_network = 0;
        }
    | "addsource", _ ->
        let save = find_key "save" args in
        let hash = find_key "hash" args in
        Download_query ([save], int_of_string hash, false)
        
    | "transfer", Some num ->
        let _, file_num = 
          Hashtbl.find gui.gui_identifiers_rev (int_of_string num)
        in
        let action = find_key "action" args in
        begin
          match action with
            "cancel" -> RemoveDownload_query file_num
          | "pause" -> SwitchDownload (file_num, false)
          | _ -> SwitchDownload (file_num, true)
        end
        
    | _ -> failwith "Unknown Message"
  with e ->
      lprintf "Error %s while parsing giFT command: \n%s\n"
        (Printexc2.to_string e)
      (String.escaped s);
      raise Not_found
  
