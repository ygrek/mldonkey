

{

type token =
| EOF | BEGIN_TABLE | END_TABLE | BEGIN_ROW | END_ROW | BEGIN_COL | END_COL
| STRING of string | SPACE

}

rule token = parse
    [ ' ' '\t' '\n' '\r' ]+ { SPACE }
  | '<' { get_tag lexbuf }
  | [ ^ '<' ' ' '\n' '\r' ]+ { STRING (Lexing.lexeme lexbuf) }
  | eof { EOF }
    
and get_tag = parse
  "table"    { end_tag lexbuf BEGIN_TABLE }
  | "/table" { end_tag lexbuf END_TABLE }
  | "tr"     { end_tag lexbuf BEGIN_ROW   }
  | "/tr"    { end_tag lexbuf END_ROW }
  | "td"     { end_tag lexbuf BEGIN_COL }
  | "/td"    { end_tag lexbuf END_COL }
  | _        { end_tag lexbuf SPACE }

and end_tag = parse 
  | '>'      { fun token -> token }
  | [ ' ' '\t' '\n' '\r' ]+ { end_tag lexbuf }
  | '"'      { quoted lexbuf; end_tag lexbuf }
  | [ ^ '"' '>' ] + { end_tag lexbuf }
    
and quoted = parse
  | '"'      { () }
  | _        { quoted lexbuf }
    
{

let lower s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
      'A' .. 'Z' -> s.[i] <- Char.lowercase s.[i]
    | _ -> ()
  done

let translate s =
  lower s;
  let lexbuf = Lexing.from_string s in
  try
    let buf = Buffer.create 100 in
    let rec iter can_space spaced list =
      match token lexbuf with
        EOF ->
          let s = Buffer.contents buf in
          let list = 
            if String.length s = 0 then list
            else (STRING s) :: list         
          in
          List.rev list
      | STRING s -> 
          if spaced then Buffer.add_char buf ' ';
          Buffer.add_string buf s; iter true false list
      | SPACE -> iter can_space can_space list
      | tok -> 
          let s = Buffer.contents buf in
          Buffer.clear buf;
          if String.length s = 0 then
            iter false false (tok :: list)
          else
            iter false false (tok :: (STRING s) :: list)
    in
    iter false false []
  with e ->
      Printf.printf "Html2table: Exception %s at pos %d"
        (Printexc2.to_string e)
      (Lexing.lexeme_start lexbuf);
      print_newline ();
      []

let rec spaces n =
  if n>0 then (print_char ' '; spaces (n-1))

    (*
let translate s =
  let list = translate s in
  let rec iter pos tags list =
    match list with
      [] ->
        begin
          match tags with
            [] -> ()
          | tag :: _ ->
              failwith (Printf.sprintf "Unterminated %s" (
                match tag with
                  BEGIN_TABLE -> "TABLE"
                | BEGIN_ROW -> "ROW"
                | BEGIN_COL -> "COL"
                | _ -> "???"))
        end
                  
    | tag :: tail ->
        match tag with
        | BEGIN_TABLE ->
            spaces pos;Printf.printf "TABLE"; print_newline (); 
            iter (pos+1) (BEGIN_TABLE :: tags) tail
        | BEGIN_ROW ->
            spaces pos;Printf.printf "ROW"; print_newline (); 
            iter (pos+1) (BEGIN_ROW :: tags) tail
        | BEGIN_COL ->
            spaces pos;Printf.printf "COL"; print_newline (); 
            iter (pos+1) (BEGIN_COL :: tags) tail
        | STRING s ->
            spaces pos;Printf.printf "[%s]" s; print_newline (); 
            iter pos tags tail
        | END_COL | END_TABLE | END_ROW ->
            begin
              match tags, tag with
              | BEGIN_COL :: tags, END_COL 
              | BEGIN_TABLE :: tags, END_TABLE 
              | BEGIN_ROW :: tags, END_ROW ->
                  iter (pos-1) tags tail
              | _ ->
                  failwith "Unmatched tag";
            end
        | _ -> failwith "assertion false"
  in
  iter 0 [] list;
  list
    *)

let translate s =
  let list = translate s in
  let rec iter list servers =
    match list with
      BEGIN_ROW :: 
      BEGIN_COL :: STRING server_name :: END_COL ::
      BEGIN_COL :: STRING server_ip :: END_COL ::
      BEGIN_COL :: STRING server_port :: END_COL ::
      BEGIN_COL :: STRING server_net :: END_COL :: tail ->
        begin
          try
            let ip = Ip.of_string server_ip in
            if not (Ip.valid ip) then raise Not_found;
            let port = int_of_string server_port in
(*            Printf.printf "SERVER %s %s:%d FROM %s"
              server_name (Ip.to_string ip) port server_net;
            print_newline (); *)
            iter tail ((server_name, ip, port, server_net) :: servers)
          with _ -> iter tail servers
        end
    | [] -> List.rev servers
    | _ :: tail -> iter tail servers      
  in
  iter list []

open TcpBufferedSocket
  
let load_servers_list url f =
(*  Printf.printf "QUERY URL %s" url; print_newline (); *)
  CommonGlobals.mldonkey_wget url (fun filename ->
    f (translate (File.to_string filename)))


  
}