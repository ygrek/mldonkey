(***********************************************************************)
(*                               MLChat                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The application class. *)

open Chat_types
open Chat_proto

module M = Chat_messages
module C = Configwin

let safe_int_param h label f v =
  C.string ~help: h ~f: (fun s -> try f (int_of_string s) with _ -> ())
    label (string_of_int v)

let input_people () =
  let id = ref "" in
  let p_id = C.string ~help: M.h_id ~f: (fun s -> id := s) M.id !id in
  let host =  ref "" in
  let p_host = C.string ~f: (fun s -> host := s) M.host !host in
  let port = ref 5036 in
  let p_port = safe_int_param M.h_port M.port 
      (fun n -> port := n)
      !port
  in
  match C.simple_get M.people [p_id ; p_host ; p_port] with
    C.Return_cancel -> None
  | C.Return_apply | C.Return_ok -> 
      Some  (!id, !host, !port)

(** Return true if conf was modified. In this case,
   the conf has already been saved.*)
let edit_conf conf =
  let p_id = C.string 
      ~help: M.h_id
      ~f: conf#set_id M.id conf#id 
  in
  let p_port = safe_int_param M.h_port M.port conf#set_port conf#port in
  let p_timeout = safe_int_param M.h_timeout M.h_timeout
      conf#set_timeout conf#timeout
  in
  let p_popup = C.bool ~help: M.h_popup_all
      ~f: conf#set_popup_all
      M.popup_all
      conf#popup_all
  in

  let p_col_connected = C.color
      ~help: M.h_color_connected 
      ~f: conf#set_color_connected
      M.h_color_connected conf#color_connected
  in
  let p_col_connected_temp = C.color
      ~help: M.h_color_connected_temp
      ~f: conf#set_color_connected_temp
      M.h_color_connected_temp conf#color_connected_temp
  in
  let p_col_not_connected = C.color
      ~help: M.h_color_not_connected
      ~f: conf#set_color_not_connected
      M.h_color_not_connected conf#color_not_connected
  in
  let p_col_myself = C.color
      ~help: M.h_color_myself
      ~f: conf#set_color_myself
      M.h_color_myself conf#color_myself
  in
  let add_people () =
    match input_people () with
      None -> []
    | Some p -> [p]
  in
  let p_people = C.list ~f: conf#set_people
      ~add: add_people
      ~titles: [M.id ; M.host ; M.port]
      M.people 
      (fun (i,h,p) -> [i ; h ; string_of_int p])
      conf#people
  in
  let structure = [
    C.Section (M.connection, 
	     [p_id ; p_port ; p_timeout ; p_popup]) ;
    C.Section (M.colors,
	     [ p_col_connected ;
	       p_col_connected_temp ;
	       p_col_not_connected ;
	       p_col_myself ] ) ;
    C.Section (M.people,
	       [ p_people ]) ;
  ] 
  in
  match C.get M.options structure with
    C.Return_cancel -> false
  | C.Return_apply | C.Return_ok ->
      conf#save;
      true

class app 
    ?(no_quit=false)
    pred conf com =
  let data = new Chat_data.data pred conf com in

  object (self)
    val mutable working = false

    inherit Chat_gui.gui no_quit data as gui

    method handle_paquet p =
      let ((version,id,(host,port)), iddest, proto) = p in
      let reply = 
	if version <> Chat_proto.version then
	  None
	else
	  (
	   match proto with
	   | HelloOk ->
	       data#set_connected id host port;
	       gui#update;
	       None

	   | Hello ->
	       data#set_connected id host port;
	       gui#update;
	       Some HelloOk
		  
	   | Byebye ->
	       data#set_not_connected id host port;
	       gui#update;
	       None
		 
	   | Message mes ->
	       let l = data#people in
	       data#set_connected id host port;
	       if l <> data#people then gui#update;
	       let show = 
		 conf#popup_all or 
		 (List.exists (pred (id,host,port)) conf#people)
	       in
	       let dial = Chat_gui.get_dialog ~show data id host port in
	       dial#handle_message mes;
	       None
	   | AddOpen (i, (h, p)) ->
	       prerr_endline (Printf.sprintf "received AddOpen i=%s h=%s p=%d" i h p);
	       self#handle_paquet ((version,i,(h,p)), iddest, Message "");
	       None
	  )
      in
      match reply with
	None -> ()
      |	Some r ->
	  try data#com#send id (host,port) r
	  with Failure s -> prerr_endline s

    method accept =
      match data#com#receive with
	None -> ()
      | Some p -> self#handle_paquet p

    method work () =
      if working then false
      else
	(
	 self#accept;
	 ignore (GMain.Timeout.add ~ms: conf#timeout ~callback: self#work);
	 working <- false;
	 false
	)

    method say_hello (id, host, port, state, temp) =
      if temp then ()
      else
	try data#com#send id (host, port) Hello 
	with Failure s ->
	  data#set_not_connected id host port;
	  prerr_endline s

    method say_byebye (id, host, port, state, temp) =
	try data#com#send id (host, port) Byebye
	with _ -> ()

    method edit_conf =
      if edit_conf conf then
	(
	 data#update_people ;
	 List.iter self#say_hello data#people;
	 gui#update
	)
      else
	()

    method set_not_temp_selected =
      List.iter
	(fun (i,h,p,_,t) ->
	  if t then data#add_people i h p
	  else ())
	selected_people;
      gui#update

    initializer
      ignore (itemOptions#connect#activate (fun () -> self#edit_conf));
      ignore (itemSetNotTemp#connect#activate
		(fun () -> self#set_not_temp_selected));

      ignore (GMain.Timeout.add ~ms: conf#timeout ~callback: self#work) ;
      List.iter self#say_hello data#people;
      gui#update;

      ignore (box#connect#destroy 
		(fun () -> List.iter self#say_byebye data#people))

  end
