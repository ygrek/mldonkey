(* Copyright 2006 MLDonkey project *)
(*
    This client is part of mldonkey.

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

open Printf2
open Md4
open Options
open CommonTypes
open CommonOptions

let log_prefix = "[cUd]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

(*************************************************************************)
(*                         TYPES                                         *)
(*************************************************************************)

type userdb = {
    user_name : string;
    user_pass : Md4.t;
    user_groups : string list;
    user_default_group : string option;
    user_mail : string;
    user_commit_dir : string;
    user_max_concurrent_downloads : int;
  }

type groupdb = {
    group_name : string;
    group_mail : string;
    group_admin : bool;
  }

exception User_has_downloads of int

(*************************************************************************)
(*                         DEFAULTS                                      *)
(*************************************************************************)

let users_ini = create_options_file "users.ini"

let users2_section = file_section users_ini ["Users"] "User accounts on the core (new format)"
let users_section = file_section users_ini ["Users"] "User accounts on the core (old format)"

let blank_password = Md4.string ""

let admin_user = "admin"
let system_user_default_group = "mldonkey"

(*************************************************************************)
(*                         GroupOption                                   *)
(*************************************************************************)

module GroupOption = struct

    let value_to_group v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let gname =
            try
              get_value "group_name" value_to_string
            with _ -> system_user_default_group
          in
          let gmail =
	    try
              get_value "group_mail" value_to_string
            with _ -> ""
          in
          let gadmin =
	    try
              get_value "group_admin" value_to_bool
            with _ -> true
          in
	  { group_name = gname;
	    group_mail = gmail;
	    group_admin = gadmin;
          }

      | _ -> failwith "Options: not a valid group"

    let group_to_value group =
      Options.Module [
        "group_name", string_to_value group.group_name;
        "group_mail", string_to_value group.group_mail;
        "group_admin", bool_to_value group.group_admin;
      ]

    let t = define_option_class "Groups" value_to_group group_to_value

  end

let grouplist = define_option users2_section ["groups"]
  "The groups that are defined on this core.

group_admin           = Are members of this group MLDonkey admins?
                        Only members of this group can change settings and see uploads.
"
    (list_option GroupOption.t)
    [ 
      { group_name = system_user_default_group;
        group_mail = "";
	group_admin = true;
      };
    ]

(*************************************************************************)
(*                         Group database functions                      *)
(*************************************************************************)

let user2_group_iter f =
  List.iter f !!grouplist

let user2_group_add name ?(mail = "") ?(admin = true) () =
  let new_group = {
      group_name = name;
      group_mail = mail;
      group_admin = admin;
  } in
  grouplist =:= new_group :: List.filter (fun g -> g.group_name <> name) !!grouplist

let user2_group_remove name =
  grouplist =:= List.filter (fun g -> g.group_name <> name) !!grouplist

let user2_group_find group =
  List.find (fun g -> g.group_name = group) !!grouplist

let user2_group_exists group =
  try
    ignore (user2_group_find group);
    true
  with Not_found -> false

let user2_group_exists_option group =
  match group with
    None -> true
  | Some group -> user2_group_exists group

(*************************************************************************)
(*                         UserOption                                    *)
(*************************************************************************)

module UserOption = struct

    let value_to_user v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
	  let value_to_md4 v = Md4.of_string (value_to_string v) in
          let uname =
            try
              get_value "user_name" value_to_string
            with _ -> failwith "empty username"
          in          
          let upass =
            try
              get_value "user_pass" value_to_md4
            with _ -> blank_password
          in          
          let umail =
	    try
              get_value "user_mail" value_to_string
            with _ -> ""
          in
          let ucdir =
	    try
              get_value "user_commit_dir" value_to_string
            with _ -> ""
          in
          let umaxdl =
	    try
              get_value "user_max_concurrent_downloads" value_to_int
            with _ -> 0
          in
          let ugroups =
	    try
              let ugl = get_value "user_groups" (value_to_list value_to_string) in
	        List.filter (fun g -> user2_group_exists g) ugl
            with _ -> [system_user_default_group]
          in
          let udgroup =
	    try
              match get_value "user_default_group" stringvalue_to_option with
		None -> None
	      | Some udg ->
		  if user2_group_exists udg then
		    if List.mem udg ugroups then
		      Some udg
		    else begin
		      lprintf_nl "User %s is not member of group %s, setting user_default_group to None" uname udg;
		      None
		    end
		  else begin
		    lprintf_nl "user_default_group %s of user %s does not exist, setting to None" udg uname;
		    None
		  end
            with _ -> Some system_user_default_group
          in
	  { user_name = uname;
	    user_pass = upass;
	    user_groups = ugroups;
	    user_default_group = udgroup;
	    user_mail = umail; 
	    user_commit_dir = ucdir; 
	    user_max_concurrent_downloads = umaxdl; 
	  }

      | _ -> failwith "Options: not a valid user"

    let user_to_value user =
      Options.Module [
        "user_name", string_to_value user.user_name;
        "user_pass", string_to_value (Md4.to_string user.user_pass);
        "user_groups", list_to_value string_to_value user.user_groups;
        "user_default_group", option_to_stringvalue user.user_default_group;
        "user_mail", string_to_value user.user_mail;
        "user_commit_dir", string_to_value user.user_commit_dir;
        "user_max_concurrent_downloads", int_to_value user.user_max_concurrent_downloads;
      ]

    let t = define_option_class "Users" value_to_user user_to_value

  end

let userlist = define_option users2_section ["users2"]
  "The users that are defined on this core. The default user is
called 'admin', and uses an empty password. To create new users,
login as admin in mldonkey, and use the 'useradd' command.

user_groups                   = Files belonging to one of these groups can be seen by the user.
user_default_group            = New downloads by this user will belong to this group.
user_commit_dir               = Commit files to <incoming>/<user_commit_dir>
user_max_concurrent_downloads = Maximum number of downloads allowed, 0 = unlimited
"
    (list_option UserOption.t)
    [ { user_name = admin_user;
        user_pass = blank_password;
        user_groups = [system_user_default_group];
        user_default_group = Some system_user_default_group;
        user_mail = "";
        user_commit_dir = "";
        user_max_concurrent_downloads = 0;
    } ]

let users = define_option users_section ["users"]
  "Depreciated option, kept for compatibility reasons - used by MLDonkey < 2.7.5"
    (list_option (tuple2_option (string_option, Md4.option)))
    [admin_user, blank_password]

(*************************************************************************)
(*                         User database functions                       *)
(*************************************************************************)

let user2_user_iter f =
  List.iter f !!userlist

let user2_user_add name pass ?(groups = [system_user_default_group])
			     ?(default_group = Some system_user_default_group)
			     ?(mail = "") ?(commit_dir = "") ?(max_dl = 0) () =
  let groups =
    let l =
      (List.filter (fun g -> user2_group_exists g) groups)
    in
    if l = [] then
      [system_user_default_group]
    else l
  in
  let default_group =
    match default_group with
    | None -> default_group
    | Some group -> if not (user2_group_exists group) then None else Some group
  in
  let new_user = {
      user_name = name;
      user_pass = pass;
      user_groups = groups;
      user_default_group = default_group;
      user_mail = mail;
      user_commit_dir = commit_dir;
      user_max_concurrent_downloads = max_dl;
  } in
  userlist =:= new_user :: List.filter (fun u -> u.user_name <> name) !!userlist

let user2_user_remove user =
  userlist =:= List.filter (fun u -> u.user_name <> user) !!userlist

let user2_user_find user =
  List.find (fun u -> u.user_name = user) !!userlist

let user2_user_exist user =
  try
    ignore (user2_user_find user);
    true
  with Not_found -> false

(*************************************************************************)
(*               User database functions / passwords                     *)
(*************************************************************************)

let user2_user_password user =
  (user2_user_find user).user_pass

let user2_user_set_password user pass_string =
  let new_user = {
    (user2_user_find user) with
    user_pass = Md4.string pass_string
  } in
  userlist =:= new_user :: List.filter (fun u -> u.user_name <> user) !!userlist

let valid_password user pass =
  try
    user2_user_password user = Md4.string pass
  with Not_found -> false

let empty_password user =
  valid_password user ""

(*************************************************************************)
(*               User database functions / mail                          *)
(*************************************************************************)

let user2_user_mail user =
  (user2_user_find user).user_mail

let user2_print_user_mail user =
  try
    user2_user_mail user
  with Not_found -> ""

let user2_user_set_mail user mail =
  let new_user = {
    (user2_user_find user) with
    user_mail = mail
  } in
  userlist =:= new_user :: List.filter (fun u -> u.user_name <> user) !!userlist

(*************************************************************************)
(*          User database functions / concurrent downloads               *)
(*************************************************************************)

let user2_user_dls user =
  (user2_user_find user).user_max_concurrent_downloads

let user2_print_user_dls user =
  try
    let dls = user2_user_dls user in
      if dls = 0 then "unlimited"
      else (Printf.sprintf "%d" dls)
  with Not_found -> "unknown"

let user2_user_set_dls user dls =
  let new_user = {
    (user2_user_find user) with
    user_max_concurrent_downloads = dls
  } in
  userlist =:= new_user :: List.filter (fun u -> u.user_name <> user) !!userlist

(*************************************************************************)
(*                 User database functions / commit dir                  *)
(*************************************************************************)

let user2_user_commit_dir user =
  (user2_user_find user).user_commit_dir

let user2_print_user_commit_dir user =
  try
    user2_user_commit_dir user
  with Not_found -> ""

let user2_user_set_commit_dir user dir =
  let new_user = {
    (user2_user_find user) with
    user_commit_dir = dir
  } in
  userlist =:= new_user :: List.filter (fun u -> u.user_name <> user) !!userlist

(*************************************************************************)
(*                         User/Group database functions                 *)
(*************************************************************************)

let user2_user_groups user =
  try
    (user2_user_find user).user_groups
  with Not_found -> failwith (Printf.sprintf "User %s does not exist" user)

let user2_user_groups_safe user =
  try
    (user2_user_find user).user_groups
  with Not_found -> []

let user2_user_groups_safe_default user =
  try
    (user2_user_find user).user_groups
  with Not_found -> [system_user_default_group]

let user2_user_groups_iter user f =
  List.iter f (user2_user_groups_safe user)

let user2_print_user_groups user =
  try
    let u = user2_user_find user in
      String.concat "," u.user_groups
  with Not_found -> ""

let user2_user_default_group user =
  try
    (user2_user_find user).user_default_group
  with Not_found -> None

let user2_print_user_default_group user =
  try
    let u = user2_user_find user in
      match u.user_default_group with
        None -> "none"
      | Some group -> group
  with Not_found -> failwith (Printf.sprintf "User %s does not exist" user)

let user2_user_add_group user group =
  if not (user2_group_exists group) then
    user2_group_add group ();
  try
    let u = user2_user_find user in
      user2_user_add
        u.user_name
	u.user_pass
	?groups:(Some (List.append u.user_groups [group]))
	?mail:(Some u.user_mail)
  with Not_found -> failwith (Printf.sprintf "User %s does not exist" user)

let user2_user_remove_group user group =
  try
    let u = user2_user_find user in
      user2_user_add
        u.user_name
	u.user_pass
	?groups:(Some (List.filter (fun g -> not (g = group)) u.user_groups))
	?mail:(Some u.user_mail)
  with Not_found -> failwith (Printf.sprintf "User %s does not exist" user)

let user2_user_is_group_member user group =
  List.mem group (user2_user_groups_safe user)

(*************************************************************************)
(*                         Access rights                                 *)
(*************************************************************************)

let user2_is_admin user =
  user = admin_user ||
  List.exists (fun groupname ->
    try
      (user2_group_find groupname).group_admin
    with Not_found -> false)
  (user2_user_groups_safe user)

let user2_can_view_uploads user =
  user2_is_admin user

let user2_can_view_file gui_user file_owner file_group =
  user2_is_admin gui_user || gui_user = file_owner ||
  (match file_group with
  | None -> false
  | Some file_group -> user2_user_is_group_member gui_user file_group)

let print_command_result o buf result =
  if use_html_mods o then
    html_mods_table_one_row buf "serversTable" "servers" [
      ("", "srh", result); ]
  else
    Printf.bprintf buf "%s" result

(*************************************************************************)
(*                         Hooks                                         *)
(*************************************************************************)

let _ =
  set_after_load_hook users_ini (fun _ ->
    List.iter (fun (user,pass) ->
      if not (user2_user_exist user) then begin
	user2_user_add user pass ();
        lprintf_nl "converted user %s to new format" user
      end) !!users;
(* clean !!users to avoid saving users more than once *)
    users =:= [];
    if not (user2_user_exist admin_user) then
      begin
	user2_user_add admin_user blank_password ();
	lprintf_nl "SECURITY INFO: user 'admin' has to be present, creating with empty password!..."
      end
  );

(* This code provides backward-compatibility for older MLDonkey clients *)
(* reading new user db and copying the values into old user db !!users *)
  set_before_save_hook users_ini (fun _ ->
    user2_user_iter (fun user ->
	users =:= (user.user_name, (user2_user_password user.user_name)) :: !!users
    )
  );
(* clean !!users to avoid saving users more than once *)
  set_after_save_hook users_ini (fun _ -> users =:= [])
