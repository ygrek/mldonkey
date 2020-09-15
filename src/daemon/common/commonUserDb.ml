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
(*                         DEFAULTS                                      *)
(*************************************************************************)

let users_ini = create_options_file "users.ini"

let users2_section = file_section users_ini ["Users"] "User accounts on the core (new format)"
let users_section = file_section users_ini ["Users"] "User accounts on the core (old format)"

let admin_group_name = "mldonkey"

let blank_password = Md4.string ""

let admin_user_name = "admin"

(*************************************************************************)
(*                         GroupOption                                   *)
(*************************************************************************)

module GroupOption = struct

    let value_to_group v =
      match v with
      | Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let gname =
            try
              get_value "group_name" value_to_string
            with _ -> admin_group_name
          in
          let gadmin =
            try
              get_value "group_admin" value_to_bool
            with _ -> true
          in
          { group_name = gname;
            group_admin = gadmin;
          }

      | _ -> failwith "Options: not a valid group"

    let group_to_value group =
      Options.Module [
        "group_name", string_to_value group.group_name;
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
    [{
      group_name = admin_group_name;
      group_admin = true;
    }]

(*************************************************************************)
(*                         Group database functions                      *)
(*************************************************************************)

let user2_groups_iter f =
  List.iter f ((List.sort (fun g1 g2 -> compare g1.group_name g2.group_name)) !!grouplist)

let update_group name new_group =
  let other_groups = List.filter (fun g -> g.group_name <> name) !!grouplist in
  grouplist =:=
      match new_group with
      | None -> other_groups
      | Some new_group -> new_group :: other_groups

let user2_group_add name admin =
  let new_group = {
      group_name = name;
      group_admin = admin;
  } in
  update_group name (Some new_group)

let user2_group_remove group =
  update_group group.group_name None

let user2_group_find group =
  List.find (fun g -> g.group_name = group) !!grouplist

let user2_group_exists group =
  List.exists (fun g -> g.group_name = group) !!grouplist

let user2_default_group_matches_group dgroup group =
  match dgroup with
    None -> false
  | Some g -> group = g

let user2_group_admin group admin =
  group.group_admin <- admin

let admin_group () =
  user2_group_find admin_group_name

(*************************************************************************)
(*                         UserOption                                    *)
(*************************************************************************)

let default_admin_user = {
  user_name = admin_user_name;
  user_pass = blank_password;
  user_groups = [admin_group ()];
  user_default_group = Some (admin_group ());
  user_mail = "";
  user_commit_dir = "";
  user_max_concurrent_downloads = 0;
  }

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
              let ugl2 = ref ([]: groupdb list) in
              List.iter (fun ug ->
                try
                  let usergroup = user2_group_find ug in
                  if not (List.mem usergroup !ugl2) then
                    ugl2 := !ugl2 @ [usergroup]
                  else
                    lprintf_nl "Removing duplicate group %s from user %s" ug uname
                with Not_found ->
                  lprintf_nl "Removing non-existing group %s from user %s" ug uname
              ) ugl;
              !ugl2
            with Not_found -> [admin_group ()]
          in
          let udgroup =
            try
              match get_value "user_default_group" value_to_stringoption with
                None -> None
              | Some udg ->
                  begin try
                    let g = user2_group_find udg in
                    if List.mem g ugroups then
                      Some g
                    else begin
                      lprintf_nl "User %s is not member of group %s, setting user_default_group to None" uname udg;
                      None
                    end
                  with Not_found ->
                    lprintf_nl "user_default_group %s of user %s does not exist, setting to None" udg uname;
                    None
                  end
            with Not_found -> Some (admin_group ())
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
        "user_groups", list_to_value (fun v -> string_to_value v.group_name) user.user_groups;
        "user_default_group", stringoption_to_value (match user.user_default_group with Some g -> Some g.group_name | None -> None);
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
user_mail                     = Address used to sent confirmation mails after comitting a download
user_max_concurrent_downloads = Maximum number of downloads allowed, 0 = unlimited
"
    (list_option UserOption.t) [default_admin_user]


let users = define_option users_section ["users"]
  "Depreciated option, kept for compatibility reasons - used by MLDonkey < 2.7.5"
    (list_option (tuple2_option (string_option, Md4.option)))
    [admin_user_name, blank_password]

(*************************************************************************)
(*                         User database functions                       *)
(*************************************************************************)

let user2_users_iter f =
  List.iter f ((List.sort (fun u1 u2 -> compare u1.user_name u2.user_name)) !!userlist)

let update_user name new_user =
  let other_users = List.filter (fun u -> u.user_name <> name) !!userlist in
  userlist =:=
      match new_user with
      | None -> other_users
      | Some new_user -> new_user :: other_users

let user2_user_add name pass ?(groups = [admin_group_name])
                             ?(default_group = Some admin_group_name)
                             ?(mail = "") ?(commit_dir = "") ?(max_dl = 0) () =
  (* shouldn't we warn admin about already existing user ? *)
  let groups = List.map user2_group_find (List.filter user2_group_exists groups) in
  let default_group =
    match default_group with
      None -> None
    | Some group -> if not (user2_group_exists group) then None else Some (user2_group_find group)
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
  update_user name (Some new_user)

let user2_user_remove user =
  update_user user None

let user2_user_find user =
  List.find (fun u -> u.user_name = user) !!userlist

let user2_user_exists user =
  List.exists (fun u -> u.user_name = user) !!userlist

(*************************************************************************)
(*               User database functions / passwords                     *)
(*************************************************************************)

let user2_user_password user =
  (user2_user_find user).user_pass

let user2_user_set_password user pass_string =
  user.user_pass <- Md4.string pass_string

let valid_password user pass =
  try
    user2_user_password user = Md4.string pass
  with Not_found -> false

let has_empty_password user =
  valid_password user.user_name ""

(*************************************************************************)
(*               User database functions                                 *)
(*************************************************************************)

let user2_user_set_mail user mail =
  user.user_mail <- mail

let user2_print_user_dls user =
  let dls = user.user_max_concurrent_downloads in
  if dls = 0 then "unlimited" else string_of_int dls

let user2_user_set_dls user dls =
  user.user_max_concurrent_downloads <- dls

let user2_user_commit_dir user =
  (user2_user_find user).user_commit_dir

let user2_user_set_commit_dir user dir =
  user.user_commit_dir <- dir

let admin_user () =
  try
    user2_user_find admin_user_name
  with Not_found -> default_admin_user

(*************************************************************************)
(*                         User/Group database functions                 *)
(*************************************************************************)

let sort_groups_by_name gl =
  List.sort (fun g1 g2 -> compare g1.group_name g2.group_name) gl

let user2_user_groups_iter user f =
  List.iter f (sort_groups_by_name user.user_groups)

let user2_print_user_groups sep user =
  String.concat sep (List.map (fun g -> g.group_name) (sort_groups_by_name user.user_groups))

let user2_print_group group =
  match group with
    None -> "none"
  | Some group -> group.group_name

let user2_print_user_default_group user =
  user2_print_group user.user_default_group

let user2_user_set_default_group user group =
  user.user_default_group <- group

let user2_user_add_group user group =
  user.user_groups <- group :: user.user_groups

let user2_user_remove_group user group =
  user.user_groups <- List.filter ((<>) group) user.user_groups

let user2_num_group_members group =
  let counter = ref 0 in
  user2_users_iter (fun u ->
    user2_user_groups_iter u (fun g ->
      if g = group then incr counter));
  !counter

(*************************************************************************)
(*                         Access rights                                 *)
(*************************************************************************)

let user2_is_admin user =
  user = admin_user () ||
  List.exists (fun groupname ->
    try
      groupname.group_admin
    with Not_found -> false)
  user.user_groups

(* could be expanded later *)
let user2_can_view_uploads user =
  user2_is_admin user

let user2_can_view_file user file_owner file_group =
  user2_is_admin user || user = file_owner ||
  (match file_group with
  | None -> false
  | Some file_group -> List.mem file_group user.user_groups)

(*************************************************************************)
(*                         Hooks                                         *)
(*************************************************************************)

let _ =
  set_after_load_hook users_ini (fun _ ->
    List.iter (fun (user,pass) ->
      if not (user2_user_exists user) then begin
        user2_user_add user pass ();
        lprintf_nl "converted user %s to new format" user
      end) !!users;
(* clean !!users to avoid saving users more than once *)
    users =:= [];
(* Security and default checks 
   - user "admin" must exist, it has hard-coded admin rights independent of group membership
   - group "mldonkey" must exist and must have admin status *)
    if not (user2_user_exists admin_user_name) then
      begin
        user2_user_add admin_user_name blank_password ();
        lprintf_nl "SECURITY INFO: user 'admin' has to be present, creating with empty password..."
      end;
    begin
      try
        let g = admin_group () in
        if not g.group_admin then
          begin
            user2_group_admin g true;
            lprintf_nl "SECURITY INFO: group 'mldonkey' must have admin status, updating..."
          end
      with Not_found ->
        user2_group_add admin_group_name true;
        lprintf_nl "SECURITY INFO: group 'mldonkey' has to be present, creating with admin rights..."
    end
  );

(* This code provides backward-compatibility for older MLDonkey clients *)
(* reading new user db and copying the values into old user db !!users *)
  set_before_save_hook users_ini (fun _ ->
    user2_users_iter (fun user ->
        users =:= (user.user_name, (user2_user_password user.user_name)) :: !!users
    )
  );
(* clean !!users to avoid saving users more than once *)
  set_after_save_hook users_ini (fun _ -> users =:= [])

let _ =
  Heap.add_memstat "CommonUserDb" (fun level buf ->
      Printf.bprintf buf "  grouplist: %d\n" (List.length !!grouplist);
      if level > 0 then
        List.iter (fun g -> Printf.bprintf buf "    group %s\n" g.group_name) !!grouplist;
      Printf.bprintf buf "  userlist: %d\n" (List.length !!userlist);
      if level > 0 then
        List.iter (fun u -> Printf.bprintf buf "    user %s\n" u.user_name) !!userlist;
  )

(* GUI user list *)
let ui_users = ref []

let find_ui_user user =
  let rec iter list =
    match list with
      [] ->
        let u = {
            ui_user = user2_user_find user;
            ui_user_searches = [];
            ui_last_search = None;
            ui_last_results = [];
            ui_http_conn = None;
          } in
        ui_users := u :: !ui_users;
        u
    | u :: tail ->
        if u.ui_user = user2_user_find user then u else iter tail
  in
  iter !ui_users

let _ =
(* these functions are called before ini files are read
   user "admin" is always needed in !!users, so create a
   dummy here which is replaced during start-up with the
   admin user read from users.ini *)
  update_user admin_user_name (Some default_admin_user);
  ignore (find_ui_user admin_user_name)
