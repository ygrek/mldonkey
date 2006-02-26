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

type userdb = {
    user_name : string;
    user_pass : Md4.t;
    user_mail : string;
  }

let users2 = Hashtbl.create 10

let blank_password = Md4.string ""

let user2_iter f =
  Hashtbl.iter f users2

let user2_add name pass mail =
  let u = {
      user_name = name;
      user_pass = pass;
      user_mail = mail
  } in
  Hashtbl.replace users2 name u;
  u

let user2_remove user =
  Hashtbl.remove users2 user

let user2_find user =
  try
    Hashtbl.find users2 user
  with Not_found -> failwith (Printf.sprintf "User %s does not exist" user)

let user2_password user =
  try
    let u = user2_find user in
      u.user_pass
  with Not_found -> failwith (Printf.sprintf "User %s does not exist" user)

let user2_mail user =
  try
    let u = user2_find user in
      u.user_mail
  with Not_found -> failwith (Printf.sprintf "User %s does not exist" user)

let valid_password user pass =
  try
    user2_password user = Md4.string pass
  with e -> false

let empty_password user =
  try
    let p = user2_password user in
     p  = blank_password
  with _ -> false

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
	  { user_name = uname;
	    user_pass = upass;
	    user_mail = umail; }

      | _ -> failwith "Options: not a valid user"

    let user_to_value user =
      Options.Module [
        "user_name", string_to_value user.user_name;
        "user_pass", string_to_value (Md4.to_string user.user_pass);
        "user_mail", string_to_value user.user_mail; ]

    let t = define_option_class "Users" value_to_user user_to_value

  end

let users2_section = file_section users_ini ["Users"] "User accounts on the core (new format)"

let userlist = define_option users2_section ["users2"]
  "The users that are defined on this core. The default user is
called 'admin', and uses an empty password. To create new users,
login as admin in mldonkey, and use the 'useradd' command."
    (list_option UserOption.t)
    [ { user_name = "admin";
        user_pass = blank_password;
        user_mail = "" } ]

let _ =
  set_after_load_hook users_ini (fun _ ->
      List.iter (fun user ->
          ignore (user2_add user.user_name user.user_pass user.user_mail)
      ) !!userlist;
      userlist =:= [];
      if !!users <> [] then begin
        lprintf_nl "[cUd] converting %d users to new format" (List.length !!users);
        List.iter (fun (user,pass) -> ignore (user2_add user pass "")) !!users;
	users =:= []
      end
  );
  set_before_save_hook users_ini (fun _ ->
      user2_iter (fun _ user ->
          userlist =:= (user2_find user.user_name)
          :: !!userlist
      )
  );
  set_after_save_hook users_ini (fun _ -> userlist =:= [])
