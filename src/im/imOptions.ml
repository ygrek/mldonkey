(* Copyright 2002 b8_fange *)
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

open Options
open ImProtocol
open ImAccount
  
module AccountOption = struct
    
    let value_to_account v =
      match v with
        Options.Module assocs ->
          let get_value name conv = conv (List.assoc name assocs) in
          let network = try
              get_value "account_network" value_to_string
            with _ -> "Donkey"
          in
          let network = protocol_find_by_name network in
          let account = protocol_account_from_option network assocs in
          account
      | _ -> assert false
    
    let account_to_value account =
      let netname = string_to_value (protocol_name (
            account_protocol account)) in
        Options.Module (
          ("account_network", netname)
          ::
          (account_to_option account)
        )
          
    let t =
      define_option_class "Account" value_to_account account_to_value
    ;;
  end

let accounts_ini = create_options_file
    (Filename.concat  CommonOptions.config_dir "mldonkey_im.ini")
let accounts_section = file_section accounts_ini [] ""
  
let accounts = 
  define_option accounts_section ["accounts"] 
  "The different accounts" (listiter_option AccountOption.t) []

    
open Gettext
    
let browse_url_command = define_option accounts_section
    ["browse_url_command"] "The command to be called for browsing an url"
  (T.option (T.string T.format)) "opera -newwindow '%s'" 
  