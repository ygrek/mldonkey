(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

open CommonOptions
open Options
open CommonGlobals
  
let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)

let audiogal_ini = create_options_file (
    Filename.concat file_basedir "audiogal.ini")
  
  
let login = define_option audiogal_ini ["login"]
  "Your login member on www.audiogalaxy.com.
Use the Windows satellite to get one" string_option ""
  
let password = define_option audiogal_ini ["password"]
    "Your password on www.audiogalaxy.com"
    string_option ""

let gold_account = define_option audiogal_ini ["gold_account"]
    "Is your account a gold one"
    bool_option false
  
let redirection_server = define_option audiogal_ini ["redirection_server"]
    ""
    string_option "garlix.audiogalaxy.com"
  
let gold_redirection_server = define_option audiogal_ini 
  ["gold_redirection_server"]
    ""
    string_option "gold.audiogalaxy.com"
  
let redirection_server_ip = define_option audiogal_ini
    ["redirection_server_ip"]
    ""
    Ip.option (Ip.of_string "64.245.59.169")

  
let gold_redirection_server_ip = define_option audiogal_ini 
    ["gold_redirection_server_ip"]
    ""
    Ip.option (Ip.of_string "64.245.59.147")
  

let http_port = define_option audiogal_ini ["http_port"]
  "The port to redirect to www.audiogalaxy.com"
    int_option 4081
  
  
  