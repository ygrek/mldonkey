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

open Options

let fileTP_ini = create_options_file "fileTP.ini"

let fileTP_section = file_section fileTP_ini ["FileTP"] "FileTP options"

  (*
let enabled = define_option fileTP_section
    ["fileTP2_enabled"]
    "Do you want to support FileTP2 protocol (not yet supported)"
    bool_option true
*)

let mirrors = define_option fileTP_section ["mirrors"]
    "A list of lists, where each list contains equivalent prefixes for mirrors"
    (list_option (list_option string_option))
  (* [
"http://www-ftp.lip6.fr/pub/linux/distributions/mandrake/";
"http://mirrors.kernel.org/mandrake/";
     ] *)
  []

let remote_shells = define_option fileTP_section ["remote_shells"]
  "A list of (hostname, args). Example: [  ( \"mycomputer.mydomain.fr\", (ssh, \"mldonkey@mycomputer.mydomain.fr\") ) ]"
    (list_option (tuple2_option (string_option, list_option string_option)))
  []

let get_range = define_option fileTP_section ["get_range"]
  "The command to call to get a range"
    string_option "get_range"

let range_arg = define_option fileTP_section ["range_arg"]
  "The argument to !!get_range to get a range"
    string_option "range"

let options_version = define_option fileTP_section ["options_version"]
    ~internal: true
    "(internal option)"
    int_option 0

let chunk_size = define_option fileTP_section ["chunk_size"]
  "Chunk size (in bytes) (0 = No chunks)"
  int_option 0

  (*
let verbose_clients =
  define_option fileTP_section ["verbose_clients"]
  "level of verbosity when communicating with clients"
    int_option 0

let verbose_servers =
  define_option fileTP_section ["verbose_servers"]
    "level of verbosity when communicating with servers" int_option 0
    *)

let shortname o =
  Printf.sprintf "FTP-%s" (shortname o)

let gui_fileTP_options_panel =
  (*
  define_option fileTP_section ["gui_fileTP_options_panel"]
    "Which options are configurable in the GUI option panel, and in the
  fileTP section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
(*    "Max Connected Ultrapeers", shortname max_ultrapeers, "T";
    "Max Known Ultrapeers", shortname max_known_ultrapeers, "T";
    "Max Known Peers", shortname max_known_peers, "T";    *)
  ]

let network_name = "FileTP"
