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
open Int64ops
open Gettext
open Md4
open LittleEndian
open Unix
open Printf2
  
let _s x = _s "Dp500" x
let _b x = _b "Dp500" x  

module CommonTypes = struct type file end
module CommonFile = struct
    let file_size _ = zero
    let file_disk_name _ = "no_file"
    let file_best_name _ = "no such file"
    let file_downloaded _ = zero
  end
  
module DP500 = DriverLink.DP500(struct 
    

      module CommonOptions = CommonOptions
      module CommonFile = CommonFile
      module CommonTypes = CommonTypes

      let files () = []
      
    end)
  
let _ =
  
  let exists_downloads_ini = Sys.file_exists 
      (options_file_name downloads_ini) in
  if not exists_downloads_ini then begin
      lprintf "No config file found. Generating one.\n"; 
      let oc = open_out (options_file_name downloads_ini) in
      close_out oc; 
    end;
  (try 
      Options.load downloads_ini;
(*      Options.load downloads_expert_ini;       *)
    with e -> 
        lprintf "Exception %s during options load\n" (Printexc2.to_string e); 
        exit 2;
        ());  
  DP500.start ()
  