(* Copyright 2003, Denis Fortin 
   
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

open BTGlobals
open BTRate
open BTTypes
open CommonGlobals
open BasicSocket
open CommonTypes
open List2

let max_uploaders = 5



(*given some files choose the next uploaders based on their behavior
  Will choose five uploaders for each file in list
  fun_comp is the function used to classify clients
*)
let choose_next_uploaders files fun_comp =   
  let full_list = ref ([] : BTTypes.client list) 
  and keepn orl l i = 
(*keep l1 l2 num constructs a list of num items max with all 
      l1 items +  some l2 items *)
    let orig_num = List.length orl in
    if orig_num < i && i>0 then
      let keep,rest = cut (i - orig_num) l in
      orl@keep,rest
    else
      orl,l
  in
  
  List.iter (fun f ->
      let max_list = ref ([] : BTTypes.client list) in
(*Choose at most five uploaders for _each_ files*)	 
(*all clients*)
      let possible_uploaders = ref ([] :  BTTypes.client list) in
      Hashtbl.iter (fun _ c -> 
          begin
            possible_uploaders := (c::!possible_uploaders);
          end )  f.file_clients;


(*Interested clients with a connection*)
      let filtl = List.filter (fun c -> c.client_interested == true 
            && (c.client_sock != NoConnection) 
        ) !possible_uploaders in
(*dl : clients which gave something to us
		       nodl : clients which gave nothing to us
		   let dl,nodl = List.partition (fun a -> Rate.(>) a.client_downloaded_rate 
						   Rate.zero ) filtl in*)

(*sort by biggest contributor*)
      let sortl = List.sort fun_comp filtl in
      
      let to_add,next = keepn !max_list sortl (max_uploaders - 1) in
      max_list:= to_add;
(*
		       clients in optim are current optimistic uploaders (30 seconds)	      
		     *)
      let optim,notoptim = List.partition ( fun a ->
            (Rate.ratesince a.client_upload_rate) > 0.
              && 
            a.client_last_optimist + 30 > last_time()
        ) next in
(*
			 Choose 
		       *)
      let notoptim = List.sort (fun a b -> compare a.client_last_optimist b.client_last_optimist) notoptim in
      
      let to_add,next =  keepn !max_list (optim) (max_uploaders) in
      max_list := to_add;
      let to_add,_ = keepn !max_list (notoptim) 
        (max_uploaders) in
      full_list := !full_list @ to_add;
  
  ) files;
  !full_list
  

let choose_best_downloaders files = 
  choose_next_uploaders files (fun a b -> Rate.compare b.client_downloaded_rate 
        a.client_downloaded_rate)
  
(*highest uploader first in list*)
let choose_best_uploaders files = 
  choose_next_uploaders files (fun a b -> Rate.compare b.client_upload_rate 
        a.client_upload_rate)
  