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

let max_uploaders = 5



(*given some files choose the next uploaders based on their behavior
  Will choose five uploaders for each file in list
*)
let choose_next_uploaders files =   
  let full_list = ref ([] : BTTypes.client list) 
  and keepn orl l i = 
    (*keep l1 l2 num constructs a list of num items max with all 
      l1 items +  some l2 items *)
    if (List.length orl) < i then
      let rec keepaux k j =
	if j=0 then [] 
	else match k with
	  | [] -> []
	  | p::r -> p::(keepaux r (j-1)) in
	orl@(keepaux l (i-List.length orl))
    else
      orl
  in
    
    List.iter (fun f ->
		 if file_state f = FileDownloading then
		   begin
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
		       nodl : clients which gave nothing to us*)
		   let dl,nodl = List.partition (fun a -> Rate.(>) a.client_downloaded_rate 
						   Rate.zero ) filtl in
		     
		   (*sort by biggest contributor*)
		   let sortl = List.sort (fun a b -> Rate.compare b.client_downloaded_rate 
					    a.client_downloaded_rate) dl in

		     
		     max_list:= keepn !max_list sortl (max_uploaders - 1);
		     (*
		       clients in optim are current optimistic uploaders (30 seconds)	      
		     *)
		     let optim,notoptim = List.partition ( fun a ->
							     (Rate.ratesince a.client_upload_rate) > 0.
							     && 
							     a.client_last_optimist + 30 > last_time()
							 ) nodl in
		       (*
			 Choose 
		       *)
		     let notoptim = List.sort (fun a b -> compare a.client_last_optimist b.client_last_optimist) notoptim in
		       
		       
		       
		       max_list := keepn !max_list (optim) (max_uploaders);    
		       full_list := !full_list @ (keepn !max_list (notoptim) (max_uploaders))
		   end
) files;
    !full_list;
