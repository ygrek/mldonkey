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

open Printf2
open CommonOptions

type hash_method = MD4 | MD5 | SHA1

type job = {
    job_name : string;
    job_begin : int64;
    job_len : int64;
    job_method : hash_method;
    job_result : string;
    job_handler : (job -> unit);
    job_error : bool;
  }

let fifo = Fifo.create ()
let current_job = ref None
  
external job_done : job -> bool = "ml_job_done"
external job_start : job -> Unix.file_descr -> unit = "ml_job_start"
  
let _ =
  if BasicSocket.has_threads () then begin
      lprintf "Using threads"; lprint_newline ();
    end;
  BasicSocket.add_infinite_timer 0.1 (fun _ ->
(*      lprintf "test job"; lprint_newline ();  *)
      try
        match !current_job with
        | None -> raise Not_found
        | Some (job, fd) ->
(*            lprintf "job done "; lprint_newline (); *)
            if job_done job then begin
                if !verbose_md4 then begin
                    lprintf "Job finished"; lprint_newline (); 
                  end;
                current_job := None;
                Unix.close fd;
                (try job.job_handler job with e -> 
                      lprintf "exception %s in job_handler"
                        (Printexc2.to_string e); lprint_newline ();
                      );
                raise Not_found
              end
      with _ ->

          let job = try Fifo.take fifo 
              
            with e -> 
(*                lprintf "No waiting job"; lprint_newline (); *)
                raise e
          in
(*          lprintf "Job ready"; lprint_newline (); *)
          try
            let fd = Unix.openfile job.job_name [Unix.O_RDONLY] 0o444 in
            current_job := Some (job, fd);
            if !verbose_md4 then begin
                lprintf "Starting job %s %Ld %Ld" job.job_name
                  job.job_begin job.job_len; lprint_newline (); 
              end;
            job_start job fd;
            if !verbose_md4 then begin
                lprintf "Job started"; lprint_newline ();
              end
          with e ->
              lprintf "Exception %s in starting job" 
                (Printexc2.to_string e); lprint_newline ();
  )
  
let compute_md4 name begin_pos len f =
  let job = {
      job_name = name;
      job_begin = begin_pos;
      job_len = len;
      job_method = MD4;
      job_result = String.create Md4.Md4.length;
      job_handler = f;
      job_error = false;
    } in
  Fifo.put fifo job
  
