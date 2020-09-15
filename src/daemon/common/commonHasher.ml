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

let log_prefix = "[cHa]"

let lprintf_nl fmt =
  lprintf_nl2 log_prefix fmt

let lprintf_n fmt =
  lprintf2 log_prefix fmt

type hash_method = MD4 | MD5 | SHA1 | TIGER

type 'a job = {
    job_name : string;
    job_begin : int64;
    job_len : int64;
    job_method : hash_method;
    job_result : 'a;
    job_handler : ('a job -> unit);
    job_error : bool;
  }

let (fifo : string job Fifo.t) = Fifo.create ()
let current_job = ref None

external job_done : 'a job -> bool = "ml_job_done"
external job_start : 'a job -> Unix.file_descr -> unit = "ml_job_start"

let _ =
  BasicSocket.add_infinite_timer 0.1 (fun _ ->
(*      lprintf "test job\n";  *)
      try
        match !current_job with
        | None -> raise Not_found
        | Some (job, fd) ->
            if job_done job then begin
                if !verbose_md4 then lprintf_nl "Finished %s job %s %Ld %Ld"
                  (match job.job_method with
                    MD5 -> "MD5" | TIGER -> "TIGER" | SHA1 -> "SHA1" | MD4 -> "MD4")
                  job.job_name job.job_begin job.job_len;
                current_job := None;
                Unix.close fd;
                (try job.job_handler job with e ->
                      lprintf_nl "exception %s in job_handler"
                        (Printexc2.to_string e);
                      );
                raise Not_found
              end
      with _ ->

          let job = try Fifo.take fifo

            with e ->
(*                lprintf "No waiting job\n"; *)
                raise e
          in
(*          lprintf "Job started\n";   *)
          try
            let fd = Unix.openfile job.job_name [Unix.O_RDONLY] 0o444 in
            current_job := Some (job, fd);
            job_start job fd;
          with e ->
              lprintf_nl "Exception %s in starting job"
                (Printexc2.to_string e);
  )

let compute_md4 name begin_pos len f =
  let job = {
      job_name = name;
      job_begin = begin_pos;
      job_len = len;
      job_method = MD4;
      job_result = Md4.Md4.create ();
      job_handler = f;
      job_error = false;
    } in
  Fifo.put fifo (Obj.magic job)

let compute_sha1 name begin_pos len f =
  let job = {
      job_name = name;
      job_begin = begin_pos;
      job_len = len;
      job_method = SHA1;
      job_result = Md4.Sha1.create ();
      job_handler = f;
      job_error = false;
    } in
  Fifo.put fifo (Obj.magic job)

let compute_md5 name begin_pos len f =
  let job = {
      job_name = name;
      job_begin = begin_pos;
      job_len = len;
      job_method = MD5;
      job_result = Md4.Md5.create ();
      job_handler = f;
      job_error = false;
    } in
  Fifo.put fifo (Obj.magic job)

let compute_tiger name begin_pos len f =
  let job = {
      job_name = name;
      job_begin = begin_pos;
      job_len = len;
      job_method = TIGER;
      job_result = Md4.TigerTree.create ();
      job_handler = f;
      job_error = false;
    } in
  Fifo.put fifo (Obj.magic job)
