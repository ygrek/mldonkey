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

let execvp_command cmd args handler = 
  let (in_read, output) = Unix.pipe() in
  let (input, out_write) = Unix.pipe() in
  match Unix.fork() with
    0 -> begin
        try
          match Unix.fork () with
            0 -> begin
                try
                  if input <> Unix.stdin then
                    begin Unix.dup2 input Unix.stdin; Unix.close input end;
                  if output <> Unix.stdout then
                    begin Unix.dup2 output Unix.stdout; Unix.close output end;
                  Unix.close in_read;
                  Unix.close out_write;
                  Unix.execvp cmd args;
                with e -> 
                    Printf.eprintf "Exception %s in exec_command\n"
                      (Printexc2.to_string e) ; 
                    exit 1
              end
          | id -> 
              exit 2
        with _ -> 
            exit 3
      end
  | id -> 
      ignore (snd(Unix.waitpid [] id));
      Unix.close input;
      Unix.close output;
      let sock = handler in_read out_write in
      sock, id

let fork_and_exec cmd ?vars args = 
  let env =
    match vars with
      | None -> None
      | Some vars ->
          (* convert environment to an alist *)
          let env = List.map (fun kev -> 
            try
              let len = String.length kev in
              let equal_sign = String.index kev '=' in
                (String.sub kev 0 equal_sign, 
                 String.sub kev (equal_sign + 1) (len - 1 - equal_sign))
            with Not_found -> (* May that happen ? *)
              (kev, "")
          ) (Array.to_list (Unix.environment ())) in
          (* update parent's environment with vars alist *)
          let env = List.fold_left (fun acc ((k, v) as assoc) ->
            assoc :: List.remove_assoc k acc
          ) env vars in
          Some (Array.of_list (List.map (fun (k, v) -> k ^ "=" ^ v) env)) in
  match Unix.fork() with
    0 -> begin
        try
          match Unix.fork() with
            0 -> begin
                try
                  match env with
                    | None ->
                        Unix.execv cmd args;
                    | Some env ->
                        Unix.execve cmd args env;
                with e -> 
                    lprintf "Exception %s while starting file_completed_cmd\n" (Printexc2.to_string e); 
                    exit 127
              end
          | id -> exit 0
        with _ -> exit 0
      end
  | id -> ignore (snd(Unix.waitpid [] id))
      
let setuid = Unix.setuid
let setgid = Unix.setgid
let set_close_on_exec = Unix.set_close_on_exec
let set_signal signal f = Sys.set_signal signal f
  
let chroot = Unix.chroot  

let write = Unix.write

let set_nonblock = Unix.set_nonblock

external glibc_version : unit -> string = "glibc_version"

let glibc_version_num () =
  begin
    try
      glibc_version ()
    with e -> ""
  end
  
let set_console_title buf = ()  (* fake function for DriverMain.mingw_second_timer *)
