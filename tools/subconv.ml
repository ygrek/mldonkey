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

open Gettext
open Md4
open LittleEndian
open Unix
open Printf2

let _s x = _s "Subconv" x
let _b x = _b "Subconv" x  

type position =
  FrameInterval of float * float

let argument = ref []
let framerate = ref 24.
let split_frame = ref None
let delay_frame = ref 0.
let concat_frame = ref None

let input_line ic =
  let s = input_line ic in
  let len = String.length s in
  if len > 0 && s.[len-1] = '\r' then
    String.sub s 0 (len-1)
  else s
  
let int_of_string s =
  try
    int_of_string s    
  with e ->
      lprintf "Error in int_of_string: [%s]\n" s;
      raise e

(*************************************************************************)
(*                                                                       *)
(*                         SRT functions                                 *)
(*                                                                       *)
(*************************************************************************)

let srt_time_of_frame frame =
  let time = frame /. !framerate in
  Printf.sprintf "%02d:%02d:%02d,%03d"
    (int_of_float (time /. 3600.))
  (int_of_float (time /. 60.) mod 60)
  (int_of_float time mod 60)
  (int_of_float (time *. 1000.) mod 1000)

let frame_of_srt_time time = 
  try
    match String2.split_simplify time ':' with
      hours :: mins :: secs :: [] ->
        let hours = int_of_string hours in
        let mins = int_of_string mins in
        let secs, msecs =
          match String2.split_simplify secs ',' with
            [secs; msecs] ->
              int_of_string secs, int_of_string msecs
          | _ ->  raise Not_found
        in
          ((float_of_int hours *. 3600. +.
            float_of_int mins *. 60. +.
            float_of_int secs +.
            float_of_int msecs /. 1000.) *. !framerate)
    | _ -> raise Not_found
  
  with e ->
      failwith (Printf.sprintf "Bad time format [%s]" time)

(*************************************************************************)
(*                                                                       *)
(*                         output_frames                                 *)
(*                                                                       *)
(*************************************************************************)

let fprintf_sub oc (pos, lines) frame_offset =
  
  let frame1, frame2 = 
    match pos with
      FrameInterval (frame1, frame2) ->
        frame1, frame2
  in
  
  let frame1 = frame1 +. !delay_frame in
  let frame2 = frame2 +. !delay_frame in
  
  let line = String2.unsplit lines '|' in
  
  Printf.fprintf oc "{%d}{%d}%s\n" 
    (int_of_float (frame1 -. frame_offset)) 
  (int_of_float (frame2 -. frame_offset)) line
  
let fprintf_srt oc i (pos, lines) frame_offset =
  
  let frame1, frame2 = 
    match pos with
      FrameInterval (frame1, frame2) ->
        frame1, frame2
  in
  
  let frame1 = frame1 +. !delay_frame in
  let frame2 = frame2 +. !delay_frame in
  
  Printf.fprintf oc "%d\n" i;
  Printf.fprintf oc "%s --> %s\n"
    (srt_time_of_frame 
      (frame1 -. frame_offset)) 
  (srt_time_of_frame 
      (frame2 -. frame_offset));
  List.iter (fun line ->
      Printf.fprintf oc "%s\n" line) lines;
  Printf.fprintf oc "\n"
  
let output_frames file frame_begin frame_end frame_offset =
  if file <> "" then
    match Filename2.last_extension file with
      ".sub" -> 
        
        let oc = open_out file in
        
        List.iter (fun (pos, lines) ->            
            let frame1, frame2 = 
              match pos with
                FrameInterval (frame1, frame2) ->
                  frame1, frame2
            in
            
            let frame1 = frame1 +. !delay_frame in
            let frame2 = frame2 +. !delay_frame in
            
            if frame1 < frame_end && frame2 > frame_begin then
              
              fprintf_sub oc (pos,lines) frame_offset
        ) !argument;
        
        close_out oc
    
    | ".srt" ->
        
        let oc = open_out file in
        
        let rec iter i list =
          match list with
            [] -> ()
          | (pos, lines) :: tail ->

              let frame1, frame2 = 
                match pos with
                  FrameInterval (frame1, frame2) ->
                    frame1, frame2
              in
              
              let frame1 = frame1 +. !delay_frame in
              let frame2 = frame2 +. !delay_frame in
              
              if frame1 < frame_end && frame2 > frame_begin then begin
                  
                  fprintf_srt oc i (pos,lines) frame_offset;
                  iter (i+1) tail
                end else iter i tail
        
        in
        iter 1 !argument;
        
        close_out oc        
        
    | e -> lprintf "Unknown extension [%s]\n" e; exit 2      

(*************************************************************************)
(*                                                                       *)
(*                         read_frames                                   *)
(*                                                                       *)
(*************************************************************************)

let read_frames file concat_frame =
  
  match Filename2.last_extension file with
    ".sub" -> 
      
      let subtitles = ref [] in
      let ic = open_in file in
      (try
          while true do
            let line = input_line ic in
            
            let pos1 = String.index line '}' in
            let pos2 = String.index_from line (pos1+1) '}' in
            
            let frame1 = int_of_string (String.sub line 1 (pos1 - 1)) in
            let frame2 = int_of_string (String.sub line 
                  (pos1+2) (pos2 - pos1 - 2)) in
            
            let sub = String.sub line (pos2+1) (String.length line - pos2 - 1) in
            let lines = String2.split sub '|' in
            
            subtitles := (FrameInterval (
                concat_frame +. float_of_int frame1, 
                concat_frame +. float_of_int frame2), lines) :: 
            !subtitles
          done
        with End_of_file -> 
            close_in ic
        | e -> close_in ic; raise e
      );
      List.rev !subtitles
  
  
  | ".srt" ->
      
      let subtitles = ref [] in
      let ic = open_in file in
      (try
          while true do
            
            let line = input_line ic in
            let i = int_of_string line in
            
            let line = input_line ic in
            let frame1, frame2 =
              match String2.split line ' ' with
                time1 :: _ :: time2 :: [] ->
                  
                  concat_frame +. frame_of_srt_time time1, 
                  concat_frame +. frame_of_srt_time time2
              
              | _ -> 
                  failwith (Printf.sprintf  "Bad line [%s]\n" line)
            in
            
            let rec iter list =
              let line = input_line ic in
              if line = "" then List.rev list else
                iter (line :: list)
            in
            let lines = iter [] in
            
            subtitles := (FrameInterval (frame1, frame2), lines) :: 
            !subtitles
          done
        with End_of_file -> 
            close_in ic
        | e -> close_in ic; raise e
      );
      List.rev !subtitles
  
  | e -> lprintf "Unknown extension [%s]\n" e; exit 2

(*************************************************************************)
(*                                                                       *)
(*                         MAIN                                          *)
(*                                                                       *)
(*************************************************************************)
      
let _ =
  Arg2.parse [
    
    "-from", Arg2.String (fun file ->
        argument := read_frames file 0.;
    ), " <filename>: read <filename> subtitle file (.sub or .srt file)";
    
    "-framerate", Arg2.Float (fun f -> framerate := f), 
    " <framerate>: set the framerate (nb frames/second)";
    
    "-delay_frame", Arg2.Float (fun i -> delay_frame := i), 
    " <frame>: introduce this delay in the subtitles";
    
    "-delay_time", Arg2.Float (fun f -> 
        delay_frame :=  (f *. !framerate)), 
    " <time>: introduce this delay in the subtitles";
    
    (*
    "-remove_delay_frame", Arg2.Float (fun i -> delay_frame := -. i), 
    " <frame>: introduce this delay in the subtitles";
    
    "-remove_delay_time", Arg2.Float (fun f -> 
        delay_frame := -. (f *. !framerate)), 
    " <time>: introduce this delay in the subtitles";
*)
    
    "-rescale", Arg2.Float (fun newframerate ->
        
        let rescale = !framerate /. newframerate in
        
        let rescale frame = frame *. rescale in
        
        argument := List2.tail_map (fun 
            (FrameInterval (frame1, frame2),lines) ->
            FrameInterval (rescale frame1, rescale frame2), lines
        ) !argument
    
    ) , " <framerate> : rescale the frames on a different frame rate";
    
    "-to", Arg2.String (fun file ->
        output_frames file min_float max_float 0.    
    ), " <filename>: write <filename> subtitle file (.sub or .srt)";
    
    "-split_frame", Arg2.Float (fun i -> split_frame := Some i), 
    " <frame>: where to cut the subtitle file";
    
    "-split_time", Arg2.Float (fun f -> 
        split_frame := Some (f *. !framerate)), 
    " <time>: where to cut the subtitle file";
    
    "-split", Arg2.Array (2, fun array ->
        match !split_frame with
          None -> lprintf "No split frame set. You must set one.\n"; exit 2
        | Some split_frame ->
            
            output_frames array.(0) min_float split_frame 0.;
            output_frames array.(1) split_frame max_float split_frame;
    
    ), " <filename1> <filename2>: cut subtitles in <filename1> and <filename2>";
    
    "-concat_frame", Arg2.Float (fun i -> concat_frame := Some i), 
    " <frame>: where to add the subtitle file";
    
    "-concat_time", Arg2.Float (fun f -> 
        concat_frame := Some (f *. !framerate)), 
    " <time>: where to add the subtitle file";
    
    "-print", Arg2.Unit (fun _ ->
        
        match !argument with
          [] -> failwith "Not enough subtitles"
        | (pos, lines) :: _ ->

            Printf.fprintf Pervasives.stdout "FIRST SUBTITLE:\n";
            fprintf_sub Pervasives.stdout (pos, lines) 0.;
            fprintf_srt Pervasives.stdout 1 (pos, lines) 0.;
            
            let rec iter list =
              match list with
                [] -> assert false
              | [pos, lines] ->
                  Printf.fprintf Pervasives.stdout "\n\nLAST SUBTITLE:\n";
                  fprintf_sub Pervasives.stdout (pos, lines) 0.;
                  fprintf_srt Pervasives.stdout 1 (pos, lines) 0.;

              | _ :: tail -> iter tail
            in
            iter !argument
        
        
    ) , " : print first and last lines of file";
    
    "-concat", Arg2.String (fun file ->
        
        match !concat_frame with
          None -> lprintf "No concat frame set. You must set one.\n"; exit 2
        | Some concat_frame ->
        
            argument := !argument @ read_frames file concat_frame
        
    ) , " <filename> : add these subtitles to the previous ones";
    
  ] (fun e -> 
      lprintf "Don't know what to do with [%s]\n" e; exit 2
      ) 
  "subconv: modify subtitles files"
