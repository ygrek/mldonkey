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
let string_ncmp s1 s2 n =
  let sz1 = String.length s1 in
  let sz2 = String.length s2 in
  if sz1 < n || sz2 < n then s1 = s2
  else
    let s1' = String.sub s1 0 n in
    let s2' = String.sub s2 0 n in
    s1' = s2'

(* search str in doc from char deb *)
let search_from doc deb str =
  let last = (String.length doc) - (String.length str) in
  let i = ref deb in
  let cont = ref true in
  let len = String.length str in
  let ok () =
    try
      for j=0 to len-1 do
        if doc.[!i+j] != str.[j] then raise Not_found
      done;
      true
    with _ -> false in
  while !cont do
    (* make sure we're not too far away *)
    if !i > last
    then raise Not_found;
    (* Is it ok ? *)
    if ok() then cont := false else incr i
  done;
  !i

(* replace all the occurences of a char in a string by a given string *)
let replace doc chr str =
  let res = Buffer.create (2 * (String.length doc)) in
  let pos = ref 0 in
  let rec aux () =
    let new_pos = String.index_from doc !pos chr in
    Buffer.add_substring res doc !pos (new_pos - !pos);
    Buffer.add_string res str;
    pos := new_pos + 1;
    aux () in
  (try
    aux ()
  with
  | Not_found -> Buffer.add_substring res doc !pos ((String.length doc) - !pos)
  | Invalid_argument _ -> ());
  Buffer.contents res

let split s c =
  let len = String.length s in
  let rec iter pos =
    try
      if pos = len then [""] else
      let pos2 = String.index_from s pos c in
      if pos2 = pos then "" :: iter (pos+1) else
        (String.sub s pos (pos2-pos)) :: (iter (pos2+1))
    with _ -> [String.sub s pos (len-pos)]
  in
  iter 0
;;

let splitn s c n =
  let len = String.length s in
  let rec iter pos n =
    try
      if n = 0 then raise Not_found else
      if pos = len then [""] else
      let pos2 = String.index_from s pos c in
      (String.sub s pos (pos2-pos)) :: (iter (pos2+1) (n-1))
    with _ -> [String.sub s pos (len-pos)]
  in
  iter 0 n
;;

let rec remove_empty list list2 =
  match list with
    [] -> List.rev list2
  | "" :: list -> remove_empty list list2
  | s :: list -> remove_empty list (s :: list2)
      
let split_simplify s c =
  let list = split s c in
  remove_empty list []

let rec unsplit l c =
  match l with
    [] -> ""
  | [x] -> x
  | x :: ((_ :: _) as tail) ->
      Printf.sprintf "%s%c%s" x c (unsplit tail c)
;;

let words s = 
  let len = String.length s in
  let rec iter_out pos =
    if pos = len then [] else
    let c = s.[pos] in
    match c with
      ' ' | '\009' | '\010' | '\013' -> iter_out (pos+1)
      | _ -> iter_in pos (pos+1)
        
  and iter_in pos0 pos =
    if pos = len then [String.sub s pos0 (len-pos0)] else
    let c = s.[pos] in
    match c with
      ' ' | '\009' | '\010' | '\013' -> 
        (String.sub s pos0 (pos - pos0))::
        (iter_out (pos+1))
      | _ -> iter_in pos0 (pos+1)
  in
  iter_out 0
;;

let convert init f s =
  let len = String.length s in
  let b = Buffer.create len in
  let status = ref init in
  for i = 0 to len - 1 do
    let c = s.[i] in
    status := f b !status c
  done;
  Buffer.contents b

let before s pos = String.sub s 0 pos
let after s pos = 
  let len = String.length s in
  String.sub s pos (len - pos)
  
let cut_at s c =
  try
    let pos = String.index s c in
    before s pos,
    after s (pos+1);
  with _ -> s, ""

      
let check_prefix s prefix =
  let len = String.length s in
  let plen = String.length prefix in
  len >= plen  && String.sub s 0 plen = prefix

let check_suffix s suffix =
  let len = String.length s in
  let slen = String.length suffix in
  len >= slen && String.sub s (len - slen) slen = suffix
  
let upp_initial s =
  if String.length s > 0 then
    let first_char = Char.uppercase_ascii s.[0] in
    String.make 1 first_char ^ String.sub s 1 (String.length s - 1)
  else
    s
    
(* not optimal !*)
let subequal s1 pos1 s2 pos2 len =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  pos1 + len <= len1 &&
  pos2 + len <= len2 &&
  (let rec iter pos =
      pos = len ||
      (s1.[pos1 + pos] = s2.[pos2 + pos] && iter (pos+1))
    in
    iter 0)
    
(* not optimal !*)
let subcontains s sub =
  let slen = String.length sub in
  let len = String.length s in
  len >= slen && (
    let rec after_pos pos =
      not (pos + slen > len) &&
      (subequal s pos sub 0 slen ||
      after_pos (pos+1))        
    in 
    after_pos 0)
  
let of_char c = String.make 1 c
  
  
let resize s newlen =
  let len = String.length s in
  if len > newlen then
    String.sub s 0 newlen 
  else
    let str = Bytes.create newlen in
    Bytes.blit_string s 0 str 0 len;
    Bytes.to_string str

let resize_bytes s newlen =
  let len = Bytes.length s in
  if len > newlen then
    Bytes.sub s 0 newlen 
  else
    let str = Bytes.create newlen in
    Bytes.blit s 0 str 0 len;
    str

let init len f =
  let s = String.create len in
  for i = 0 to len - 1 do
    s.[i] <- f i
  done;
  s

let is_space c = c = ' ' || c = '\n' || c = '\r' || c = '\t'
  
let tokens s =
  let len = String.length s in
  let rec iter_space start_pos i  =
    if i = len then
      [String.sub s start_pos (i - start_pos)]
    else
    let c = s.[i] in
    if is_space c  then
      (String.sub s start_pos (i - start_pos)) ::
      (iter_next (i+1))
    else
      iter_space start_pos (i+1)
      
  and iter_next i =
    if i = len then [] else
    let c = s.[i] in
    if is_space c then
      iter_next (i+1)
    else
    if c = '\'' || c = '"' then
      iter_delim (i+1) (i+1) c
    else
      iter_space i (i+1)
      
  and iter_delim start_pos i delim =
    if i = len then
      failwith (Printf.sprintf "Unterminated delim %c" delim)
    else
    let c = s.[i] in
    if c = delim then
      (String.sub s start_pos (i - start_pos)) ::
      (iter_next (i+1))
    else
    if c = '\\' && i + 1 < len then
      iter_delim start_pos (i+2) delim
    else
      iter_delim start_pos (i+1) delim
    
  in
  iter_next 0 

external contains : string -> string -> bool = "ml_strstr"  
  
let rec strneql s1 s2 len =
  len = 0 || (
    let len = len - 1 in
    s1.[len] = s2.[len] && strneql s1 s2 len)
  
let starts_with s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  len2 <= len1 && strneql s1 s2 len2

let replace_char s c1 c2 =
  let rep c = if c == c1 then c2 else c in
  String.map rep s;
  ()

let stem s =
  let s = String.lowercase_ascii s in
  let result = Bytes.of_string s in
  for i = 0 to Bytes.length result - 1 do
    let c = Bytes.get result i in
    match c with
    | 'a'..'z' | '0'..'9' -> ()
    | _ -> Bytes.set result i ' '
  done;
  split_simplify (Bytes.to_string result) ' '
  
let map f s =
  let len = String.length s in
  if len = 0 then [||] else
  let v = f s.[0] in
  let array = Array.make len v in
  for i = 1 to len -1 do 
    array.(i) <- f s.[i]
  done;
  array
  
let iteri f s =
  let len = String.length s in
  for i = 0 to len-1 do
    f i s.[i]
  done
  
let init n f =
  let s = String.create n in
  for i = 0 to n - 1 do
    s.[i] <- f i 
  done;
  Bytes.to_string s

let exists p s =
  let l = String.length s in
  let rec aux i =
    i < l && (p s.[i] || aux (i+1)) in
  aux 0

let existsi p s =
  let l = String.length s in
  let rec aux i =
    i < l && (p i s.[i] || aux (i+1)) in
  aux 0

let for_all p s =
  let l = String.length s in
  let rec aux i =
    i >= l || p s.[i] && aux (i+1) in
  aux 0

let hex_string_of_string s =
  let buf = Buffer.create 100 in
  String.iter (fun c ->
    Printf.bprintf buf "%02x " (int_of_char c)
  ) s;
  Buffer.contents buf

let ( |> ) x f = f x

let dehtmlize =
  let br_regexp = Str.regexp_case_fold "<br>" in
  let tag_regexp = Str.regexp "<[^>]*>" in
  fun s ->
    s
    |> Str.global_replace br_regexp "\n"
    |> Str.global_replace tag_regexp ""
