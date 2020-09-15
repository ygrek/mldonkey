(***********************************************************************)
(*                               Zoggy                                 *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Useful functions. *)

open Zog_types

(** Get the first value of a property, function of the property kind. *)
let default_prop_value kind =
  try
    let (_,_,values_kind,_) = Zog_types.get_prop_info kind in
    match values_kind with
      Bool -> 
        (
         match kind with
           Expand | Homogeneous | Right_justify -> "false"
         | _ -> "true"
        )
    | PosInt -> ""
    | Float -> ""
    | Code -> ""
    | Code_list -> "[]"
    | Enum [] -> ""
    | Enum ((s,_) :: _) -> s
    | Enum_list [] -> ""
    | Enum_list ((s,_) :: _) -> s
    | Keysym -> ""
  with
    Failure s ->
      prerr_endline s ;
      ""


(** {2 Variables for Lexer/Parser} *)
let line_number = ref 0


(** {2 From Daniel de Rauglaudre's wserver} *)

let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)
;;

let hexa_val conf =
  match conf with
    '0'..'9' -> Char.code conf - Char.code '0'
  | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
  | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
  | _ -> 0
;;

let decode s =
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
        '%' | '+' -> true
      | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s ->
            let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
            s1.[i1] <- Char.chr v; i + 3
        | '+' -> s1.[i1] <- ' '; succ i
        | x -> s1.[i1] <- x; succ i
      in
      copy_decode_in s1 i (succ i1)
    else s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = String.create len in
    strip_heading_and_trailing_spaces (copy_decode_in s1 0 0)
  else s
;;

let special x = List.mem x ['='; '&'; '"'; '+'];;

let encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
        ' ' -> true
      | x -> if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
          ' ' -> s1.[i1] <- '+'; succ i1
        | c ->
            if special c then
              begin
                s1.[i1] <- '%';
                s1.[i1 + 1] <- hexa_digit (Char.code c / 16);
                s1.[i1 + 2] <- hexa_digit (Char.code c mod 16);
                i1 + 3
              end
            else begin s1.[i1] <- c; succ i1 end
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;;


(** {2 Convenient functions} *)


let class_of_class_name name =
  try
    let (c,_,_,_) = 
      List.find (fun (_,n,_,_) -> n = name) Zog_types.class_names_and_strings_complete
    in
    c
  with
    Not_found -> 
      raise (Failure (Zog_messages.class_not_found name))

let property_kind_of_property_name name =
  try
    let (k,_,_,_) = 
      List.find (fun (_,n,_,_) -> n = name) Zog_types.properties
    in
    k
  with
    Not_found -> 
      raise (Failure (Zog_messages.property_not_found name))

let apply_opt f o =
  match o with
    None -> ()
  | Some x -> f x

let p fmt = Format.pp_print_string fmt

let rec print_gui_element fmt t g_ele =
  try
    let (_,name,_,_) = Zog_types.get_class_info g_ele.classe in
    p fmt (t^"<"^name^" name="^g_ele.name) ;
    List.iter
      (fun prop ->
        try
          if prop.prop_value <> default_prop_value prop.prop_kind then
          let (_,pname,_,_) = Zog_types.get_prop_info prop.prop_kind in
          p fmt (" "^pname^"=\""^(encode prop.prop_value)^"\"")
        with
          Failure s -> prerr_endline s
      )
      g_ele.props;
    if not g_ele.expanded then p fmt " expanded=\"false\"" ;
    p fmt ">\n" ;
    List.iter (print_gui_element fmt (t^"  ")) g_ele.children ;
    p fmt (t^"</"^name^">\n")
  with
    Failure s -> prerr_endline s

let print_entity fmt ent =
  p fmt ("<entity name="^ent.en_name) ;
  List.iter (fun s -> p fmt (" "^s)) ent.en_params;
  p fmt ">\n" ;
  apply_opt (print_gui_element fmt "  ") ent.en_ele ;
  p fmt "</entity>\n"

let write_entity_list file entities =
  try
    let chanout = open_out file in
    let fmt = Format.formatter_of_out_channel chanout in
    List.iter (print_entity fmt) entities ;
    close_out chanout
  with
    Sys_error s ->
      raise (Failure s)
