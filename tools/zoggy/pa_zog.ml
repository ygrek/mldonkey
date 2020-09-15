(**************************************************************************)
(*                   Cameleon                                             *)
(*                                                                        *)
(*      Copyright (C) 2002 Institut National de Recherche en Informatique et   *)
(*      en Automatique. All rights reserved.                              *)
(*                                                                        *)
(*      This program is free software; you can redistribute it and/or modify  *)
(*      it under the terms of the GNU General Public License as published by  *)
(*      the Free Software Foundation; either version 2 of the License, or  *)
(*      any later version.                                                *)
(*                                                                        *)
(*      This program is distributed in the hope that it will be useful,   *)
(*      but WITHOUT ANY WARRANTY; without even the implied warranty of    *)
(*      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     *)
(*      GNU General Public License for more details.                      *)
(*                                                                        *)
(*      You should have received a copy of the GNU General Public License  *)
(*      along with this program; if not, write to the Free Software       *)
(*      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA          *)
(*      02111-1307  USA                                                   *)
(*                                                                        *)
(*      Contact: Maxence.Guesdon@inria.fr                                *)
(**************************************************************************)

(* $Id$ *)

(*
#load "pa_extend.cmo";;
*)
#load "q_MLast.cmo";;

(** Code generation from Zoggy input *)

open Zog_types

exception Field_error of string

(* BEGIN CDK *)

let anonymous_name_counter = ref 0
  
let gen_anonynous_name ele =
  if ele.name = "_" && ele.children <> [] then begin
      incr anonymous_name_counter;
      ele.name <- Printf.sprintf "_anonymous_container_%d" 
        !anonymous_name_counter;
    end
  
(* END CDK *)
  
module Zoggy(Syntax : Camlp4.Sig.Camlp4Syntax) =
struct
  open Camlp4.PreCast

  include Syntax
(*   open Ast *)

let parse_string loc =
  function
    "false" -> 
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
        )	
      in
*)
      <:expr@loc< False >>
  | "true" ->
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
        )	
      in
*)
      <:expr@loc< True >>

  | str ->
      try
(*         let strm = Stream.of_string str in *)
        Gram.parse_string expr loc str
(*
        Pcaml.expr_reloc
          (fun _ -> Lexing.dummy_pos, Lexing.dummy_pos) 
          Lexing.dummy_pos 
          r
*)
      with
        e ->
          Printf.eprintf "Error in \"%s\"\n" str; flush stderr;
          let (e, loc) =
            match e with
            | Loc.Exc_located (loc, e) -> e, loc 
            | e -> e, loc
          in
          Loc.raise loc e

(** parsing a prop value *)
let parse_prop_value prop =
(*   let loc = fst prop.prop_value_loc + 1, snd prop.prop_value_loc - 1 in *)
  parse_string prop.prop_value_loc prop.prop_value

let ast_of_class_info loc cl =
  let (_, _, _, fonc) = Zog_types.get_class_info cl in parse_string loc fonc

(** Return the value of a property kind in a list of properties. *)
let ast_of_prop_value loc props kind =
  try
    let p = List.find (fun p -> p.prop_kind = kind) props in
    parse_prop_value p
  with
    Not_found -> 
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)
        <:expr@loc< "" >>

let get_prop_label ele prop =
  match prop.prop_kind with
  | Function -> ""
  | Expand -> "expand"
  | Fill -> "fill"
  | Padding -> "padding"
  | Width -> "width"
  | Height -> "height"
  | Border_width -> "border_width"
  | Title -> "title"
  | Allow_shrink -> "allow_shrink"
  | Allow_grow -> "allow_grow"
  | Auto_shrink -> "auto_shrink"
  | X_pos -> "x"
  | Y_pos -> "y"
  | PLabel -> "label"
  | Group -> "group"
  | Orientation -> "orientation"
  | Toolbar_style -> "style"
  | Toolbar_space_size -> "space_size"
  | Toolbar_space_style -> "space_style"
  | Tooltips -> "tooltips"
  | Button_relief_style -> "button_relief"
  | Spacing -> "spacing"
  | Homogeneous -> "homogeneous"
  | Button_box_style -> "layout"
  | Child_width -> "child_width"
  | Child_height -> "child_height"
  | Child_ipadx -> "child_ipadx"
  | Child_ipady -> "child_ipady"
  | Label_xalign -> "label_xalign"
  | Label_yalign -> "label_yalign"
  | Shadow_type ->
      begin match ele.classe with
        Arrow -> "shadow"
      | _ -> "shadow_type"
      end
  | Obey_child -> "obey_child"
  | Ratio -> "ratio"
  | Hscrollbar_policy -> "hpolicy"
  | Vscrollbar_policy -> "vpolicy"
  | Handle_position -> "handle_position"
  | Snap_edge -> "snap_edge"
  | Column_titles -> "titles"
  | Show_titles -> "titles_show"
  | X_align -> "xalign"
  | Y_align -> "yalign"
  | X_pad -> "xpad"
  | Y_pad -> "ypad"
  | PText -> "text"
  | Line_wrap -> "line_wrap"
  | Tab_pos -> "tab_pos"
  | Show_tabs -> "show_tabs"
  | Homogeneous_tabs -> "homogeneous_tabs"
  | Show_border -> "show_border"
  | Scrollable -> "scrollable"
  | Tab_border -> "tab_border"
  | Popup -> "popup"
  | SBUpdate_policy -> "update_policy"
  | Visibility -> "visibility"
  | Editable -> "editable"
  | Use_arrows -> "use_arrows"
  | Case_sensitive -> "case_sensitive"
  | Word_wrap -> "word_wrap"
  | Column_number -> "columns"
  | Draw_indicator -> "draw_indicator"
  | Active -> "active"
  | Placement -> "placement"
  | Selection_mode -> "selection_mode"
  | Justification -> "justify"
  | Max_length -> "max_length"
  | View_mode -> "view_mode"
  | View_lines -> "view_lines"
  | Handle_size -> "handle_size"
  | Modal -> "modal"
  | Tab_label -> ""
  | Accel_group_name | Accel_modifier | Accel_flags | Accel_keysym |
    Show_toggle | Show_indicator | Right_justify ->
      ""
  | Arrow_type -> "kind"
  | Calendar_options -> "options"
  | Popdown_strings -> "popdown_strings"
  | Value_in_list -> "value_in_list"
  | Ok_if_empty -> "ok_if_empty"
  | Update_policy -> "update_policy"

  | PPixmap_file | PPixmap_data | PPixmap_code -> ""

(** Remove blanks (space, tabs, \r and \n) from a string. *)
let remove_blanks s =
  let buf = Buffer.create 16 in
  let f c = 
    match c with
      ' ' | '\n' | '\t' | '\r' -> ()
    | _ ->
        Buffer.add_char buf c
  in
  String.iter f s;
  Buffer.contents buf

(** Indicate whether a property param must be printed.*)
let must_gen prop =
  try
    let (_, _, values_kind, _) = Zog_types.get_prop_info prop.prop_kind in
    match values_kind with
      Bool -> true
    | PosInt -> 
        begin try int_of_string prop.prop_value >= 0 with
          _ -> (remove_blanks prop.prop_value) <> ""
        end
    | Float -> (remove_blanks prop.prop_value) <> ""
    | Code | Code_list -> (remove_blanks prop.prop_value) <> ""
    | Enum [] -> false
    | Enum ((s, _) :: _) -> true
    | Enum_list [] -> false
    | Enum_list ((s, _) :: _) -> true
    | Keysym ->
        let v_no_blank = remove_blanks prop.prop_value in v_no_blank <> ""
  with
    Failure s -> prerr_endline s; false

let field_error_string f =
  match List.find (fun (x, _, _, _) -> x = f) Zog_types.properties with
      (_, n, _, _) -> n

let ast_of_creation_options_code (loc:Loc.t) ele f =
(*
  let loc = 
    (
     { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
     { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
    )	
  in
*)

  let g f prop =
    match prop.prop_kind with
      Function | Tab_label | Expand | Fill | Padding -> f
    | Accel_modifier | Accel_group_name | Accel_flags | Accel_keysym | Show_toggle |
      Show_indicator | Right_justify ->
        f
    | PPixmap_file ->
        if must_gen prop then
          let v = parse_prop_value prop in
          <:expr< $f$ (GDraw.pixmap_from_xpm ~file : $v$ ()) >>
        else
          raise (Field_error (field_error_string PPixmap_file))
    | PPixmap_data ->
        if must_gen prop then
          let v = parse_prop_value prop in
          <:expr< $f$ (GDraw.pixmap_from_xpm_d ~data : $v$ ()) >>
        else
          raise (Field_error (field_error_string PPixmap_data))
    | PPixmap_code ->
        if must_gen prop then
          let v = parse_prop_value prop in
          <:expr< $f$ $v$ >>
        else
          raise (Field_error (field_error_string PPixmap_code))
    | _ ->
        if must_gen prop then
          let v = parse_prop_value prop in
          <:expr< $f$ ~ $get_prop_label ele prop$ : $v$ >>
        else f
  in
  List.fold_left g f ele.props

let ast_of_pack_options_code loc f ele =
  let g f prop =
    match prop.prop_kind with
      Expand | Fill | Padding ->
        if must_gen prop then
(*
          let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
            )	
          in
*)
          let v = parse_prop_value prop in
          <:expr< $f$ ~ $get_prop_label ele prop$ : $v$ >>
        else f
    | _ -> f
  in
  List.fold_left g f ele.props

let ast_of_pack_code loc parent ele f =
  try
    let pack_met = Zog_types.pack_method_of_ele parent ele in
    match pack_met with
      No_pack -> f
    | Insert_page ->
        let g = 
(*
          let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst parent.name_loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd parent.name_loc }
            )	
          in
*)
          <:expr< $lid:parent.name$ >> 
        in
        let loc1 = loc in
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)
        <:expr<
          $f$ ~packing:
            (fun w ->
               $g$ # append_page
                 ~tab_label:
                    (GMisc.label
                        ~text:$ast_of_prop_value loc1 ele.props Tab_label$ ())
                    #coerce w)
        >>
    | _ ->
        let g = 
(*         let loc = parent.name_loc in *)
(* FIXME
          let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst parent.name_loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd parent.name_loc }
            )	
          in
*)
          <:expr< $lid:parent.name$ >> 
        in
        let loc1 = loc in
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)
        let p x = <:expr< $f$ ~packing: $x$ >> in
        match pack_met with
          Pack -> p (ast_of_pack_options_code loc1 <:expr< $g$ # pack >> ele)
        | Add -> p <:expr< $g$ # add >>
        | Add1 -> p <:expr< $g$ # add1 >>
        | Add2 -> p <:expr< $g$ # add2 >>
        | Add_with_viewport -> p <:expr< $g$ # add_with_viewport >>
        | Set_submenu -> p <:expr< $g$ # set_submenu >>
        | Insert_page | No_pack -> f
  with
    Failure s -> 
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)
        prerr_endline s; 
        <:expr< $f$ failed >>

let ast_of_custom_pack_code loc parent ele ce =
  try
    let pack_met = Zog_types.pack_method_of_ele parent ele in
    match pack_met with
      No_pack -> ce
    | Insert_page ->

        let g = 
(* FIXME
          let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst parent.name_loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd parent.name_loc }
            )	
          in
*)
          <:expr< $lid:parent.name$>> 
        in
        let n = 
(* FIXME
          let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst ele.name_loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd ele.name_loc }
            )	
          in
*)
          <:expr< $lid:ele.name$ >> 
        in
        let loc1 = loc in
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)

        <:class_expr<
        let _ = $g$ # append_page
            ~tab_label:
            (GMisc.label
               ~text:$ast_of_prop_value loc1 ele.props
               Tab_label$ ())
            #coerce $n$#coerce in
        $ce$
        >>
    | _ ->

        let g = 
(* FIXME
          let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst parent.name_loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd parent.name_loc }
            )	
          in
*)
          <:expr< $lid:parent.name$ >> 
        in
        let loc1 = loc in
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)
        let r =
          match pack_met with
            Pack -> ast_of_pack_options_code loc1 <:expr< $g$ # pack >> ele
          | Add -> <:expr< $g$ # add >>
          | Add1 -> <:expr< $g$ # add1 >>
          | Add2 -> <:expr< $g$ # add2 >>
          | Add_with_viewport -> <:expr< $g$ # add_with_viewport >>
          | Set_submenu -> <:expr< $g$ # set_submenu >>
          | Insert_page | No_pack -> g
        in
        let n = 
(* FIXME
          let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst ele.name_loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd ele.name_loc }
            )	
          in
*)
          <:expr< $lid:ele.name$ >> 
        in
        <:class_expr< let _ = $r$ $n$ # coerce in $ce$ >>
  with
    Failure s -> 
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
      in
*)
      prerr_endline s; 
      <:class_expr< let _ = failed in $ce$ >>

(** The accel_group variable name for the given Menubar ele. *)
let accel_group_name ele =
  let accel_group_v = Zog_types.get_prop_value ele.props Accel_group_name in
  let name =
    match remove_blanks accel_group_v with
      "" -> "accel_" ^ ele.name
    | s -> s
  in
  name

(** Output the OCaml for the given menu_item (or check or radio)
   and its optional submenu, to perform after creation initializations
   like adding accelerators and fixing some properties. *)
let rec ast_of_post_menu_item_creation_code loc accel_name ele ce =
  let keysym_v = Zog_types.get_prop_value ele.props Accel_keysym in
  let ce =
    List.fold_right (ast_of_post_menu_creation_code loc accel_name)
      ele.children ce
  in
  match remove_blanks keysym_v with
    "" -> ce
  | v ->
      let modifier =
        match
          remove_blanks (Zog_types.get_prop_value ele.props Accel_modifier)
        with
          "" -> 
(*
            let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
            )	
            in
*)
            <:expr< [] >>
        | s -> parse_string loc s
      in
      let flags =
        match
          remove_blanks (Zog_types.get_prop_value ele.props Accel_flags)
        with
          "" ->
(*
            let loc = 
            (
             { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
             { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
            )	
            in
*)
            <:expr< [] >>
        | s -> parse_string loc s
      in
      let loc1 = loc in
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
        )	
      in
*)
      <:class_expr<
        let _ =
          $lid:ele.name$ #add_accelerator ~group: $lid:accel_name$
             ~modi: $modifier$ ~flags: $flags$ $parse_string loc1 v$
        in
        $ce$ >>

and ast_of_post_menu_creation_code loc accel_name ele ce =
  match ele.classe with
    Menu ->
      let ce =
        List.fold_right (ast_of_post_menu_item_creation_code loc accel_name)
          ele.children ce
      in
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
        )	
      in
*)
      <:class_expr<
         let _ = $lid:ele.name$ #set_accel_group $lid:accel_name$ in $ce$ >>
  | _ -> ce

(** Output the OCaml for the given element which must be a Menubar. *)
let ast_of_post_menubar_creation_code ?win loc ele ce =
  match ele.classe with
    Menubar ->
      let acc_name = accel_group_name ele in
      let ce =
        List.fold_right (ast_of_post_menu_item_creation_code loc acc_name)
          ele.children ce
      in
      let ce2 = 
        (
         match win with
           None -> ce
         | Some w -> 
(*
             let loc = 
               (
                { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
                { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
               )	
             in
*)
             <:class_expr<
             let _ = $lid:w$#add_accel_group $lid:acc_name$ in $ce$
             >>
        )      
      in
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
        )	
      in      
*)
      <:class_expr<
      let $lid:acc_name$ = GtkData.AccelGroup.create () in 
      $ce2$
      >>

  | _ -> ce

let rec ast_of_ele_creations ?win loc parent_opt previous_opt ele ce =
  let ce = ast_of_post_menubar_creation_code ?win loc ele ce in
  let ce =
    let rec iter prev ce =
      function
        [] -> ce
      | e :: q ->
          let ce = iter (Some e) ce q in
          ast_of_ele_creations ?win loc (Some ele) prev e ce
    in
    iter None ce ele.children
  in
  let e =
    if ele.classe = Custom_box then ast_of_prop_value loc ele.props Function
    else
      let f = ast_of_class_info ele.name_loc ele.classe in
      let f = ast_of_creation_options_code loc ele f in
      let f =
        match ele.classe, previous_opt with
          Radio_menu_item, Some e when e.classe = Radio_menu_item ->
(*
            let loc = 
              (
               { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
               { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
              )	
            in
*)
            <:expr< $f$ ~group: $lid:e.name$ #group >>
        | _ -> f
      in
      match parent_opt with
        None -> f
      | Some parent -> ast_of_pack_code loc parent ele f
  in
  let ce =
    if ele.classe = Custom_box then
      match parent_opt with
        None -> ce
      | Some parent -> ast_of_custom_pack_code loc parent ele ce
    else ce
    in
(* BEGIN CDK *)
  gen_anonynous_name ele;
  if ele.name = "_" then  
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)
        <:class_expr< let _ = $e$ () in $ce$  >>
    else
    (* END CDK *)
  let n = 
(* FIXME
    let loc = 
      (
       { Lexing.dummy_pos with Lexing.pos_cnum = fst ele.name_loc } ,
       { Lexing.dummy_pos with Lexing.pos_cnum = snd ele.name_loc }
          )	
    in 
*)
    <:patt< $lid:ele.name$ >> 
  in
(*
  let loc = 
    (
     { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
     { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
    )	
  in
*)
  <:class_expr< let $n$ = $e$ () in $ce$ >>

  let rec ast_of_ele_methods loc ele =
    let cil =
      if ele.classe = Menubar then
        let accel_name = accel_group_name ele in
(*
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
        in
*)
        [ <:class_str_item< method $accel_name$ = $lid:accel_name$ >> ]
      else []
    in
(* BEGIN CDK *)
    gen_anonynous_name ele;
    let cil = if ele.name.[0] <> '_' then 
      let n = 
(* FIXME
        let loc = 
          (
           { Lexing.dummy_pos with Lexing.pos_cnum = fst ele.name_loc } ,
           { Lexing.dummy_pos with Lexing.pos_cnum = snd ele.name_loc }
          )	
        in
*)
        <:expr< $lid:ele.name$ >> 
      in
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
        )	
      in
*)
      <:class_str_item< method $ele.name$ = $n$ >> :: cil else cil in
(* END CDK *)  
    List.fold_left (fun cil ele -> cil @ ast_of_ele_methods loc ele) cil
      ele.children
  
let rec ast_of_ele_vals loc ele =
  let cil =
    if ele.classe = Menubar then
      let accel_name = accel_group_name ele in
(*
      let loc = 
        (
         { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
         { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
        )	
      in
*)
      [ <:class_str_item< value $accel_name$ = $lid:accel_name$ >> ]
    else []
  in
(* BEGIN CDK *)
  gen_anonynous_name ele;
  let n = 
(* FIXME
    let loc = 
      (
       { Lexing.dummy_pos with Lexing.pos_cnum = fst ele.name_loc } ,
       { Lexing.dummy_pos with Lexing.pos_cnum = snd ele.name_loc }
      )	
    in
*)
    <:expr< $lid:ele.name$ >> 
  in
  let cil = if ele.name.[0] <> '_' then 
(*
    let loc = 
      (
       { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
       { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
      )	
    in
*)
    <:class_str_item< value $ele.name$ = $n$ >> :: cil  
   else cil 
  in
(* END CDK *)  
  List.fold_left (fun cil ele -> cil @ ast_of_ele_vals loc ele) cil
    ele.children

let ast_of_entity loc entity =
  let ce =
    let cil =
      match entity.en_ele with
        None -> []
      | Some e ->
          let cil = ast_of_ele_vals loc e @ ast_of_ele_methods loc e in
          match e.classe with
            Window -> cil
          | _ ->
              let n = 
(* FIXME
                let loc = 
                  (
                   { Lexing.dummy_pos with Lexing.pos_cnum = fst e.name_loc } ,
                   { Lexing.dummy_pos with Lexing.pos_cnum = snd e.name_loc }
                  )	
                in
*)
    <:expr< $lid:e.name$ >> 
              in
(*
              let loc = 
                (
                 { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
                 { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
                )	
              in
*)
              let ci = <:class_str_item< method coerce = $n$ # coerce >> in
              cil @ [ci]
    in
(*
    let loc = 
      (
       { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
       { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
      )	
    in
*)
    <:class_expr@loc< object (*$None$*) $list:cil$ end >>
  in
  let ce =
    match entity.en_ele with
      None -> ce
    | Some ele ->
        let win_opt = 
          match ele.classe with
            Window -> Some ele.name
          | _ -> None
        in
        ast_of_ele_creations ?win: win_opt loc None None ele ce
  in
(*
  let loc = 
    (
     { Lexing.dummy_pos with Lexing.pos_cnum = fst loc } ,
     { Lexing.dummy_pos with Lexing.pos_cnum = snd loc }
          )	
  in
*)
  let ce =
    List.fold_right (fun p ce -> <:class_expr< fun $lid:p$ -> $ce$ >>)
      entity.en_params <:class_expr< fun () -> $ce$ >>
  in
  <:str_item@loc< class $lid:entity.en_name$ = $ce$ >>

(* Parser for Zoggy input (XML) *)

(* let gram = Grammar.create (Plexer.make ()) *)
(* let project = Gram.Entry.mk gram "project" *)
;;
Gram.Entry.clear implem;;

let field_error s =
  Printf.eprintf "Error: field %s is empty\n" s

let eoi : 'eoi Gram.Entry.t = Gram.Entry.mk "eoi"
let () = 
  Gram.extend (eoi : 'eoi Gram.Entry.t)
   ((fun () ->
       (None,
        [ (None, None,
           [ ([ Gram.Stoken
                  (((function | EOI -> true | _ -> false),
                    "EOI")) ],
              (Gram.Action.mk
                 (fun (__camlp4_0 : Gram.Token.t) (loc : Gram.Loc.t)
                    ->
                    match __camlp4_0 with
                    | EOI -> (() : 'eoi)
                    | _ -> assert false))) ]) ]))
      ())

  EXTEND Gram
(*     GLOBAL: project; *)
    GLOBAL: implem;
    implem: 
      [ [ el = LIST0 entity ; eoi -> el, None ] ]
    ;
(*     eoi: [[ EOI -> () ]]; *)
    entity:
      [ [ "<"; LIDENT "entity"; LIDENT "name"; "="; name = LIDENT;
          pl = LIST0 [ x = LIDENT -> x ]; ">"; w = OPT widget; "</"; LIDENT "entity";
          ">" ->
            try
              let entity = {en_name = name; en_params = pl; en_ele = w} in
              (
(* 	       let loc = ((fst loc).Lexing.pos_cnum, (snd loc).Lexing.pos_cnum) in *)
               ast_of_entity loc entity
              )
            with Field_error m ->
              field_error m;
              exit 1
        ] ]
    ;
    widget:
      [ [ "<"; tag = LIDENT; LIDENT "name"; "="; (name, nloc) = ident;
          proplist = LIST0 property; ">"; children = LIST0 widget;
          (tag_end, loc_tend) = tag_end ->
            if tag <> tag_end then
              Loc.raise loc_tend
                (Stream.Error ("</" ^ tag ^ "> expected"));
            let proplist =
              List.filter (fun (x, v, vloc) -> x <> "expanded" && x <> "in_interface") proplist
            in
            let proplist =
              List.map
                (fun (x, v, vloc) ->
                   {prop_kind = Zog_misc.property_kind_of_property_name x;
                    prop_value = Zog_misc.decode v; 
                     prop_value_loc = vloc})
(*          ((fst vloc).Lexing.pos_cnum, (snd vloc).Lexing.pos_cnum)}) *)
                proplist
            in
(* 	    let nloc = ((fst loc).Lexing.pos_cnum, (snd loc).Lexing.pos_cnum) in *)
            { name = name; name_loc = loc;
              classe = Zog_misc.class_of_class_name tag; 
              props = proplist;
              children = children; 
              expanded = false ;
            } ] ]
    ;
    tag_end:
      [ [ "</"; tag = LIDENT; ">" -> tag, loc ] ]
    ;
    property:
      [ [ x = LIDENT; "="; (v, vloc) = string -> x, v, vloc 
        | "function"; "="; (v, vloc) = string -> "function", v, vloc ]
      ]
    ;
    string:
      [ [ v = STRING -> v,  loc ] ]
    ;
    ident:
      [ 
        [ v = LIDENT -> v, loc
        | "_" -> "_", loc ]
      ]
    ;
  END

end

module Id = struct
let version = "0"
let name = "Zoggy"
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Zoggy)

(* let _ = Pcaml.parse_implem := Grammar.Entry.parse project *)
