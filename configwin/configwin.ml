(* Copyright 2001, Maxence Guesdon, INRIA Rocquencourt, FRANCE *)
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

type string_param =
    Configwin_types.string_param = {
    string_label : string;
    mutable string_value : string;
    string_editable : bool;
    string_f_apply : string -> unit;
  } 
type bool_param =
    Configwin_types.bool_param = {
    bool_label : string;
    mutable bool_value : bool;
    bool_editable : bool;
    bool_f_apply : bool -> unit;
  } 
type strings_param =
    Configwin_types.strings_param = {
    strings_label : string;
    mutable strings_value : string list;
    strings_editable : bool;
    strings_f_add : unit -> string list;
    strings_f_apply : string list -> unit;
    } 
type combo_param =
    Configwin_types.combo_param = {
    combo_label : string;
    mutable combo_value : string;
    combo_choices : string list;
    combo_editable : bool;
    combo_blank_allowed : bool;
    combo_new_allowed : bool;
    combo_f_apply : string -> unit;
  } 
type custom_param =
    Configwin_types.custom_param = {
    custom_box : GPack.box;
    custom_f_apply : unit -> unit;
    custom_expand : bool;
  } 
type color_param =
    Configwin_types.color_param = {
    color_label : string;
    mutable color_value : string;
    color_editable : bool;
    color_f_apply : string -> unit;
  } 
type filenames_param =
    Configwin_types.filenames_param = {
    files_label : string;
    mutable files_value : string list;
    files_editable : bool;
    files_f_apply : string list -> unit;
  } 
type parameter_kind =
    Configwin_types.parameter_kind =
    String_param of string_param
  | Strings_param of strings_param
  | Filename_param of string_param
  | Bool_param of bool_param
  | Text_param of string_param
  | Combo_param of combo_param
  | Custom_param of custom_param
  | Color_param of color_param
  | Filenames_param of filenames_param
type configuration_structure =
    Configwin_types.configuration_structure =
    Section of string * parameter_kind list
  | Section_list of string * configuration_structure list
type return_button =
    Configwin_types.return_button =
    Return_apply
  | Return_ok
  | Return_cancel
      
let edit = Configwin_ihm.edit 

let get = Configwin_ihm.edit ~with_apply: false

let simple_edit = Configwin_ihm.simple_edit

let simple_get = Configwin_ihm.simple_edit ~with_apply: false
