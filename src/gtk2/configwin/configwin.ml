(**************************************************************************)
(*  Copyright 2003, 2002 b8_bavard, b8_zoggy, , b52_simon INRIA            *)
(*                                                                        *)
(*    This file is part of mldonkey.                                      *)
(*                                                                        *)
(*    mldonkey is free software; you can redistribute it and/or modify    *)
(*    it under the terms of the GNU General Public License as published   *)
(*    by the Free Software Foundation; either version 2 of the License,   *)
(*    or (at your option) any later version.                              *)
(*                                                                        *)
(*    mldonkey is distributed in the hope that it will be useful,         *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU General Public License for more details.                        *)
(*                                                                        *)
(*    You should have received a copy of the GNU General Public License   *)
(*    along with mldonkey; if not, write to the Free Software             *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston,               *)
(*    MA  02111-1307  USA                                                 *)
(*                                                                        *)
(**************************************************************************)

type parameter_kind = Configwin_types.parameter_kind

type configuration_structure =
    Configwin_types.configuration_structure =
    Section of string * parameter_kind list
  | Section_list of string * configuration_structure list

type return_button =
    Configwin_types.return_button =
    Return_apply
  | Return_ok
  | Return_cancel


let string = Configwin_ihm.string
let password = Configwin_ihm.password
let text = Configwin_ihm.text
let strings = Configwin_ihm.strings
let list = Configwin_ihm.list
let bool = Configwin_ihm.bool
let filename = Configwin_ihm.filename
let filenames = Configwin_ihm.filenames
let color = Configwin_ihm.color
let font = Configwin_ihm.font
let combo = Configwin_ihm.combo
let custom = Configwin_ihm.custom
let date = Configwin_ihm.date

let edit = Configwin_ihm.edit 

let get = Configwin_ihm.edit ~with_apply: false

let simple_edit = Configwin_ihm.simple_edit

let simple_get = Configwin_ihm.simple_edit ~with_apply: false

let box = Configwin_ihm.box
