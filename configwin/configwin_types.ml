(***********************************************************************)
(*                          Configwin                                  *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** This module contains the types used in Configwin. *)

(** This type represents a string or filename parameter. *)
type string_param = {
    string_label : string; (** the label of the parameter *)
    mutable string_value : string; (** the current value of the parameter *)
    string_editable : bool ; (** indicates if the value can be changed *)
    string_f_apply : (string -> unit) ; (** the function to call to apply the new value of the parameter *)
    string_help : string option ; (** optional help string *)
  } ;;

(** This type represents a boolean parameter. *)
type bool_param = {
    bool_label : string; (** the label of the parameter *)
    mutable bool_value : bool; (** the current value of the parameter *)
    bool_editable : bool ; (** indicates if the value can be changed *)
    bool_f_apply : (bool -> unit) ; (** the function to call to apply the new value of the parameter *)
    bool_help : string option ; (** optional help string *)
  } ;;

(** This type represents a parameter whose value is a list of ['a]. *)
type 'a list_param = {
    list_label : string; (** the label of the parameter *)
    mutable list_value : 'a list; (** the current value of the parameter *)
    list_titles : string list option; (** the titles of columns, if they must be displayed *)
    list_f_edit : ('a -> 'a) option; (** optional edition function *)
    list_eq : ('a -> 'a -> bool) ; (** the comparison function used to get list without doubles *)
    list_strings : ('a -> string list); (** the function to get a string list from a ['a]. *)
    list_color : ('a -> string option) ; (** a function to get the optional color of an element *)
    list_editable : bool ; (** indicates if the value can be changed *)
    list_f_add : unit -> 'a list ; (** the function to call to add list *)
    list_f_apply : ('a list -> unit) ; (** the function to call to apply the new value of the parameter *)
    list_help : string option ; (** optional help string *)
  } ;;

type combo_param = {
    combo_label : string ;
    mutable combo_value : string ;
    combo_choices : string list ;
    combo_editable : bool ;
    combo_blank_allowed : bool ;
    combo_new_allowed : bool ;
    combo_f_apply : (string -> unit);
    combo_help : string option ; (** optional help string *)
  } ;;

type custom_param = {
    custom_box : GPack.box ;
    custom_f_apply : (unit -> unit) ;
    custom_expand : bool ;
  } ;;

type color_param = {
    color_label : string; (** the label of the parameter *)
    mutable color_value : string; (** the current value of the parameter *)
    color_editable : bool ; (** indicates if the value can be changed *)
    color_f_apply : (string -> unit) ; (** the function to call to apply the new value of the parameter *)
    color_help : string option ; (** optional help string *)
  } ;;

type date_param = {
    date_label : string ; (** the label of the parameter *)
    mutable date_value : int * int * int ; (** day, month, year *)
    date_editable : bool ; (** indicates if the value can be changed *)
    date_f_string : (int * int * int) -> string ;
      (** the function used to display the current value (day, month, year) *)
    date_f_apply : ((int * int * int) -> unit) ;
      (** the function to call to apply the new value (day, month, year) of the parameter *)
    date_help : string option ; (** optional help string *)
  } ;;

type font_param = {
    font_label : string ; (** the label of the parameter *)
    mutable font_value : string ; (** the font name *)
    font_editable : bool ; (** indicates if the value can be changed *)
    font_f_apply : (string -> unit) ;
      (** the function to call to apply the new value of the parameter *)
    font_help : string option ; (** optional help string *)
  } ;;

(** This type represents the different kinds of parameters. *)
type parameter_kind =
    String_param of string_param
  | List_param of (unit -> <box: GObj.widget ; apply : unit>)
  | Filename_param of string_param
  | Bool_param of bool_param
  | Text_param of string_param
  | Combo_param of combo_param
  | Custom_param of custom_param
  | Color_param of color_param
  | Date_param of date_param
  | Font_param of font_param
;;

(** This type represents the structure of the configuration window. *)
type configuration_structure =
  | Section of string * parameter_kind list (** label of the section, parameters *)
  | Section_list of string * configuration_structure list (** label of the section, list of the sub sections *)
;;

(** To indicate what button was pushed by the user when the window is closed. *)
type return_button =
    Return_apply (** The user clicked on Apply at least once before
	     closing the window with Cancel or the window manager. *)
  | Return_ok (** The user closed the window with the ok button. *)
  | Return_cancel (** The user closed the window with the cancel
		     button or the window manager but never clicked
		     on the apply button.*)
