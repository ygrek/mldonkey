{

(***********************************************************************)
(*                                                                     *)
(*                             xlib                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

module Xtypes = struct

type size = int
type colordef = RGB of int * int * int | NoColor | Color of string
  type coord = int

end

open Xtypes

  exception BadFile of string * int
(* To buffer string literals *)

  let initial_string_buffer = String.create 256
  let string_buff = ref initial_string_buffer
  let string_index = ref 0
  let string_start_pos = ref 0
  
  let reset_string_buffer () =
    string_buff := initial_string_buffer;
    string_index := 0
  
  let store_string_char c =
    if !string_index >= String.length (!string_buff) then begin
        let new_buff = String.create (String.length (!string_buff) * 2) in
        String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
        string_buff := new_buff
      end;
    String.unsafe_set (!string_buff) (!string_index) c;
    incr string_index
  
  let get_stored_string () =
    let s = String.sub (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s
    
  let code = ref ""
  let read = ref 0
  
  let width = ref 0
  let height = ref 0
  let hot_x = ref 0
  let hot_y = ref 0
  
  let hexcode lexbuf i = 
    let c = Char.lowercase (Lexing.lexeme_char lexbuf i) in
    if c>= '0' && c <='9' then (Char.code c) - (Char.code '0')
    else
    if c>= 'a' && c <= 'f' then
      Char.code c - Char.code 'a' + 10
    else
      raise (BadFile ("hexcode",Lexing.lexeme_start lexbuf))

}

let hex = [ '0'-'9' 'a'-'z' 'A'-'Z']

rule xpm_line = parse
    "\""
    { 
    reset_string_buffer();
    let string_start = Lexing.lexeme_start lexbuf in
    string_start_pos := string_start;
    string lexbuf;
    lexbuf.Lexing.lex_start_pos <-
      string_start - lexbuf.Lexing.lex_abs_pos;
    get_stored_string() }
  | eof { raise (BadFile ("",Lexing.lexeme_start lexbuf))}
  | _ { xpm_line lexbuf }

and string = parse
    '"'
    { () }
  | eof
      { raise (BadFile ("",Lexing.lexeme_start lexbuf))    }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
      string lexbuf }

and first_line = parse
    "\"" { 
    let dx = nombre lexbuf in
    let dy = nombre lexbuf in
    let colors = nombre lexbuf in
    let nchars = nombre lexbuf in
    ending_string lexbuf;
    (dx,dy,colors,nchars)
  }
  | "/*" { comment lexbuf; first_line lexbuf }
  | eof { raise (BadFile ("",Lexing.lexeme_start lexbuf)) }
  | _ { first_line lexbuf }  

and nombre = parse
    [ ' ' '\n' '\009' '\010' '\012'] { nombre lexbuf }
  | [ '0' - '9' ] + { int_of_string(Lexing.lexeme lexbuf) }
  | '-' [ '0' - '9' ] + { int_of_string(Lexing.lexeme lexbuf) }
  | _ { raise (BadFile ("nombre",Lexing.lexeme_start lexbuf)) }

and ending_string = parse
    "\"" { () }
  | _ { ending_string lexbuf }

and comment = parse
    "*/" { () }
  | eof { raise (BadFile ("",Lexing.lexeme_start lexbuf)) }
  | _ { comment lexbuf }

and color_def = parse
    "\"" { read_chars lexbuf; read_c lexbuf; 
    let color = read_color lexbuf in ending_string lexbuf;
    color }
  | "/*" { comment lexbuf; color_def lexbuf }
  | _ { color_def lexbuf }

and read_chars = parse
    [ ^ '"'] { 
    !code.[String.length !code - !read] <- Lexing.lexeme_char lexbuf 0;
    decr read;
    if !read > 0 then read_chars lexbuf
  } 
  | _ { raise (BadFile ("chars",Lexing.lexeme_start lexbuf)) }

and read_c = parse
    [' ' '\009'] 'c' [' ' '\009'] { () }
  | [ ^ '"'] { read_c lexbuf }
  | _ { raise (BadFile (" c ",Lexing.lexeme_start lexbuf)) }

and read_color = parse
    '#' hex hex hex hex hex hex hex hex hex hex hex hex { 
    let hex pos = hexcode lexbuf pos * 16 + hexcode lexbuf (pos+1) in
    let r = hex 1 * 256 + hex 3 in
    let g = hex 5 * 256 + hex 7 in
    let b = hex 9 * 256 + hex 11 in
    RGB(r,g,b)    
  }
|    '#' hex hex hex hex hex hex  { 
    let hex pos = hexcode lexbuf pos * 16 + hexcode lexbuf (pos+1) in
    let r = (hex 1) * 256  in
    let g = (hex 3) * 256  in
    let b = (hex 5) * 256 in
    RGB(r,g,b)    
  }
  
  |  ['n' 'N'] ['o' 'O'] ['n' 'N'] ['e' 'E'] { NoColor }
  |  ['A'-'Z' 'a'-'z' '_' '0'-'9'] + { Color(Lexing.lexeme lexbuf) }
  |  [ ' ' '\n' '\009' '\010' '\012'] { read_color lexbuf } 
  | _ { raise (BadFile ("color",Lexing.lexeme_start lexbuf)) }

and xbm_file = parse
    "/*" { comment lexbuf; xbm_file lexbuf }
  | "_width" { width := nombre lexbuf;
      xbm_file lexbuf }
  | "_height" { height := nombre lexbuf;
      xbm_file lexbuf }
  | "_x_hot" { hot_x := nombre lexbuf;
      xbm_file lexbuf }
  | "_y_hot" { hot_y := nombre lexbuf;
      xbm_file lexbuf }
  | '{' { 
      let len = (max (!width / 8) 1) * !height  in
      code := String.create len;
      read := len;
      read_bits lexbuf
    }
  | eof { raise (BadFile ("looking for {",Lexing.lexeme_start lexbuf)) }
  | _ { xbm_file lexbuf }

and read_bits = parse
    "/*" { comment lexbuf; xbm_file lexbuf }
  | "0x" _ _ { 
      !code.[String.length !code - !read] <- 
        Char.chr (hexcode lexbuf 2 * 16 + hexcode lexbuf 3);
      decr read;
      if !read > 0 then read_bits lexbuf }
  | '}' | eof {  raise (BadFile ("looking for 0x",Lexing.lexeme_start lexbuf)) }
  | _ { read_bits lexbuf }

{
type bitmap_data = int * int * int * int * string
type pixmap_data = int * int * Xtypes.colordef array * int array array
(*
type bitmap = Xtypes.size * Xtypes.size * coord * coord * Xtypes.pixmap
type pixmap = Xtypes.size * Xtypes.size * int * Xtypes.pixmap * Xtypes.pixmap
*)

let readPixmapDataFromFile filename =
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let (dx,dy,ncolors,nchars) = first_line lexbuf in
    let colors = Array.create ncolors NoColor in
    let codes = Hashtbl.create ncolors in
    for i = 0 to ncolors - 1 do
      code := String.create nchars;
      read := nchars;
      colors.(i) <- color_def lexbuf;
      Hashtbl.add codes !code i;
    done;
    let table = Array.init dy (fun _ -> Array.create dx 0) in
    let code = String.create nchars in
    for y = 0 to dy - 1 do
      let line = xpm_line lexbuf in
      if String.length line <> dx * nchars then raise (BadFile ("size",Lexing.lexeme_start lexbuf));
      for x = 0 to dx - 1 do
        for c = 0 to nchars - 1 do
          code.[c] <- line.[x*nchars+c]
        done;
        table.(y).(x) <- Hashtbl.find codes code
      done
    done;
    close_in ic;
    (dx,dy,colors, table)
  with
    _ -> close_in ic; raise (BadFile ("readPixmapDataFromFile",0))

let readBitmapDataFromFile filename =
  width := 0;
  height := 0;
  hot_x := 0;
  hot_y := 0;
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let _ = xbm_file lexbuf in
    close_in ic;
    (!width,!height,!hot_x, !hot_y, !code)
  with
    e -> close_in ic; raise (BadFile ("readBitmapDataFromFile",0))
      
let test f = try f () with e -> lprintf "-"; raise e  
  
let data_to_string data =
  let s = ref "\n[|" in
  for j = 0 to Array.length data - 1 do
    let line = data.(j) in
    s := !s ^ (if j > 0 then ";[|" else " [|");
    for i = 0 to Array.length line - 1 do
      s := Printf.sprintf "%s%s%d" !s (if i>0 then ";" else "") line.(i)
    done;
    s := !s ^ "|]\n"
  done;
  s := !s ^ " |]"; !s

let colors_to_string colors =
  let s = ref "[|" in
  for i = 0 to Array.length colors - 1 do
    s := (match colors.(i) with
      RGB(r,g,b) -> Printf.sprintf "%s%sXtypes.RGB(%d,%d,%d)\n"
          !s (if i>0 then ";" else "")
          r g b
    | Color n -> Printf.sprintf "%s%sXtypes.Color(\"%s\")\n"
          !s (if i>0 then ";" else "")
          n
    | NoColor -> Printf.sprintf "%s%sXtypes.NoColor\n"
          !s (if i>0 then ";" else ""))
  done;
  s := !s ^ " |]"; !s

let createMLStringFromPixmapData pixmap pixmap_name =
  let (dx,dy,colors,data) = pixmap in
  Printf.sprintf "let %s = (%d,%d,\n(%s),\n(%s))\n" pixmap_name dx dy 
    (colors_to_string colors) (data_to_string data)

let _ =
  let oc = open_out (Sys.argv.(1) ^ ".ml") in
  output_string oc (createMLStringFromPixmapData 
(readPixmapDataFromFile Sys.argv.(2))
(Filename.basename Sys.argv.(1)));
  close_out oc

  (*

  
open Xtypes
open X
open Xlib  
open Xutils
  
let display = openDisplay ""
let screen = display.dpy_roots.(0)
let root = screen.scr_root
let white = Xutils.defaultWhite display
let black = Xutils.defaultBlack display
let cmap = Xutils.defaultColormap display
let depth = Xutils.defaultDepth display
  

let gc = X.createGC display root [GCforeground black; GCbackground white]

let test filename =
  let (dx,dy,pix,_) = createPixmapFromFile display root cmap depth
      filename in
  X.copyArea display gc pix 0 0 root 10 10 dx dy;;
open Xpm;;
let data = readPixmapDataFromFile "xterm.xpm";;   
let s = createMLStringFromPixmapData data "xterm_pix";;
let oc = open_out "/tmp/pix.ml"
let _ = output_string oc s
let _ = close_out oc;;
  #use "/tmp/pix.ml";;

  *)

}

