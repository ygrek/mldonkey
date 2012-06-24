(*
 * OCaml-Gd. An interface to the Gd library for generating simple images
 * Based on Shawn Wagner's OCamlGD 0.7.0. with some mods from GD4O 
 * Copyright (C) 2002  Shawn Wagner
 * Copyright (C) 2003  Matthew C. Gushee
 * Copyright (C) 2005  beedauchon 
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

exception Too_many_colors
exception Color_not_found
exception Image_creation_failed 
exception Not_supported
exception Illegal_state of string

let _ = Callback.register_exception "gdopen failed" Image_creation_failed
let _ = Callback.register_exception "gd type not supported" Not_supported

type t (* Image type *)
type c = int (* Color type *)
type font (* Font type *)

class virtual color =
object
  method virtual red_part: int
  method virtual green_part: int
  method virtual blue_part: int
  method virtual code: int
end

class virtual color_allocator =
object
  method virtual create: red:int -> green:int -> blue:int -> color
  method virtual closest: red:int -> green:int -> blue:int -> color
  method virtual closest_hwb: red:int -> green:int -> blue:int -> color
  method virtual resolve: red:int -> green:int -> blue:int -> color
  method virtual exact: red:int -> green:int -> blue:int -> color
  method virtual find: red:int -> green:int -> blue:int -> color
  method virtual white: color
  method virtual black: color
  method virtual blue: color
  method virtual green: color
  method virtual red: color
  method virtual get_transparent: color
  method virtual set_transparent: color -> unit
  method virtual transparent: unit -> int
end

class virtual image =
object
  method virtual colors: color_allocator
  method virtual line: x1:int -> y1:int -> x2:int -> y2:int -> color -> unit
  method virtual dashed_line: x1:int -> y1:int -> x2:int -> y2:int -> color -> unit
  method virtual rectangle: x1:int -> y1:int -> x2:int -> y2:int -> color -> unit
  method virtual filled_rectangle: x1:int -> y1:int -> x2:int -> y2:int -> color -> unit
  method virtual arc: cx:int -> cy:int -> w:int -> h:int -> s:int -> e:int -> color -> unit
  method virtual arc_fill: cx:int -> cy:int -> w:int -> h:int -> s:int -> e:int -> color -> style:int -> unit
  method virtual border_fill: x:int -> y:int -> border:color -> fill:color -> unit
  method virtual fill: x:int -> y:int -> color -> unit
  method virtual save_as_png: string -> unit
  method virtual save_as_jpeg: ?quality:int -> string -> unit
  method virtual out_as_png: out_channel -> unit
  method virtual out_as_jpeg: ?quality:int -> out_channel -> unit
  method virtual set_pixel: x:int -> y:int -> color -> unit
  method virtual get_pixel: x:int -> y:int -> color
  method virtual width: int
  method virtual height: int
  method virtual in_range: x:int -> y:int -> bool
  method virtual letter: font:font -> x:int -> y:int -> c:char -> color -> unit
  method virtual letter_up: font:font -> x:int -> y:int -> c:char -> color -> unit
  method virtual string: font:font -> x:int -> y:int -> s:string -> color -> unit
  method virtual string_up: font:font -> x:int -> y:int -> s:string -> color -> unit
end

(* Private interface routines. *)
(* Create an image *)
external do_image_create: int -> int -> t = "ml_image_create"
external do_image_open_png: string -> t = "ml_image_open_png"
external do_image_open_jpeg: string -> t = "ml_image_open_jpeg"

(* Drawing functions *)        
external do_set_pixel: t -> int -> int -> int -> unit = "ml_set_pixel"

external do_get_pixel: t -> int -> int -> int = "ml_get_pixel"

external do_get_width: t -> int = "ml_get_width"
external do_get_height: t -> int = "ml_get_height"

external do_draw_line: t -> int -> int -> int -> int -> int -> unit
    = "ml_image_line" "ml_image_line_native"

external do_draw_dline: t -> int -> int -> int -> int -> int -> unit
    = "ml_image_dline" "ml_image_dline_native"

external do_draw_rect: t -> int -> int -> int -> int -> int -> unit
    = "ml_image_rect" "ml_image_rect_native"

external do_draw_frect: t -> int -> int -> int -> int -> int -> unit
    = "ml_image_frect" "ml_image_frect_native"

external do_draw_arc: t -> int -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_arc" "ml_image_arc_native"

external do_draw_arc_fill: t -> int -> int -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_arc_fill" "ml_image_arc_fill_native"

external do_border_fill: t -> int -> int -> int -> int -> unit
    = "ml_image_border_fill" "ml_image_border_fill_native"
    
external do_fill: t -> int -> int -> int -> unit 
    = "ml_image_fill"

external do_save_png: t -> string -> unit = "ml_save_png"
external do_save_jpeg: t -> string -> int -> unit = "ml_save_jpeg"

external do_dump_png: t -> out_channel -> unit = "ml_dump_png"
external do_dump_jpeg: t -> out_channel -> int -> unit = "ml_dump_jpeg"


(* External functions related to colors *)
external do_color_create: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_alloc"

external do_find_closest: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_closest"

external do_find_closest_hwb: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_closest_hwb"

external do_find_exact: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_exact"

external do_resolve: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_resolve"

external do_green_part: t -> int -> int = "ml_image_green_part"
external do_red_part: t -> int -> int = "ml_image_red_part"
external do_blue_part: t -> int -> int = "ml_image_blue_part"
external do_get_transparent: t -> int = "ml_image_get_transparent"
external do_set_transparent: t -> int -> unit = "ml_image_set_transparent"

external do_get_font: int -> font = "ml_get_font"

external do_draw_char: t -> font -> int -> int -> char -> int -> unit
    = "ml_image_char" "ml_image_char_native"

external do_draw_charu: t -> font -> int -> int -> char -> int -> unit
    = "ml_image_charu" "ml_image_charu_native"

external do_draw_str: t -> font -> int -> int -> string -> int -> unit
    = "ml_image_str" "ml_image_str_native"

external do_draw_stru: t -> font -> int -> int -> string -> int -> unit
    = "ml_image_stru" "ml_image_stru_native"

external png_version : unit -> int32 = "ml_image_pngversion"

module Font =
struct
  let tiny = do_get_font 0
  let small = do_get_font 1
  let medium = do_get_font 2
  let large = do_get_font 3
  let giant = do_get_font 4
end


(* Implementation classes *)
class gdColor im col =
object
  inherit color
  method code = col
  method blue_part = do_blue_part im col
  method red_part = do_red_part im col
  method green_part = do_green_part im col
end
      
class gd_color_allocator im = 
object (this)
  inherit color_allocator

  val mutable transparent_pcolor = true
    
  method create ~red ~green ~blue =
    let color = do_color_create im ~red ~green ~blue in
      if color = -1 then raise Too_many_colors else new gdColor im color

  method closest ~red ~green ~blue =
    let color = do_find_closest im ~red ~green ~blue in
      if color = -1 then raise Color_not_found else new gdColor im color

  method closest_hwb ~red ~green ~blue =
    let color = do_find_closest_hwb im ~red ~green ~blue in
      if color = -1 then raise Color_not_found else new gdColor im color

  method exact ~red ~green ~blue =
    let color = do_find_exact im ~red ~green ~blue in
      if color = -1 then raise Color_not_found else new gdColor im color

  method resolve ~red ~green ~blue =
    let color = do_resolve im ~red ~green ~blue in
      if color = -1 then raise Color_not_found else new gdColor im color

  method find ~red ~green ~blue =
    let color = do_find_exact im ~red ~green ~blue in
      if color <> -1 then
        new gdColor im color
      else
        let color = do_color_create im ~red ~blue ~green in
          if color = -1 then raise Too_many_colors else new gdColor im color

  method black = this#find ~red:0 ~blue:0 ~green:0
  method white = this#find ~red:255 ~blue:255 ~green:255
  method blue = this#find ~blue:255 ~red:0 ~green:0
  method green = this#find ~green:255 ~red:0 ~blue:0
  method red = this#find ~red:255 ~green:0 ~blue:0
  method get_transparent =
    let cindex = do_get_transparent im in
    if cindex = -1 then raise Color_not_found 
    else new gdColor im cindex
  method set_transparent color =
    do_set_transparent im color#code
  method transparent () =
    if transparent_pcolor then 5
    else raise (Illegal_state 
      "Transparent pseudocolor is disabled.")
end

class gdImage im =
object
  inherit image
  method private get_image = im
  val c_a = new gd_color_allocator im
  method colors = c_a

  method line ~x1 ~y1 ~x2 ~y2 color =
    do_draw_line im x1 y1 x2 y2 color#code

  method dashed_line ~x1 ~y1 ~x2 ~y2 color =
    do_draw_dline im x1 y1 x2 y2 color#code

  method rectangle ~x1 ~y1 ~x2 ~y2 color =
    do_draw_rect im x1 y1 x2 y2 color#code

  method filled_rectangle ~x1 ~y1 ~x2 ~y2 color =
    do_draw_frect im x1 y1 x2 y2 color#code

  method arc ~cx ~cy ~w ~h ~s ~e color =
    do_draw_arc im cx cy w h s e color#code

  method arc_fill ~cx ~cy ~w ~h ~s ~e color ~style =
    do_draw_arc_fill im cx cy w h s e color#code style

  method border_fill ~x ~y ~border ~fill =
    do_border_fill im x y (border#code) (fill#code)

  method fill ~x ~y color =
    do_fill im x y color#code

  method letter ~font ~x ~y ~c color =
    do_draw_char im font x y c color#code

  method letter_up ~font ~x ~y ~c color =
    do_draw_charu im font x y c color#code

  method string ~font ~x ~y ~s color =
    do_draw_str im font x y s color#code

  method string_up ~font ~x ~y ~s color =
    do_draw_stru im font x y s color#code

  method save_as_png filename = do_save_png im filename
  method save_as_jpeg ?(quality = -1) filename =
    do_save_jpeg im filename quality

  method out_as_png channel = do_dump_png im channel
  method out_as_jpeg ?(quality = -1) channel =
    do_dump_jpeg im channel quality

  method set_pixel ~x ~y color =
    do_set_pixel im x y color#code

  method get_pixel ~x ~y =
    new gdColor im (do_get_pixel im x y)

  method width = do_get_width im
  method height = do_get_height im

  method in_range ~x ~y =
    x >= 0 && x <= (do_get_width im) && y >= 0 && y <= (do_get_height im)
end

(* Image creation functions *)
let create ~(x:int) ~(y:int) = 
  new gdImage (do_image_create x y)

let open_png filename =
  new gdImage (do_image_open_png filename)

let open_jpeg filename =
  new gdImage (do_image_open_jpeg filename)
