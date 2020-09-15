(**************************************************************************)
(*  Copyright (c) 2010 zzz <jian@doatest.com>                             *)
(*                                                                        *)
(*  mlupnp, a ocaml wraps of libminiupnpc & libnatpmp for mldonkey        *)
(*                                                                        *)
(*  Permission is hereby granted, free of charge, to any person           *)
(*  obtaining a copy of this software and associated documentation files  *)
(*  (the "Software"), to deal in the Software without restriction,        *)
(*  including without limitation the rights to use, copy, modify, merge,  *)
(*  publish, distribute, sublicense, and/or sell copies of the Software,  *)
(*  and to permit persons to whom the Software is furnished to do so,     *)
(*  subject to the following conditions:                                  *)
(*                                                                        *)
(*  The above copyright notice and this permission notice shall be        *)
(*  included in all copies or substantial portions of the Software.       *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

(* upnp port map zzz++*)
type ml_port_map = {
        mutable enabled : int;
        mutable intPort : int;
        mutable extPort : int;
        mutable isTcp   : int;
        mutable natpmpStatus : int;
        mutable upnpStatus : int;
        mutable notes   : string;
}

let string_of_map_status status =
  match status with
  | 0 -> "ERROR"
  | 1 -> "UNMAPPED"
  | 2 -> "UNMAPPING"
  | 3 -> "MAPPING"
  | 4 -> "MAPPED"
        | _ -> "N/A"

let strings_port_map map =
        Printf.sprintf "enable:%d ,intPort:%d ,extPort:%d ,isTcp:%d ,natpmpStatus:\"%s\" ,upnpStatus:\"%s\" ,notes:\"%s\""
        map.enabled map.intPort map.extPort map.isTcp (string_of_map_status map.natpmpStatus) (string_of_map_status map.upnpStatus) map.notes
        

external init_maps : unit -> unit = "ml_upnpInitMaps" "noalloc"
(*external set_maps  : unit -> unit = "ml_upnpSetMaps" *)
external dump_maps : unit -> unit = "ml_upnpDumpMaps" "noalloc"
external job_start : unit -> unit = "ml_upnp_job_start" "noalloc"

(* job_stop max_wait_seconds, 0=system default wait seconds *)
external job_stop  : int -> unit = "ml_upnp_job_stop"

external maps_add_item     : int -> int -> int -> int -> string -> unit  = "ml_upnpAddMap" 
external maps_remove_item  : int -> int -> int -> int -> string -> unit  = "ml_upnpRemoveMap"

(* remove_all_maps max_wait_seconds, 0=system default wait seconds *)
external remove_all_maps   : int -> unit = "ml_upnpRemoveAllMaps"

(* maps_get , get all maps info to a list *)
external maps_get  : unit -> ml_port_map list = "ml_upnpGetMaps"
