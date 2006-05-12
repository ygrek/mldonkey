(* Copyright 2006 *)
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

open Magiclib

module type MagicInfo =
sig
  val magic_works : unit -> bool

(* magic_fileinfo <file> <return mime value true/false> <libmagic result, None for exceptions> *)
  val magic_fileinfo : string -> bool -> string option
end

module MagicInfo : MagicInfo = struct

  let magic_cookie () =
    Magiclib.create ?flags:(Some [Symlink;Preserve_atime]) []
  let magic_cookie_mime () =
    Magiclib.create ?flags:(Some [Mime;Symlink;Preserve_atime]) []

  let magic_works () =
    try
      let cookie = magic_cookie_mime () in
      Magiclib.close cookie;
      true
    with e -> false

  let magic_fileinfo file mime =
    try
      let cookie =
        if mime then magic_cookie_mime () else magic_cookie ()
      in
      try
        let fileinfo = Magiclib.file cookie file in
        Magiclib.close cookie;
        Some fileinfo
      with e ->
        Magiclib.close cookie;
        None
    with e -> None

end
