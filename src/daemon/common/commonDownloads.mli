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

module Basic :
  sig
    type ('a, 'b) download = {
      download_file : 'a;
      download_client : 'b;
      mutable download_min_read : int;
      mutable download_pos : int64;
      mutable download_sock : TcpBufferedSocket.t option;
    }
    module Make :
      functor
        (M : sig
               type f
               type c
               val file : f -> CommonTypes.file
               val client : c -> CommonTypes.client
               val client_disconnected : (f, c) download -> unit
               val download_finished : (f, c) download -> unit
             end) ->
        sig
          val disconnect_download :
            (M.f, M.c) download -> BasicSocket.close_reason -> unit
          val file_complete : (M.f, M.c) download -> unit
          val download_reader :
            (M.f, M.c) download -> TcpBufferedSocket.t -> int -> unit
          val new_download :
            TcpBufferedSocket.t -> M.c -> M.f -> int -> (M.f, M.c) download
        end
  end
