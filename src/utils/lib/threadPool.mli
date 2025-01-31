(* Copyright 2025 Luca Carlon *)
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

(** The type of a thread pool. *)
type t

(** [create num_threads] creates a thread pool with [num_threads] worker threads. *)
val create : int -> t

(** [add_task pool task] adds [task] to the task queue of the thread pool [pool].
    The task is a function that takes no arguments and returns [unit]. *)
val add_task : t -> (unit -> unit) -> unit

(** [stop pool] stops all worker threads in the thread pool [pool] and waits for them to finish. *)
val stop : t -> unit