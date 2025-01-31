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

module TaskQueue = struct
  type 'a t = {
    queue : 'a Queue.t;
    mutex : Mutex.t;
    cond : Condition.t;
  }

  (* Create a new task queue *)
  let create () = {
    queue = Queue.create ();
    mutex = Mutex.create ();
    cond = Condition.create ();
  }

  (* Add a task to the queue *)
  let add t task =
    Mutex.lock t.mutex;
    Queue.add task t.queue;
    Condition.signal t.cond;
    Mutex.unlock t.mutex

  (* Retrieve a task from the queue (blocking if empty) *)
  let take t =
    Mutex.lock t.mutex;
    while Queue.is_empty t.queue do
      Condition.wait t.cond t.mutex
    done;
    let task = Queue.pop t.queue in
    Mutex.unlock t.mutex;
    task
end

(* Thread pool *)
type t = {
  threads : Thread.t list;
  tasks : (unit -> unit) TaskQueue.t;
  stop_flag : bool ref;
}

(* Worker thread function *)
let rec worker_loop tasks stop_flag =
  if !stop_flag then ()
  else
    let task = TaskQueue.take tasks in
    (try task () with _ -> ());
    worker_loop tasks stop_flag

(* Create a thread pool with a fixed number of threads *)
let create num_threads =
  let tasks = TaskQueue.create () in
  let stop_flag = ref false in
  let threads = List.init num_threads (fun _ ->
    Thread.create (fun () -> worker_loop tasks stop_flag) ()
  ) in
  { threads; tasks; stop_flag }

(* Add a task to the thread pool *)
let add_task pool task =
  TaskQueue.add pool.tasks task

(* Stop the thread pool and wait for all threads to finish *)
let stop pool =
  pool.stop_flag := true;
  List.iter Thread.join pool.threads
