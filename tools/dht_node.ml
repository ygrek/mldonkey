
open BT_DHT

let run_queries =
  let ids = [|
    "FA959F240D5859CAC30F32ECD21BD89F576481F0";
    "BDE98D04AB6BD6E8EA7440F82870E5191E130A84";
    "857224361969AE12066166539538F07BD5EF48B4";
    "81F643A195BBE3BB1DE1AC9184B9F84D74A37EFF";
    "7CC9963D90B54DF1710469743C1B43E0E20489C0";
    "C2C65A1AA5537406183F4D815C77A2A578B00BFB";
    "72F5A608AFBDF6111E5A86B337E9FC27D6020663";
    "FE73D74660695208F3ACD221B7A9A128A3D36D47";
  |] in
  fun dht ->
  let id = ids.(Random.int (Array.length ids)) in
  log #info "run_queries: start %s" id;
  let id = Kademlia.H.of_hexa id in
  lookup_node dht id (fun nodes ->
    List.iter begin fun node -> 
      log #info "run_queries: found %s" (show_node node);
      M.get_peers dht (snd node) id (fun node token peers nodes ->
        log #info "run_queries: got %d peers and %d nodes from %s with token %S" 
          (List.length peers) (List.length nodes) (show_node node) token) 
        ~kerr:(fun () -> log #info "run_queries: get_peers error from %s" (show_node node))
    end nodes)

let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  try
    match Sys.argv with
    | [|_;storage;port|] ->
      let dht = start storage (int_of_string port) in
      Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> finish dht; exit 0));
      Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> finish dht; exit 0));
      Sys.set_signal Sys.sighup (Sys.Signal_handle (fun _ -> Kademlia.show_table dht.M.rt; show_torrents dht));
      BasicSocket.add_infinite_timer 300. (fun () -> run_queries dht);
(*       let routers = ["router.bittorrent.com", 6881; "router.utorrent.com", 6881;] in *)
      let routers = [Ip.of_string "67.215.242.139", 6881] in
      bootstrap dht ~routers;
      BasicSocket.loop ()
    | _ -> Printf.eprintf "Usage : %s <storage> <port>\n" Sys.argv.(0)
  with
    exn -> log #error "main : %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ())

