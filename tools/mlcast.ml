open Unix
  
let mgroup = 1
let mport = 4000

let _ =
  let buffer = String.create 2000 in
  let sock = Multicast.server mgroup mport in
  Multicast.send sock mgroup mport (Sys.argv.(1));
  while true do
    let len,_ = Multicast.recv sock buffer in
    Printf.printf "RECEIVED: %s" (String.escaped (String.sub buffer 0 len));
    print_newline ();
    sleep 1;
    Multicast.send sock mgroup mport (Sys.argv.(1));
  done
  
  