open Unix
  
let mgroup = 1
let mport = 4000

let _ =
  let buffer = String.create 2000 in
  let sock = Multicast.server mgroup mport in
  Multicast.send sock mgroup mport (Sys.argv.(1));
  while true do
    let len,_ = Multicast.recv sock buffer in
    lprintf "RECEIVED: %s" (String.escaped (String.sub buffer 0 len));
    lprint_newline ();
    sleep 1;
    Multicast.send sock mgroup mport (Sys.argv.(1));
  done
  
  