    type t = {
    mutable name : string;
    mutable ip : Unix.inet_addr array;
  }

let local_ip = Unix.inet_addr_of_string "127.0.0.1"

let local_host = {
    name = "localhost";
    ip = [| local_ip |]      
  }

let from_name name = {
    name =  name;
    ip = [||];
  }

let from_ints a1 a2 a3 a4 =
  let name = Printf.sprintf "%d.%d.%d.%d" a1 a2 a3 a4 in
  let ip = Unix.inet_addr_of_string name in
  { name = name; ip = [| ip |] }

let from_ip ip =
  { name = Unix.string_of_inet_addr ip; 
    ip = [|ip|];
  }

let name t = t.name
let ip t = 
  if Array.length t.ip = 0 then
    let h = Unix.gethostbyname t.name in
    t.ip <- h.Unix.h_addr_list;
    t.ip.(0)
  else
    t.ip.(0)
