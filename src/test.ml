
open CommonTypes

let pr fmt = Printf.ksprintf print_endline fmt

let magnet s =
  let magnet = parse_magnet_url s in
  pr "name: %S" magnet#name;
  begin match magnet#size with Some size -> pr "size: %Ld" size | None -> () end;
  pr "uids:";
  List.iter (fun x -> pr "  %s" (string_of_uid x)) magnet#uids;
  pr ""

let () =
  magnet "magnet:?xt=urn:tree:tiger:UXNWMYERN37HJNXB7V6KDJKZXMFBIQAGMDMYDBY&dn=DCPlusPlus-0.4032.exe";
  magnet "magnet:?xt=urn:ed2k:354B15E68FB8F36D7CD88FF94116CDC1&xl=10826029&dn=mediawiki-1.15.1.tar.gz&xt=urn:tree:tiger:7N5OAMRNGMSSEUE3ORHOKWN4WWIQ5X4EBOOTLJY&xt=urn:btih:QHQXPYWMACKDWKP47RRVIV7VOURXFE5Q&tr=http%3A%2F%2Ftracker.example.org%2Fannounce.php%3Fuk%3D1111111111%26&as=http%3A%2F%2Fdownload.wikimedia.org%2Fmediawiki%2F1.15%2Fmediawiki-1.15.1.tar.gz&xs=http%3A%2F%2Fcache.example.org%2FXRX2PEFXOOEJFRVUCX6HMZMKS5TWG4K5&xs=dchub://example.org";
  magnet "magnet:?xt=urn:ed2k:31D6CFE0D16AE931B73C59D7E0C089C0&xl=0&dn=zero_len.fil&xt=urn:bitprint:3I42H3S6NNFQ2MSVX7XZKYAYSCX5QBYJ.LWPNACQDBZRYXW3VHJVCJ64QBZNGHOHHHZWCLNQ&xt=urn:md5:D41D8CD98F00B204E9800998ECF8427E";
  ()
