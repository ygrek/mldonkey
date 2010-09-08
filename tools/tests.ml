
open Printf

let pr fmt = ksprintf print_endline fmt

let test ?n ?s f =
  try
    f ()
  with
    exn -> 
      let msg = match s,n with
      | Some s, Some n -> sprintf " %d %S" n s
      | Some s, None -> sprintf " %S" s
      | None, Some n -> sprintf " %d" n
      | None, None -> ""
      in
      pr "Test%s failed: %s" msg (Printexc2.to_string exn)

let test1 ?n ?s f x = test ?n ?s (fun () -> f x)

let magnet s =
  let magnet = CommonTypes.parse_magnet_url s in
  pr "name: %S" magnet#name;
  begin match magnet#size with Some size -> pr "size: %Ld" size | None -> () end;
  pr "uids:";
  List.iter (fun x -> pr "  %s" (CommonTypes.string_of_uid x)) magnet#uids;
  pr ""

let test_magnet () =
  let t s = test1 ~s magnet s in
  t "magnet:?xt=urn:tree:tiger:UXNWMYERN37HJNXB7V6KDJKZXMFBIQAGMDMYDBY&dn=DCPlusPlus-0.4032.exe";
  t "magnet:?xt=urn:ed2k:354B15E68FB8F36D7CD88FF94116CDC1&xl=10826029&dn=mediawiki-1.15.1.tar.gz&xt=urn:tree:tiger:7N5OAMRNGMSSEUE3ORHOKWN4WWIQ5X4EBOOTLJY&xt=urn:btih:QHQXPYWMACKDWKP47RRVIV7VOURXFE5Q&tr=http%3A%2F%2Ftracker.example.org%2Fannounce.php%3Fuk%3D1111111111%26&as=http%3A%2F%2Fdownload.wikimedia.org%2Fmediawiki%2F1.15%2Fmediawiki-1.15.1.tar.gz&xs=http%3A%2F%2Fcache.example.org%2FXRX2PEFXOOEJFRVUCX6HMZMKS5TWG4K5&xs=dchub://example.org";
  t "magnet:?xt=urn:ed2k:31D6CFE0D16AE931B73C59D7E0C089C0&xl=0&dn=zero_len.fil&xt=urn:bitprint:3I42H3S6NNFQ2MSVX7XZKYAYSCX5QBYJ.LWPNACQDBZRYXW3VHJVCJ64QBZNGHOHHHZWCLNQ&xt=urn:md5:D41D8CD98F00B204E9800998ECF8427E";
  ()

let test_shorten () =
  let orig = "привет" in
  for i = 0 to 100 do
    test ~n:i ~s:"shorten" begin fun () ->
      let s = DcGlobals.shorten_string orig i in
      assert (s = String.sub orig 0 (min (String.length orig) (i*2)))
    end
  done;
  ()

let test_dc_parse () =
  let t x s =
    test ~s (fun () -> 
      match DcProtocol.dc_parse false s with 
      | DcProtocol.UnknownReq _ -> assert (not x)
      | _ -> assert x)
  in
  t true "$ADCGET list /shared1/ 0 -1";
  t true "$ADCGET file TTH/ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789012 1332982893 9194387";
  t false "$ADCGET tthl q 0 -1"

let () =
(*   let _ = Ip.addr_of_string "dchub://83.102.255.226" in *)
(*   let _ = Url.of_string "/submit?q=dcn+dchub://example.com+411" in *)
  test_magnet ();
  test_shorten ();
  test_dc_parse ();
  pr "Tests finished";
  ()
