(* Compute the key from $Lock xxxxx yyyyy| using 'gen_key xxxxx' *)


(*************************************************************************)
(* the key is quite easily :) computed from the lock                     *)
(* key[x]= ns(lock[x]^lock[x-1])                                      *)
(*   ns is a nibble swap (switch the upper 4 bits with the lower 4 bits) *)
(* exception:                                                            *)
(* key[0] is a bit different                                             *)
(* let's name A and B the 2 last bytes of the lock                       *)
(* key[0]= ns(lock[0]^A^B^0x05)         ; 0x05 is a kind of magic nibble *)
(*************************************************************************)

(*

$Lock 17lM=.U=*@Q&HvoG2=HJcLH:-,Q=5R=xMvRo-ET4;nMZYxnP,P_&oHFmc%B Pk=.l>r?nZz-VKmlf&z|

$Key 5/%DCN096%/(181)(18)(7)1(183)(134)q(166)(17)w(230)(227)(145)(130)W(240)W (146)(242)@'q(16)(215)(198)(128)v(246)TS(179)B(211)/%DCN036%/(134)(17)(6)(240)U2q0(18)a(227)(199)(199)(240)(151)(148)r(224)(178)(224)dv|

*)

let get s pos = int_of_char s.[pos]

let gen s =
  let buf = Buffer.create 100 in
  let len = String.length s in

(* first byte *)
  
  let u = (get s 0) land 255 in        (*  u=(((unsigned int)(lck->str[0]))&255); *)
  let l = (get s (len-1)) land 255 in  (* l=(((unsigned int)(lck->str[lck->len-1]))&255); *)
  let o = (get s (len-2)) land 255 in  (*  o=(((unsigned int)(lck->str[lck->len-2]))&255); *)
  
  let u = u lxor l lxor o lxor 0x05 in (* u=u^l^o^0x05; *)
  
  let v = (((u lsl 8) lor u) lsr 4) land 255 in (*  v=(((u<<8)|u)>>4)&255; *)
  
  begin
    match v with
      0 | 5 -> Printf.bprintf buf "/%%DCN%03d%%/" v
    | 36 (* '$' *) -> Printf.bprintf buf "/%%DCN036%%/"
    | 96 -> Printf.bprintf buf "/%%DCN096%%/"
    | _ -> Buffer.add_char buf (char_of_int v)
  end;
  
  for i = 1 to len - 1 do
    let u = (get s i) land 255 in
    let l = (get s (i-1)) land 255 in
    let u = u lxor l in
    let v = (((u lsl 8) lor u) lsr 4) land 255 in (*  v=(((u<<8)|u)>>4)&255; *)
    
    begin
      match v with
        0 | 5 -> Printf.bprintf buf "/%%DCN%03d%%/" v
      | 36 (* '$' *) -> Printf.bprintf buf "/%%DCN036%%/"
      | 96 -> Printf.bprintf buf "/%%DCN096%%/"
      | _ -> Buffer.add_char buf (char_of_int v)
    end;
  done;
  Buffer.contents buf
  