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

module Test  = struct
  let s1 = "" 
  let s2 = "\000" 
  let s3 = String.make 1024 'A' 
  let s4 = String.make 1025 'A' 
end


let i_a = int_of_char 'a'  
let i_A = int_of_char 'A'  
let i_f = int_of_char 'f'  
let i_F = int_of_char 'F'  
let i_0 = int_of_char '0'
let i_9 = int_of_char '9'

module type Base = sig 
    val to_string : int -> string -> string
    val to_string_case : bool -> int -> string -> string
    val of_string : int -> string -> string
  end
  
module Base16 = struct 
    
    let hexa_digit x =
      if x >= 10 then Char.chr (Char.code 'A' + x - 10)
      else Char.chr (Char.code '0' + x)
        
    let to_string hash_length s =
      let p = Bytes.create (hash_length * 2) in
      for i = 0 to hash_length - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        let i0 = (n/16) land 15 in
        let i1 = n land 15 in
        p.[2 * i] <- hexa_digit i0;
        p.[2 * i+1] <- hexa_digit i1;
      done;
      Bytes.unsafe_to_string p
    
    let hexa_digit_case upper x =
      if x >= 10 then Char.chr (Char.code (
            if upper then 'A' else 'a')+ x - 10)
      else Char.chr (Char.code '0' + x)

    let to_string_case upper hash_length s =
      let p = Bytes.create (hash_length * 2) in
      for i = 0 to hash_length - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        let i0 = (n/16) land 15 in
        let i1 = n land 15 in
        p.[2 * i] <- hexa_digit_case upper i0;
        p.[2 * i+1] <- hexa_digit_case upper i1;
      done;
      Bytes.unsafe_to_string p
    
    let digit_hexa c =
      let i = int_of_char c in
      if i >= i_a && i <= i_f then i - i_a + 10 else
      if i >= i_A && i <= i_F then i - i_A + 10 else
      if i >= i_0 && i <= i_9 then i - i_0 else
        failwith "Bad hexa char"
    
    let of_string hash_length s =
      assert (String.length s = hash_length*2);
      let p = Bytes.create hash_length in
      for i = 0 to hash_length - 1 do
        let c0 = s.[2*i] in
        let c1 = s.[2*i+1] in
        p.[i] <- char_of_int ((16 * digit_hexa c0) + digit_hexa c1);
      done;
      Bytes.unsafe_to_string p
    
  end

module Base32 = struct

    let char_of_int5 n =
      char_of_int (if n < 26 then 65+n else
          50+(n-26))

    let int5_of_char n =
      match n with
        'A' .. 'Z' -> int_of_char n - 65
      | 'a' .. 'z' -> int_of_char n - 97
      | _ -> (int_of_char n+26)-50

    let of_string hash_length r =
      let len = String.length r in
      assert (len =  (hash_length * 8 + 4)/5);
      let s = Bytes.make hash_length '\000' in
      for i = 0 to len - 1 do
        let pos = i * 5 in
        let byte = pos / 8 in
        let bit = pos mod 8 in
        let c = int5_of_char r.[i] in
        if bit < 3 then
          let x = c lsl (3-bit) in
          s.[byte] <- char_of_int (int_of_char (Bytes.get s byte) lor x);
        else
        let x = (c lsr (bit - 3)) land 0xff in
        s.[byte] <- char_of_int (int_of_char (Bytes.get s byte) lor x);
        if byte+1 < hash_length then
          let y = (c lsl (11 - bit)) land 0xff in
          s.[byte+1] <- char_of_int (int_of_char (Bytes.get s (byte+1)) lor y);
      done;
      Bytes.unsafe_to_string s

    let to_string hash_length s =
      assert (String.length s = hash_length);
      let len = (hash_length * 8 + 4)/5 in
      let r = Bytes.create len in
      for i = 0 to len - 1 do
        let pos = i * 5 in
        let byte = pos / 8 in
        let bit = pos mod 8 in
        if bit < 3 then
          let x = int_of_char s.[byte] in
          let c = (x lsr (3 - bit)) land 0x1f in
          r.[i] <- char_of_int5 c
        else
        let x = int_of_char s.[byte] in
        let y = if byte + 1 = hash_length then 0 else 
            int_of_char s.[byte+1] in
        let x = (x lsl 8) + y in
        let c = (x lsr (11 - bit)) land 0x1f in
        r.[i] <- char_of_int5 c
      done;
      Bytes.unsafe_to_string r

    let char_of_int5 upper n =
      char_of_int (if n < 26 then (if upper then 65 else 97)+n else
          50+(n-26))

    let to_string_case upper hash_length s =
      assert (String.length s = hash_length);
      let len = (hash_length * 8 + 4)/5 in
      let r = Bytes.create len in
      for i = 0 to len - 1 do
        let pos = i * 5 in
        let byte = pos / 8 in
        let bit = pos mod 8 in
        if bit < 3 then
          let x = int_of_char s.[byte] in
          let c = (x lsr (3 - bit)) land 0x1f in
          r.[i] <- char_of_int5 upper c
        else
        let x = int_of_char s.[byte] in
        let y = if byte + 1 = hash_length then 0 else 
            int_of_char s.[byte+1] in
        let x = (x lsl 8) + y in
        let c = (x lsr (11 - bit)) land 0x1f in
        r.[i] <- char_of_int5 upper c
      done;
      Bytes.unsafe_to_string r
      
  end

module Base6427 = struct  
    let base64tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    
    let _ = assert (String.length base64tbl = 64)
    
    let to_string _ hashbin =
      let hash64 = Bytes.create 30 in
      let hashbin n = int_of_char hashbin.[n] in
      hash64.[0] <- '=';
      let j = ref 1 in
      for i = 0 to 6 do
        let tmp = if i < 6 then
            ((hashbin (3*i)) lsl 16) lor ((hashbin(3*i+1)) lsl 8) 
            lor (hashbin (3*i+2))
          else
            ((hashbin(3*i)) lsl 16) lor ((hashbin(3*i+1)) lsl 8)
        in
        for k = 0 to 3 do
          hash64.[!j] <- base64tbl.[(tmp lsr ((3- k)*6)) land 0x3f];
          incr j
        done
      done;
      hash64.[!j-1] <- '=';
      Bytes.sub_string hash64 0 !j
    
    let base64tbl_inv =
      let table = Bytes.create 126 in
      for i = 0 to 63 do
        table.[int_of_char base64tbl.[i]] <- char_of_int i
      done;
      Bytes.unsafe_to_string table
    
    let of_string _ hash64 =
      let hashbin = Bytes.make 20 '\000' in
      let hash64 n = 
        let c = hash64.[n] in
        int_of_char base64tbl_inv.[int_of_char c]
      in
      let j = ref 0 in
      for i = 0 to 6 do
        if i < 6 then
          let tmp = ref 0 in
          for k = 0 to 3 do
            tmp := (!tmp lsl 6) lor (hash64 (i*4+k+1))
          done;
          hashbin.[!j] <- char_of_int ((!tmp lsr 16) land 0xff);
          hashbin.[!j+1] <- char_of_int ((!tmp lsr  8) land 0xff);
          hashbin.[!j+2] <- char_of_int ((!tmp lsr  0) land 0xff);
          j := !j + 3;
        else
        let tmp = ref 0 in
        for k = 0 to 2 do
          tmp := (!tmp lsl 6) lor (hash64 (i*4+k+1))
        done;
        tmp := (!tmp lsl 6);
        hashbin.[!j] <- char_of_int ((!tmp lsr 16) land 0xff);
        hashbin.[!j+1] <- char_of_int ((!tmp lsr  8) land 0xff);
        j := !j + 2;
      done;
      Bytes.unsafe_to_string hashbin
      
    let to_string_case _ = to_string
  end
  
  
module type Digest = sig
    type t

    val null : t
    val one : t
    val two : t
      
    val equal : t -> t -> bool
      
    val to_string : t -> string
    val to_string_case : bool -> t -> string
    val of_string : string -> t

    val to_bits : t -> string
      
    val to_hexa : t -> string
    val to_hexa_case : bool -> t -> string
    val of_hexa : string -> t
      
    val to_base32 : t -> string
    val to_base32_case : bool -> t -> string
    val of_base32 : string -> t
    
    val string : string -> t
(*    val file : string -> t *)
    val create : unit -> t
    val direct_of_string : string -> t
    val direct_to_string : t -> string
    val random : unit -> t
    
    val digest_subfile : Unix32.t -> int64 -> int64 -> t
    
    val option : t Options.option_class
    
    val xor : t -> t -> t
    val value_to_hash : Options.option_value -> t
    val hash_to_value : t -> Options.option_value
    
    val up : t -> int
    val up2 : t -> int
    val up3 : t -> int
    
    val length : int
    val enabled : bool
  end
  
  
module Make(M: sig
      val hash_length : int
      val hash_name : string
      
(* [unsafe_string digest string string_len] *)
      val unsafe_string : bytes -> string -> int -> unit
          
(* [unsafe_file digest filename filesize] *)
      val unsafe_file : bytes -> string -> int64 -> unit
(* [unsafe_string digest file_fd offset len] *)
      val digest_subfile : bytes -> Unix.file_descr -> int64 -> int64 -> unit 
    
      module Base : Base
    end) = struct
    open M
    
    type t = string
      
    let length = hash_length
    
    let null = String.make hash_length '\000'
    let one = String.make hash_length '\001'
    let two =  String.make hash_length '\002'

    let equal h1 h2 = (String.compare h1 h2) = 0

    let string s =
      let len = String.length s in
      let digest = Bytes.create hash_length in
      unsafe_string digest s len;
      Bytes.unsafe_to_string digest

    let to_bits s =
      let len = String.length s in
      let digest = Bytes.create (8*len) in
      for i = 0 to len-1 do
        let c = int_of_char s.[i] in
        for j = 7 downto 0 do
          digest.[i*8 + (7-j)] <- 
            (if c land (1 lsl j) <> 0 then '1' else '0')
            
        done
      done;
      Bytes.unsafe_to_string digest
      
    external xor_c : t -> t -> bytes -> unit = "md4_xor" "noalloc"
    
    let xor m1 m2 =
      let m3 = Bytes.create hash_length in
      xor_c m1 m2 m3;
      Bytes.unsafe_to_string m3
    
    let file s =
      let digest = Bytes.create hash_length in
      let file_size = Unix32.getsize s in
      unsafe_file digest s file_size;
      Bytes.unsafe_to_string digest
    
    let digest_subfile fd pos len =
      let digest = Bytes.create hash_length in
      Unix32.apply_on_chunk fd pos len 
        (fun fd pos ->
          digest_subfile digest fd pos len);
      Bytes.unsafe_to_string digest
    
    let create () = String.make hash_length '\x00'
    
    let direct_to_string s = s
    let direct_of_string s = s
    
    let random () =
      let s = Bytes.create hash_length in
      for i = 0 to hash_length - 1 do
        s.[i] <- char_of_int (Random.int 256)
      done;
      Bytes.unsafe_to_string s
    
    let of_string = Base.of_string hash_length
    let to_string = Base.to_string hash_length
    let to_string_case upper s = Base.to_string_case upper hash_length s
    
    let of_hexa = Base16.of_string hash_length
    let to_hexa = Base16.to_string hash_length
    let to_hexa_case upper s = Base16.to_string_case upper hash_length s
    
    let of_base32 = Base32.of_string hash_length
    let to_base32 = Base32.to_string hash_length
    let to_base32_case upper s = Base32.to_string_case upper hash_length s
      
    open Options
    
    let value_to_hash v = of_string (value_to_string v)
    
    let hash_to_value v = string_to_value (to_string v)
    
    let option =
      define_option_class hash_name value_to_hash hash_to_value
    
    
    let up s = int_of_char s.[0]
    let up2 s = ((int_of_char s.[0]) lsl 8) lor (int_of_char s.[1])
    let up3 s = ((int_of_char s.[0]) lsl 16) lor 
                ((int_of_char s.[1]) lsl 8) lor (int_of_char s.[2])
  
    let enabled = true
  end
  
module Md4 = Make(struct
      let hash_length = 16
      let hash_name = "Md4"        
      
      external unsafe_string : bytes -> string -> int -> unit = "md4_unsafe_string"
      external unsafe_file : bytes -> string -> int64 -> unit = "md4_unsafe_file"
      external digest_subfile : bytes -> Unix.file_descr -> int64 -> int64 -> unit =
        "md4_unsafe64_fd"
  
      module Base = Base16
    end)
  
module Md5 = Make(struct
      let hash_length = 16
      let hash_name = "Md5"        
      
      external unsafe_string : bytes -> string -> int -> unit = "md5_unsafe_string"
      external unsafe_file : bytes -> string -> int64 -> unit = "md5_unsafe_file"
      external digest_subfile : bytes -> Unix.file_descr -> int64 -> int64 -> unit =
        "md5_unsafe64_fd"
    
      module Base = Base16
    end)
  
module PreSha1 = Make(struct
      let hash_length = 20
      let hash_name = "Sha1"        
      
      external unsafe_string : bytes -> string -> int -> unit = "sha1_unsafe_string"
      external unsafe_file : bytes -> string -> int64 -> unit = "sha1_unsafe_file"
      external digest_subfile : bytes -> Unix.file_descr -> int64 -> int64 -> unit =
        "sha1_unsafe64_fd"
      
      module Base = Base32
                  
    end)

module Sha1 = struct
    include PreSha1
    open PreSha1
    open Test
    open Printf2
      
    let enabled =
      try
        let sha1 = "ABCDEFGHGHIJKLMNOPQRSTUVWXYZ2ABC" in
        assert (to_string (of_string sha1) = sha1);
        
        assert (to_string (string s1) =
          "3I42H3S6NNFQ2MSVX7XZKYAYSCX5QBYJ");
        assert (to_string (string s2) =
          "LOUTZHNQZ74T6UVVEHLUEDSD63W2E6CP");
        assert (to_string (string s3) = 
          "ORWD6TJINRJR4BS6RL3W4CWAQ2EDDRVU");
        assert (to_string (string s4) = 
          "UUHHSQPHQXN5X6EMYK6CD7IJ7BHZTE77");
        
        true
      with e ->
          lprintf "Unable to compute correct Sha1 hashes.\n";
          lprintf "Send a bug report with your configuration\n";
          lprintf "and how you obtained this executable.\n";
          lprintf "Running with Sha1 tree corruption detection disabled.\n";
          lprintf "(used only if you run the BitTorrent plugin)\n";
          false
  end
  
module Tiger = Make(struct
      let hash_length = 24
      let hash_name = "Tiger"        
      
      external unsafe_string : bytes -> string -> int -> unit = 
        "tiger_unsafe_string"
        
      let unsafe_file digest filename = 
        failwith "Tiger.unsafe_file not implemented"
        
      let digest_subfile _ _ _ _ = 
        failwith "Tiger.digest_subfile not implemented"
    
      module Base = Base32
        
    end)
  
module PreTigerTree = Make(struct
      let hash_length = 24
      let hash_name = "TigerTree"        
      
      external unsafe_string : bytes -> string -> int -> unit = "tigertree_unsafe_string"
      external digest_subfile : bytes -> Unix.file_descr -> int64 -> int64 -> unit =
        "tigertree_unsafe64_fd"
      
      let unsafe_file digest filename file_size = 
        let fd = Unix32.create_diskfile filename false in
        Unix32.apply_on_chunk fd Int64.zero file_size 
          (fun fd pos ->
            digest_subfile digest fd pos file_size)
    
      module Base = Base32
        
    end)

module TigerTree = struct
    include PreTigerTree
    open PreTigerTree
    open Printf2
    open Test
            
      let enabled = 
        try
          assert (to_string (string s1) =
            "LWPNACQDBZRYXW3VHJVCJ64QBZNGHOHHHZWCLNQ");  
          assert (to_string (string s2) =
            "VK54ZIEEVTWNAUI5D5RDFIL37LX2IQNSTAXFKSA");
          assert (to_string (string s3) =
            "L66Q4YVNAFWVS23X2HJIRA5ZJ7WXR3F26RSASFA");
          assert (to_string (string s4) =
            "PZMRYHGY6LTBEH63ZWAHDORHSYTLO4LEFUIKHWY");
          true
      with e ->
          lprintf "TigerTree: Exception %s\n" 
            (Printexc2.to_string e);
          lprintf "Unable to compute correct Tiger trees.\n";
          lprintf "Send a bug report with your configuration\n";
          lprintf "and how you obtained this executable.\n";
          lprintf "Running with Tiger tree corruption detection disabled.\n";
          lprintf "(used only if you run the Gnutella plugin)\n";
          false
  end

(* Use urn:tree:tiger: also ... *)
        
  
module PreMd5Ext = Make(struct
      let hash_length = 20
      let hash_name = "Md5Ext"        

      external unsafe_string : bytes -> string -> int -> unit =
        "fst_hash_string_ml"
        
      external unsafe_file : bytes -> string -> int64 -> unit = "fst_hash_file_ml"
      let digest_subfile _ _ _ _ = 
        failwith "Md5Ext.digest_subfile not implemented"
    
      module Base = Base6427
        
    end)
  
module Md5Ext = struct
    include PreMd5Ext
    
    open Printf2
    
    let enabled =
      try
        let s1 = "abcedefghijklmneo" in
        assert (to_string (string s1) = "=DLr2bO9taE9mZwmabUd/9e7///8=");
        let s2 = String.make 1000 'A' in
        assert (to_string (string s2) = "=dkRnLQSSkPA5DZyZPH00PRf8//8=");
        true
      
      with e ->
          lprintf "Unable to correct correct Fasttrack hash.\n";
          lprintf "You will not be able to share your files on the\n";
          lprintf "Fasttrack network.\n";
          false
  
  end
