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
    open Misc
    
    let hexa_digit x =
      if x >= 10 then Char.chr (Char.code 'A' + x - 10)
      else Char.chr (Char.code '0' + x)
        
    let to_string hash_length s =
      let p = String.create (hash_length * 2) in
      for i = 0 to hash_length - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        let i0 = (n/16) land 15 in
        let i1 = n land 15 in
        p.[2 * i] <- hexa_digit i0;
        p.[2 * i+1] <- hexa_digit i1;
      done;
      p
    
    let hexa_digit_case upper x =
      if x >= 10 then Char.chr (Char.code (
            if upper then 'A' else 'a')+ x - 10)
      else Char.chr (Char.code '0' + x)

    let to_string_case upper hash_length s =
      let p = String.create (hash_length * 2) in
      for i = 0 to hash_length - 1 do
        let c = s.[i] in
        let n = int_of_char c in
        let i0 = (n/16) land 15 in
        let i1 = n land 15 in
        p.[2 * i] <- hexa_digit_case upper i0;
        p.[2 * i+1] <- hexa_digit_case upper i1;
      done;
      p
    
    let digit_hexa c =
      let i = int_of_char c in
      if i >= i_a && i <= i_f then i - i_a + 10 else
      if i >= i_A && i <= i_F then i - i_A + 10 else
      if i >= i_0 && i <= i_9 then i - i_0 else
        failwith "Bad hexa char"
    
    let of_string hash_length s =
      assert (String.length s = hash_length*2);
      let p = String.create hash_length in
      for i = 0 to hash_length - 1 do
        let c0 = s.[2*i] in
        let c1 = s.[2*i+1] in
        p.[i] <- char_of_int ((16 * digit_hexa c0) + digit_hexa c1);
      done;
      p
    
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
      let s = String.make hash_length '\000' in
      for i = 0 to len - 1 do
        let pos = i * 5 in
        let byte = pos / 8 in
        let bit = pos mod 8 in
        let c = int5_of_char r.[i] in
        if bit < 3 then 
          let x = c lsl (3-bit) in
          s.[byte] <- char_of_int (int_of_char s.[byte] lor x);
        else
        let x = (c lsr (bit - 3)) land 0xff in
        s.[byte] <- char_of_int (int_of_char s.[byte] lor x);
        if byte+1 < hash_length then
          let y = (c lsl (11 - bit)) land 0xff in
          s.[byte+1] <- char_of_int (int_of_char s.[byte+1] lor y);
      done;
      s    
    
    let to_string hash_length s =
      assert (String.length s = hash_length);
      let len = (hash_length * 8 + 4)/5 in
      let r = String.create len in
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
      r

    let char_of_int5 upper n =
      char_of_int (if n < 26 then (if upper then 65 else 97)+n else
          50+(n-26))
    
    let to_string_case upper hash_length s =
      assert (String.length s = hash_length);
      let len = (hash_length * 8 + 4)/5 in
      let r = String.create len in
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
      r
      
  end
  
module type Digest = sig
    type t
    
    val null : t
    val one : t
    val two : t
    val to_string : t -> string
    val to_string_case : bool -> t -> string
    val of_string : string -> t
    
    val string : string -> t
    val file : string -> t
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
      
      val unsafe_string : string -> string -> int -> unit
      val unsafe_file : string -> string -> unit
      val digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit 
    
      module Base : Base
    end) = struct
    open M
    
    type t = string
      
    let length = hash_length
    
    let null = String.make hash_length '\000'
    let one = String.make hash_length '\001'
    let two =  String.make hash_length '\002'
    
    let string s =
      let len = String.length s in
      let digest = String.create hash_length in
      unsafe_string digest s len;
      digest
    
    external xor_c : t -> t -> t -> unit = "md4_xor" "noalloc"
    
    let xor m1 m2 =
      let m3 = String.create hash_length in
      xor_c m1 m2 m3;
      m3
    
    let file s =
      let digest = String.create hash_length in
      unsafe_file digest s;
      digest
    
    let digest_subfile fd pos len =
      let digest = String.create hash_length in
      let (fd, pos) = Unix32.fd_of_chunk fd pos len in
      digest_subfile digest fd pos len;
      digest
    
    let create () =  String.create hash_length
    
    let direct_to_string s = s
    let direct_of_string s = s
    
    let random () =
      let s = create () in
      for i = 0 to hash_length - 1 do
        s.[i] <- char_of_int (Random.int 256)
      done;
      s
    
    let of_string = Base.of_string hash_length
    let to_string = Base.to_string hash_length
    let to_string_case upper s = Base.to_string_case upper hash_length s
      
    open Options
    
    let value_to_hash v = of_string (value_to_string v)
    
    let hash_to_value v = string_to_value (to_string v)
    
    let option =
      define_option_class hash_name value_to_hash hash_to_value
    
    
    let up s = int_of_char s.[0]
    let up2 s = (int_of_char s.[0])*256+(int_of_char s.[1])
    let up3 s = (int_of_char s.[0])*65536+(int_of_char s.[1])*256+(int_of_char s.[2])
  
    let enabled = true
  end
  
module Md4 = Make(struct
      let hash_length = 16
      let hash_name = "Md4"        
      
      external unsafe_string : string -> string -> int -> unit = "md4_unsafe_string"
      external unsafe_file : string -> string -> unit = "md4_unsafe_file"
      external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
        "md4_unsafe64_fd"
  
      module Base = Base16
    end)
  
module Md5 = Make(struct
      let hash_length = 16
      let hash_name = "Md5"        
      
      external unsafe_string : string -> string -> int -> unit = "md5_unsafe_string"
      external unsafe_file : string -> string -> unit = "md5_unsafe_file"
      external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
        "md5_unsafe64_fd"
    
      module Base = Base16
    end)
  
module PreSha1 = Make(struct
      let hash_length = 20
      let hash_name = "Sha1"        
      
      external unsafe_string : string -> string -> int -> unit = "sha1_unsafe_string"
      external unsafe_file : string -> string -> unit = "sha1_unsafe_file"
      external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
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
  
(* NOT YET IMPLEMENTED *)
module PreTiger = Make(struct
      let hash_length = 24
      let hash_name = "Tiger"        
      
      external unsafe_string : string -> string -> int -> unit = "tiger_unsafe_string"
        
      let unsafe_file digest filename = 
        Printf2.lprintf "Tiger.unsafe_file not implemented\n";
        exit 2
        
(*
      external unsafe_file : string -> string -> unit = "sha1_unsafe_file"
*)
      external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
        "tiger_unsafe64_fd"
    
      module Base = Base32
        
    end)

module Tiger = struct
    include PreTiger
    open PreTiger
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
            lprintf "Unable to compute correct Tiger trees.\n";
            lprintf "Send a bug report with your configuration\n";
            lprintf "and how you obtained this executable.\n";
            lprintf "Running with Tiger tree corruption detection disabled.\n";
            lprintf "(used only if you run the Gnutella plugin)\n";
            false
  end
  
(* Use urn:tree:tiger: also ... *)
        
  
module Md5Ext = Make(struct
      let hash_length = 20
      let hash_name = "Md5Ext"        

      let unsafe_string _ _ _ = 
        failwith "Md5Ext.unsafe_string not implemented"
      let unsafe_file _ _ = 
        failwith "Md5Ext.unsafe_file not implemented"
      let digest_subfile _ _ _ _ = 
        failwith "Md5Ext.digest_subfile not implemented"
    
      module Base = Base16
    end)
