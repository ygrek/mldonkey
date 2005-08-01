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

open Printf2
open Unix

let execvp_command cmd args handler = 
  let (in_read, output) = Unix.pipe() in
  let (input, out_write) = Unix.pipe() in
  match Unix.fork() with
    0 -> begin
        try
          match Unix.fork () with
            0 -> begin
                try
                  if input <> Unix.stdin then
                    begin Unix.dup2 input Unix.stdin; Unix.close input end;
                  if output <> Unix.stdout then
                    begin Unix.dup2 output Unix.stdout; Unix.close output end;
                  Unix.close in_read;
                  Unix.close out_write;
                  Unix.execvp cmd args;
                  exit 127
                with e -> 
                    Printf.eprintf "Exception %s in exec_command\n"
                      (Printexc2.to_string e) ; 
                    exit 1
              end
          | id -> 
              exit 2
        with _ -> 
            exit 3
      end
  | id -> 
      ignore (snd(Unix.waitpid [] id));
      Unix.close input;
      Unix.close output;
      let sock = handler in_read out_write in
      sock, id

let fork_and_exec cmd args = 
  match Unix.fork() with
    0 -> begin
        try
          match Unix.fork() with
            0 -> begin
                try
                  Unix.execv cmd args;
                  exit 0
                with e -> 
                    lprintf "Exception %s while starting file_completed_cmd\n" (Printexc2.to_string e); 
                    exit 127
              end
          | id -> exit 0
        with _ -> exit 0
      end
  | id -> ignore (snd(Unix.waitpid [] id))
      
let setuid = Unix.setuid
let set_close_on_exec = Unix.set_close_on_exec
let set_signal signal f = Sys.set_signal signal f
  
  
external getdtablesize : unit -> int = "ml_getdtablesize"
  
let max_all_sockets = getdtablesize ()
let max_sockets = max (max_all_sockets - 100) (max_all_sockets / 2)
let max_filedescs = (max_all_sockets - max_sockets) / 2

let chroot = Unix.chroot  


let write = Unix.write

let set_nonblock = Unix.set_nonblock

type statfs = {
  f_type : int64;   (* type of filesystem *)
  f_bsize : int64;  (* optimal transfer block size *)
  f_blocks : int64; (* total data blocks in file system *)
  f_bfree : int64;  (* free blocks in fs *)
  f_bavail : int64; (* free blocks avail to non-superuser *)
  f_files : int64;  (* total file nodes in file system *)
  f_ffree : int64;  (* free file nodes in fs *)
  f_fsid : unit;  (* See note in statfs(2) *)
  f_fnamelen : int64; (* maximum length of filenames *)
  f_basetype : string; (* type of filesystem - Solaris, (-1) on other systems, use f_type there *)
  f_frsize : int64;  (* Fundamental file system block size, (-1) if not provided by system *)
}

exception Not_supported
exception Error

let _ = Callback.register_exception "not supported" Not_supported
let _ = Callback.register_exception "error" Error

external statfs : string -> statfs = "statfs_statfs"
external fstatfs : Unix.file_descr -> statfs = "statfs_fstatfs"
external glibc_version : unit -> string = "glibc_version"

let glibc_version_num () =
  begin
    try
      let s = Printf.sprintf "%s" (glibc_version ()) in s
    with e -> ""
  end

let ( ** ) x y = Int64.mul x y
let ( ++ ) x y = Int64.add x y
let ( -- ) x y = Int64.sub x y
let ( // ) x y = Int64.div x y

let bsize dir =
  begin
    try
  let s = statfs dir in
    if s.f_frsize = Int64.zero || s.f_frsize = Int64.of_int (-1) then
      s.f_bsize
    else
      s.f_frsize
    with e -> Int64.of_int (-1)
  end

let blocks dir =
  begin
    try
  let s = statfs dir in
    s.f_blocks
    with e -> Int64.of_int (-1)
  end

let bfree dir =
  begin
    try
  let s = statfs dir in
    s.f_bfree
    with e -> Int64.of_int (-1)
  end

let bavail dir =
  begin
    try
  let s = statfs dir in
    s.f_bavail
    with e -> Int64.of_int (-1)
  end

let fnamelen dir =
  begin
    try
  let s = statfs dir in
    s.f_fnamelen
    with e -> Int64.of_int (-1)
  end

let disktotal dir =
(* total disk space in bytes *)
  begin
    try
  let s = statfs dir in
    ((bsize dir) ** s.f_blocks)
    with e -> Int64.of_int (-1)
  end

let diskfree dir =
(* free disk space in bytes *)
  begin
    try
  let s = statfs dir in
    ((bsize dir) ** s.f_bavail)
    with e -> Int64.of_int (-1)
  end

let diskused dir =
(* used disk space in bytes *)
  begin
    try
  let s = statfs dir in
    ((bsize dir) ** (s.f_blocks -- s.f_bavail))
    with e -> Int64.of_int (-1)
  end

let percentused dir =
(* percentage of used disk space *)
  if (diskfree dir) = Int64.of_int (-1) then
    (-1)
  else
    Int64.to_int (100L -- ((diskfree dir) ** 100L // (disktotal dir)))

let percentfree dir =
(* percentage of free disk space *)
  if (diskfree dir) = Int64.of_int (-1) then
    (-1)
  else
    Int64.to_int ((diskfree dir) ** 100L // (disktotal dir))

let filesystem dir =
  begin
    try
  let s = statfs dir in
    match s.f_type with
(* values copied from statfs(2) manpage *)
    | 0xadf5L -> "ADFS_SUPER_MAGIC"
    | 0xADFFL -> "AFFS_SUPER_MAGIC"
    | 0x42465331L -> "BEFS_SUPER_MAGIC"
    | 0x1BADFACEL -> "BFS_MAGIC"
    | 0xFF534D42L -> "CIFS_MAGIC_NUMBER"
    | 0x73757245L -> "CODA_SUPER_MAGIC"
    | 0x012FF7B7L -> "COH_SUPER_MAGIC"
    | 0x28cd3d45L -> "CRAMFS_MAGIC"
    | 0x1373L -> "DEVFS_SUPER_MAGIC"
    | 0x00414A53L -> "EFS_SUPER_MAGIC"
    | 0x137DL -> "EXT_SUPER_MAGIC"
    | 0xEF51L -> "ext2" (* EXT2_OLD_SUPER_MAGIC *)
    | 0xEF53L -> "ext2/3" (* EXT2/3_SUPER_MAGIC *)
    | 0x4244L -> "HFS_SUPER_MAGIC"
    | 0xF995E849L -> "HPFS_SUPER_MAGIC"
    | 0x958458f6L -> "HUGETLBFS_MAGIC"
    | 0x9660L -> "ISOFS_SUPER_MAGIC"
    | 0x4000L -> "ISOFS_SUPER_MAGIC_WIN" (* from coreutils-5.2.1, stat.c *)
    | 0x4004L -> "ISOFS_SUPER_MAGIC_R_WIN" (* from coreutils-5.2.1, stat.c *)
    | 0x72b6L -> "JFFS2_SUPER_MAGIC"
    | 0x3153464aL -> "JFS_SUPER_MAGIC"
    | 0x137FL -> "MINIX_SUPER_MAGIC"
    | 0x138FL -> "MINIX_SUPER_MAGIC2"
    | 0x2468L -> "MINIX2_SUPER_MAGIC"
    | 0x2478L -> "MINIX2_SUPER_MAGIC2"
    | 0x4d44L -> "msdos" (* MSDOS_SUPER_MAGIC *)
    | 0x4006L -> "fat" (* from coreutils-5.2.1, stat.c *)
    | 0x564cL -> "NCP_SUPER_MAGIC"
    | 0x6969L -> "NFS_SUPER_MAGIC"
    | 0x5346544eL -> "ntfs" (* NTFS_SB_MAGIC *)
    | 0x9fa1L -> "OPENPROM_SUPER_MAGIC"
    | 0x9fa0L -> "PROC_SUPER_MAGIC"
    | 0x002fL -> "QNX4_SUPER_MAGIC"
    | 0x52654973L -> "reiserfs" (* REISERFS_SUPER_MAGIC *)
    | 0x52345362L -> "reiser4"
    | 0x7275L -> "ROMFS_MAGIC"
    | 0x517BL -> "smb" (* SMB_SUPER_MAGIC *)
    | 0x012FF7B6L -> "SYSV2_SUPER_MAGIC"
    | 0x012FF7B5L -> "SYSV4_SUPER_MAGIC"
    | 0x01021994L -> "tmpfs" (* TMPFS_MAGIC *)
    | 0x15013346L -> "UDF_SUPER_MAGIC"
    | 0x00011954L -> "UFS_MAGIC"
    | 0x9fa2L -> "USBDEVICE_SUPER_MAGIC"
    | 0xa501FCF5L -> "VXFS_SUPER_MAGIC"
    | 0x012FF7B4L -> "XENIX_SUPER_MAGIC"
    | 0x58465342L -> "XFS_SUPER_MAGIC"
    | 0x012FD16DL -> "_XIAFS_SUPER_MAGIC"
    | 5L -> "iso9660" (* Cygwin *)
    | 6L -> "fat" (* Cygwin *)
    | 0x700FFL -> "ntfs" (* Cygwin *)
    | 0xC3L -> "ext2/3" (* Cygwin *)
    | _ -> if s.f_basetype <> "-1" then
	     s.f_basetype
	   else
	     Printf.sprintf "unknown (%Ld)" s.f_type
    with e -> "not supported"
  end
