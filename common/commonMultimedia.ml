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

open CommonTypes

let input_int8 ic =
  int_of_char (input_char ic)
  
let input_int16 ic =
  let i0 = input_int8  ic in
  let i1 = input_int8 ic in
  i0 + 256 * i1

let int32_65536 = Int32.of_int 65536
  
let input_int32 ic =
  let i0 = input_int16 ic in
  let i1 = input_int16 ic in
  let i0 = Int32.of_int i0 in
  let i1 = Int32.of_int i1 in
  Int32.add i0 (Int32.mul i1 int32_65536)
  
let input_int ic =
  let i0 = input_int16 ic in
  let i1 = input_int16 ic in
  i0 + i1 * 65536

let input_string4 ic =
  let s = String.create 4 in
  really_input ic s 0 4;
  s

let print_string4 v s =
  Printf.printf "%s :" v;
  for i = 0 to 3 do
    let c = s.[i] in
    let int = int_of_char c in
    if int > 31 && int <127 then
      print_char c
    else Printf.printf "[%d]" int
  done;
  print_newline ()

let print_int32 s i=
  Printf.printf "%s: %ld" s i;
  print_newline ()

let print_int16 s i=
  Printf.printf "%s: %d" s i;
  print_newline ()
  

let search_info_avi ic =
  try
(* pos: 0 *)
    let s = input_string4 ic in
    if s <> "RIFF" then failwith "Not an AVI file (RIFF absent)";

(* pos: 4 *)
    let size = input_int32 ic in
(*
  Printf.printf "SIZE %s" (Int32.to_string size);
  print_newline ();
*)

(* pos: 8 *)
    let s = input_string4 ic in
    if s <> "AVI " then failwith  "Not an AVI file (AVI absent)";

(* pos: 12 *)
    let s = input_string4 ic in
    if s <> "LIST" then failwith  "Not an AVI file (LIST absent)";

(* position 16 *)
    let rec iter_list pos end_pos =
(*
  Printf.printf "POS %s/%s" (Int32.to_string pos) (Int32.to_string end_pos);
print_newline ();    
  *)
      if pos < end_pos then begin
(* on peut s'arreter quand size = 0 *)
          seek_in ic (Int32.to_int pos);
          let size2 = input_int32 ic in
(*
        Printf.printf "SIZE2 %s" (Int32.to_string size2);
        print_newline ();
*)
          
          if size2 = Int32.zero then raise Not_found;
          let header_name = input_string4 ic in
(*        print_string4 "header" header_name; print_newline (); *)
(* pos: pos + 8 *)       
          begin
            match header_name with
              "hdrl" ->
(*              Printf.printf "HEADER"; print_newline (); *)
                
                let s = input_string4 ic in
                if s <> "avih" then failwith "Bad AVI file (avih absent)";

(* pos: pos + 12 *)
                let main_header_len = 52 in             
                
                ignore (input_string4 ic);
                
                
                let dwMicroSecPerFrame = input_int32 ic in
                let dwMaxBytesPerSec = input_int32 ic in
                let dwPaddingGranularity = input_int32 ic in
                let dwFlags = input_int32 ic in
                let dwTotalFrames = input_int32 ic in
                let dwInitialFrames = input_int32 ic in
                let dwStreams = input_int32 ic in
                let dwSuggestedBufferSize = input_int32 ic in              
                let dwWidth = input_int32 ic in
                let dwHeight = input_int32 ic in

(*
              print_int32 "dwMicroSecPerFrame" dwMicroSecPerFrame ;
              print_int32 "dwMaxBytesPerSec" dwMaxBytesPerSec ;
              print_int32 "dwPaddingGranularity" dwPaddingGranularity ;
              print_int32 "dwFlags" dwFlags ;
              print_int32 "dwTotalFrames" dwTotalFrames ;
              print_int32 "dwInitialFrames" dwInitialFrames ;
              print_int32 "dwStreams" dwStreams ;
              print_int32 "dwSuggestedBufferSize" dwSuggestedBufferSize ;
              print_int32 "dwWidth" dwWidth;
              print_int32 "dwHeight" dwHeight;
              *)
                
                seek_in ic ((Int32.to_int pos) + main_header_len +20);
                let s = input_string4 ic in
(*              print_string4 "LIST:" s; *)
                let pos_in = Int32.add pos (Int32.of_int (
                      main_header_len +24)) in
                let last_pos = Int32.add pos_in size2 in
                iter_list pos_in last_pos
            
            
            | "movi" ->
(*              Printf.printf "CHUNKS"; print_newline (); *)
                ()
            
            | "strl" ->
(*              Printf.printf "STREAM DESCRIPTION"; print_newline (); *)
                
                let offset = Int32.of_int 4  in
                let pos0 = Int32.add pos offset in
                let end_pos0 = Int32.add pos size2 in
                iter_list pos0 end_pos0
            
            | "strh" ->
(*              Printf.printf "STREAM HEADER"; print_newline (); *)
                
                ignore (input_string4 ic);
                
                let fccType = input_string4 ic in
                let fccHandler = input_string4 ic in
                let dwFlags = input_int32 ic in (* Contains AVITF_* flags *)
                let wPriority = input_int16 ic in
                let wLanguage = input_int16 ic in
                let dwInitialFrames = input_int32 ic in
                let dwScale = input_int32 ic in
                let dwRate = input_int32 ic in (* dwRate / dwScale == samples/second *)
                let dwStart = input_int32 ic in
                let dwLength = input_int32 ic in
                let dwSuggestedBufferSize = input_int32 ic in
                let dwQuality = input_int32 ic in
                let dwSampleSize = input_int32 ic in
                let rcFrame_x = input_int16 ic in
                let rcFrame_y = input_int16 ic in
                let rcFrame_dx = input_int16 ic in
                let rcFrame_dy = input_int16 ic in
                
                if fccType = "vids" then                
                  raise (FormatFound (AVI {
                        avi_codec = fccHandler;
                        avi_width = rcFrame_dx;
                        avi_height = rcFrame_dy;
                        avi_fps = int_of_float(1000.0 *. Int32.to_float(dwRate) /. Int32.to_float(dwScale));
                        avi_rate = Int32.to_int dwLength;
                      }));
                
                
                
                print_string4 "fccType" fccType;
                print_string4 "fccHandler" fccHandler;
                print_int32 "dwFlags " dwFlags; (* Contains AVITF_* flags *)
                print_int16 "wPriority" wPriority;
                print_int16 "wLanguage" wLanguage;
                print_int32 "dwInitialFrames " dwInitialFrames;
                print_int32 "dwScale " dwScale;
                print_int32 "dwRate " dwRate; (* dwRate / dwScale == samples/second *)
                print_int32 "dwStart " dwStart;
                print_int32 "dwLength " dwLength;
                print_int32 "dwSuggestedBufferSize " dwSuggestedBufferSize;
                print_int32 "dwQuality " dwQuality;
                print_int32 "dwSampleSize " dwSampleSize;
                print_int16 "rcFrame_x" rcFrame_x;
                print_int16 "rcFrame_y" rcFrame_y;
                print_int16 "rcFrame_dx" rcFrame_dx;
                print_int16 "rcFrame_dy" rcFrame_dy;
                ()
            | _ -> ()
          end;
          
          iter_list (Int32.add pos (Int32.add size2 (Int32.of_int 8))) end_pos
        end 
    
    in
    let pos0 = Int32.of_int 16 in
    iter_list pos0 (Int32.add pos0 size);
(*  Printf.printf "DONE"; print_newline () *)
    ()
  with
  | FormatFound f as e -> raise e
  | _ -> ()

let search_info_mp3 filename =
  try
    let tag = Mp3tag.Id3v1.read filename in
    let info = Mp3tag.info filename in
    Printf.printf "MP3 INFO FOUND"; print_newline ();
    raise (FormatFound (MP3 (tag, info)))
  with
  | FormatFound _ as e -> raise e
  | Not_found -> () (* The file couldn't be found *)
  | x -> ()
      (*
      Printf.printf "error while looking for mp3file %s: %s" filename
        (Printexc2.to_string x); print_newline ()
      *)

let get_info file =
  let ic = open_in file in
  try
    search_info_mp3 file;
    search_info_avi ic ;
    close_in ic;
    let es = 
      try 
        List.map String.lowercase (Filename2.extensions file) 
      with _ -> []
    in
    match List.rev es with
    | "exe" :: _ -> FormatType ("exe", "Pro")
    | "ace" :: _ -> FormatType ("ace", "Pro")
    | "rar" :: _ -> FormatType ("rar", "Pro")
    | "gz"  :: _ -> FormatType ("gzip", "Pro")
    | "z"   :: _ -> FormatType ("compress", "Pro")
    | "tar" :: _ -> FormatType ("tar", "Pro")
    | "bz2" :: _ -> FormatType ("bzip2", "Pro")
    | "gif" :: _ -> FormatType ("gif", "Image")
    | "jpg" :: _ -> FormatType ("jpeg", "Image")
    | "jpeg" :: _ -> FormatType ("jpeg", "Image")
    | "txt" :: _ -> FormatType ("txt", "Doc")
    | "doc" :: _ -> FormatType ("doc", "Doc")
    | "wav" :: _ -> FormatType ("wav", "Audio")
    | "mpg" :: _ -> FormatType ("mpg", "Video")
    | "mpeg" :: _ -> FormatType ("mpg", "Video")
    | "mov" :: _ -> FormatType ("mov", "Video")
    | "asf" :: _ -> FormatType ("asf", "Video")
    | _ -> Unknown_format
  with e -> 
      close_in ic;
      match e with
        FormatFound f -> f
      | e -> 
          Printf.printf "get_info: Exception in %s" (Printexc2.to_string e);
          print_newline ();
          Unknown_format
          
          
module Bchunk =
  struct
    
(*

 /*
  *	binchunker for Unix
  *	Copyright (C) 1998,2001  Heikki Hannikainen <hessu@hes.iki.fi>
  *
  *  This program is free software; you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation; either version 2 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program; if not, write to the Free Software
  *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#define VERSION "1.1.1"
#define USAGE "Usage: bchunk [-v] [-p (PSX)] [-w (wav)] [-s (swabaudio)]\n" \
        "         <image.bin> <image.cue> <basename>\n" \
	"Example: bchunk foo.bin foo.cue foo\n" \
	"  -v  Verbose mode\n" \
	"  -p  PSX mode: truncate MODE2/2352 to 2336 bytes instead of normal 2048\n" \
	"  -w  Output audio files in WAV format\n" \
	"  -s  swabaudio: swap byte order in audio tracks\n"
	
#define VERSTR	"binchunker for Unix, version " VERSION " by Heikki Hannikainen <hessu@hes.iki.fi>\n" \
		"\tCreated with the kind help of Bob Marietta <marietrg@SLU.EDU>,\n" \
		"\tpartly based on his Pascal (Delphi) implementation.\n" \
		"\tSupport for MODE2/2352 ISO tracks thanks to input from\n" \
		"\tGodmar Back <gback@cs.utah.edu> and Colas Nahaboo <Colas@Nahaboo.com>\n" \
		"\tReleased under the GNU GPL, version 2 or later (at your option).\n\n"

#define CUELLEN 1024
#define SECTLEN 2352

#define WAV_RIFF_HLEN 12
#define WAV_FORMAT_HLEN 24
#define WAV_DATA_HLEN 8
#define WAV_HEADER_LEN WAV_RIFF_HLEN + WAV_FORMAT_HLEN + WAV_DATA_HLEN

/*
 *	Ugly way to convert integers to little-endian format.
 *	First let netinet's hton() functions decide if swapping should
 *	be done, then convert back.
 */

#include <inttypes.h>
#include <netinet/in.h>

#define bswap_16(x) \
     ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8))
#define bswap_32(x) \
     ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |  \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))

#define htoles(x) bswap_16(htons(x))
#define htolel(x) bswap_32(htonl(x))



struct track_t {
	int num;
	int mode;
	int audio;
	char *modes;
	char *extension;
	int bstart;
	int bsize;
	long startsect;
	long stopsect;
	long start;
	long stop;
	struct track_t *next;
};

char *basefile = NULL;
char *binfile = NULL;
char *cuefile = NULL;
int verbose = 0;
int psxtruncate = 0;
int swabaudio = 0;
int towav = 0;

/*
 *	Parse arguments
 */

void parse_args(int argc, char *argv[])
{
	int s;
	
	while ((s = getopt(argc, argv, "swvp?h")) != -1) {
		switch (s) {
			case 'v':
				verbose = 1;
				break;
			case 'w':
				towav = 1;
				break;
			case 'p':
				psxtruncate = 1;
				break;
			case 's':
				swabaudio = 1;
				break;
			case '?':
			case 'h':
				fprintf(stderr, "%s", USAGE);
				exit(0);
		}
	}

	if (argc - optind != 3) {
		fprintf(stderr, "%s", USAGE);
		exit(1);
	}
	
	while (optind < argc) {
		switch (argc - optind) {
			case 3:
				binfile = strdup(argv[optind]);
				break;
			case 2:
				cuefile = strdup(argv[optind]);
				break;
			case 1:
				basefile = strdup(argv[optind]);
				break;
			default:
				fprintf(stderr, "%s", USAGE);
				exit(1);
		}
		optind++;
	}
}

/*
 *	Convert a mins:secs:frames format to plain frames
 */

long time2frames(char *s)
{
	int mins = 0, secs = 0, frames = 0;
	char *p, *t;
	
	if (!(p = strchr(s, ':')))
		return -1;
	*p = '\0';
	mins = atoi(s);
	
	p++;
	if (!(t = strchr(p, ':')))
		return -1;
	*t = '\0';
	secs = atoi(p);
	
	t++;
	frames = atoi(t);
	
	return 75 * (mins * 60 + secs) + frames;
}

/*
 *	Parse the mode string
 */

void gettrackmode(struct track_t *track, char *modes)
{
	static char ext_iso[] = "iso";
	static char ext_cdr[] = "cdr";
	static char ext_wav[] = "wav";
	static char ext_ugh[] = "ugh";
	
	track->audio = 0;
	
	if (!strcasecmp(modes, "MODE1/2352")) {
		track->bstart = 16;
		track->bsize = 2048;
		track->extension = ext_iso;
		
	} else if (!strcasecmp(modes, "MODE2/2352")) {
		track->extension = ext_iso;
		if (psxtruncate) {
			/* PSX: truncate from 2352 to 2336 byte tracks */
			track->bstart = 0;
			track->bsize = 2336;
		} else {
			/* Normal MODE2/2352 */
			track->bstart = 24;
			track->bsize = 2048;
		}
		
	} else if (!strcasecmp(modes, "MODE2/2336")) {
		/* WAS 2352 in V1.361B still work?
		 * what if MODE2/2336 single track bin, still 2352 sectors?
		 */
		track->bstart = 16;
		track->bsize = 2336;
		track->extension = ext_iso;
		
	} else if (!strcasecmp(modes, "AUDIO")) {
		track->bstart = 0;
		track->bsize = 2352;
		track->audio = 1;
		if (towav)
			track->extension = ext_wav;
		else
			track->extension = ext_cdr;
	} else {
		printf("(?) ");
		track->bstart = 0;
		track->bsize = 2352;
		track->extension = ext_ugh;
	}
}

/*
 *	return a progress bar
 */

char *progressbar(float f, int l)
{
	static char s[80];
	int i, n;
	
	n = l * f;
	for (i = 0; i < n; i++) {
		s[i] = '*';
	}
	for (; i < l; i++) {
		s[i] = ' ';
	}
	s[i] = '\0';
	
	return s;
}

/*
 *	Write a track
 */

int writetrack(FILE *bf, struct track_t *track, char *bname)
{
	char *fname;
	FILE *f;
	char buf[SECTLEN+10];
	long sz, sect, realsz, reallen;
	char c, *p, *p2, *ep;
	int32_t l;
	int16_t i;
	float fl;
	
	if (!(fname = malloc(strlen(bname) + 8))) {
		fprintf(stderr, "main(): malloc() failed, out of memory\n");
		exit(4);
	}
	sprintf(fname, "%s%2.2d.%s", bname, track->num, track->extension);
	
	printf("%2d: %s ", track->num, fname);
	
	if (!(f = fopen(fname, "w"))) {
		fprintf(stderr, " Could not fopen track file: %s\n", strerror(errno));
		exit(4);
	}
	
	if (fseek(bf, track->start, SEEK_SET)) {
		fprintf(stderr, " Could not fseek to track location: %s\n", strerror(errno));
		exit(4);
	}
	
	reallen = (track->stopsect - track->startsect + 1) * track->bsize;
	if (verbose) {
		printf("\n mmc sectors %ld->%ld (%ld)", track->startsect, track->stopsect, track->stopsect - track->startsect + 1);
		printf("\n mmc bytes %ld->%ld (%ld)", track->start, track->stop, track->stop - track->start + 1);
		printf("\n sector data at %d, %d bytes per sector", track->bstart, track->bsize);
		printf("\n real data %ld bytes", (track->stopsect - track->startsect + 1) * track->bsize);
		printf("\n");
	}

	printf("                                          ");
	
	if ((track->audio) && (towav)) {
		// RIFF header
		fputs("RIFF", f);
		l = htolel(reallen + WAV_DATA_HLEN + WAV_FORMAT_HLEN + 4);
		fwrite(&l, 4, 1, f);  // length of file, starting from WAVE
		fputs("WAVE", f);
		// FORMAT header
		fputs("fmt ", f);
		l = htolel(0x10);     // length of FORMAT header
		fwrite(&l, 4, 1, f);
		i = htoles(0x01);     // constant
		fwrite(&i, 2, 1, f);
		i = htoles(0x02);	// channels
		fwrite(&i, 2, 1, f);
		l = htolel(44100);	// sample rate
		fwrite(&l, 4, 1, f);
		l = htolel(44100 * 4);	// bytes per second
		fwrite(&l, 4, 1, f);
		i = htoles(4);		// bytes per sample
		fwrite(&i, 2, 1, f);
		i = htoles(2*8);	// bits per channel
		fwrite(&i, 2, 1, f);
		// DATA header
		fputs("data", f);
		l = htolel(reallen);
		fwrite(&l, 4, 1, f);
	}
	
	realsz = 0;
	sz = track->start;
	sect = track->startsect;
	fl = 0;
	while ((sect <= track->stopsect) && (fread(buf, SECTLEN, 1, bf) > 0)) {
		if (track->audio) {
			if (swabaudio) {
				/* swap low and high bytes */
				p = &buf[track->bstart];
				ep = p + track->bsize;
				while (p < ep) {
					p2 = p + 1;
					c = *p;
					*p = *p2;
					*p2 = c;
					p += 2;
				}
			}
		}
		if (fwrite(&buf[track->bstart], track->bsize, 1, f) < 1) {
			fprintf(stderr, " Could not write to track: %s\n", strerror(errno));
			exit(4);
		}
		sect++;
		sz += SECTLEN;
		realsz += track->bsize;
		if (((sz / SECTLEN) % 500) == 0) {
			fl = (float)realsz / (float)reallen;
			printf("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b%4ld/%-4ld MB  [%s] %3.0f %%", realsz/1024/1024, reallen/1024/1024, progressbar(fl, 20), fl * 100);
			fflush(stdout);
		}
	}
	
	fl = (float)realsz / (float)reallen;
	printf("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b%4ld/%-4ld MB  [%s] %3.0f %%", realsz/1024/1024, reallen/1024/1024, progressbar(1, 20), fl * 100);
	fflush(stdout);
	
	if (ferror(bf)) {
		fprintf(stderr, " Could not read from %s: %s\n", binfile, strerror(errno));
		exit(4);
	}
	
	if (fclose(f)) {
		fprintf(stderr, " Could not fclose track file: %s\n", strerror(errno));
		exit(4);
	}
	
	printf("\n");
	return 0;
}

/*
 *	Main
 */

int main(int argc, char **argv)
{
	char s[CUELLEN+1];
	char *p, *t;
	int i, idx;
	struct track_t *tracks = NULL;
	struct track_t *track = NULL;
	struct track_t *prevtrack = NULL;
	struct track_t **prevp = &tracks;
	
	FILE *binf, *cuef;
	
	printf("%s", VERSTR);
	
	parse_args(argc, argv);
	
	if (!((binf = fopen(binfile, "r")))) {
		fprintf(stderr, "Could not open BIN %s: %s\n", binfile, strerror(errno));
		return 2;
	}
	
	if (!((cuef = fopen(cuefile, "r")))) {
		fprintf(stderr, "Could not open CUE %s: %s\n", cuefile, strerror(errno));
		return 2;
	}
	
	printf("Reading the CUE file:\n");
	
	/* We don't really care about the first line. */
	if (!fgets(s, CUELLEN, cuef)) {
		fprintf(stderr, "Could not read first line from %s: %s\n", cuefile, strerror(errno));
		return 3;
	}
	
	i = 0;
	while (fgets(s, CUELLEN, cuef)) {
		while ((p = strchr(s, '\r')) || (p = strchr(s, '\n')))
			*p = '\0';
			
		if ((p = strstr(s, "TRACK"))) {
			printf("\nTrack ");
			if (!(p = strchr(p, ' '))) {
				fprintf(stderr, "... ouch, no space after TRACK.\n");
				continue;
			}
			p++;
			if (!(t = strchr(p, ' '))) {
				fprintf(stderr, "... ouch, no space after track number.\n");
				continue;
			}
			*t = '\0';
			
			prevtrack = track;
			if (!(track = malloc(sizeof(struct track_t)))) {
				fprintf(stderr, "main(): malloc() failed, out of memory\n");
				exit(4);
			}
			*prevp = track;
			prevp = &track->next;
			track->next = NULL;
			track->num = atoi(p);
			
			p = t + 1;
			printf("%2d: %-12.12s ", track->num, p);
			track->modes = strdup(p);
			track->extension = NULL;
			track->mode = 0;
			track->audio = 0;
			track->bsize = track->bstart = -1;
			track->bsize = -1;
			track->startsect = track->stopsect = -1;
			
			gettrackmode(track, p);
			
		} else if ((p = strstr(s, "INDEX"))) {
			if (!(p = strchr(p, ' '))) {
				printf("... ouch, no space after INDEX.\n");
				continue;
			}
			p++;
			if (!(t = strchr(p, ' '))) {
				printf("... ouch, no space after index number.\n");
				continue;
			}
			*t = '\0';
			t++;
			idx = atoi(p);
			printf(" %s %s", p, t);
			track->startsect = time2frames(t);
			track->start = track->startsect * SECTLEN;
			if (verbose)
				printf(" (startsect %ld ofs %ld)", track->startsect, track->start);
			if ((prevtrack) && (prevtrack->stopsect < 0)) {
				prevtrack->stopsect = track->startsect;
				prevtrack->stop = track->start - 1;
			}
		}
	}
	
	if (track) {
		fseek(binf, 0, SEEK_END);
		track->stop = ftell(binf);
		track->stopsect = track->stop / SECTLEN;
	}
	
	printf("\n\n");
	
	
	printf("Writing tracks:\n\n");
	for (track = tracks; (track); track = track->next)
		writetrack(binf, track, basefile);
		
	fclose(binf);
	fclose(cuef);
	
	return 0;
}


  *)
    
  end