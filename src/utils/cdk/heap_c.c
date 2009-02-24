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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

#ifdef HEAP_DUMP

#include "caml/mlvalues.h"
#include "byterun/alloc.h"
#include "byterun/compact.h"
#include "byterun/custom.h"
#include "byterun/finalise.h"
#include "byterun/gc.h"
#include "byterun/gc_ctrl.h"
#include "byterun/major_gc.h"
#include "byterun/minor_gc.h"
#include "byterun/misc.h"
#include "byterun/stacks.h"
#include <stdio.h>


static int free_words = 0;  
static int used_words = 0;
static int used_objects = 0;
static int tag_objects[256];
static int tag_words[256];
static int tag_sz_objects[256][32];
static int tag_sz_words[256][32];

static int sz_objects[32];
static int sz_words[32];

void heap_dump(void)
{
  char *ch, *chend;
  int i,j,n;
  
  finish_major_cycle();
  
  
  free_words = 0;  
  used_words = 0;
  used_objects = 0;
  
  for(i=0; i < 256; i++){
    tag_words[i] = 0;
    tag_objects[i] = 0;
    
    for(j=0; j < 32; j++){
      tag_sz_objects[i][j] = 0;
      tag_sz_words[i][j] = 0;
    }
  
  }
  
  for(i=0; i < 32; i++){
    sz_objects[i] = 0;
    sz_words[i] = 0;
  }
  
  ch = heap_start;
  while (ch != NULL){
    header_t *p = (header_t *) ch;
    
    chend = ch + Chunk_size (ch);
    while ((char *) p < chend){
      header_t hd = Hd_hp (p);
      mlsize_t sz = Wosize_hd (hd);
      
      if (Is_blue_hd (hd)){
        free_words += sz;
      }else{
        int tag = Tag_hd(hd);

        used_words += sz;
        used_objects++;
        tag_objects[tag]++;
        tag_words[tag] += sz;

        i = 0;
        while(sz >= (int) (1 << i)) i++;

        sz_objects[i]++;
        sz_words[i] += sz;
        tag_sz_objects[tag][i] ++;
        tag_sz_words[tag][i] += sz;
      }
      p += Whsize_wosize (sz);
    }
    ch = Chunk_next (ch);
  }

  printf("***************************************\n");
  printf("Heap dump\n");
  printf("   Free Words: %d\n", free_words);
  printf("   Used Words: %d\n", used_words);
  printf("   Used Objects: %d\n", used_objects);
  printf("   Per tag: \n");
  for(i=0; i < 256; i++){
    if(tag_objects[i] > 0){
      switch(i){
        case 200: printf("     Client  "); break;
        case 201: printf("     Server  "); break;
        case 202: printf("     File    "); break;

        case 246: printf("     Lazy    "); break;
        case 247: printf("     Closure "); break;
        case 248: printf("     Object  "); break;
        case 249: printf("     Infix   "); break;
        case 250: printf("     Forward "); break;
        case 251: printf("     Abstract"); break;
        case 252: printf("     String  "); break;
        case 253: printf("     Double  "); break;
        case 254: printf("     Double[]"); break;
        case 255: printf("     Custom  "); break;
        default:  printf("     %8d",i);
      }
      printf(":    objects %5d [%2d%%] words %6d [%2d%%]\n", 
        tag_objects[i], 100 * tag_objects[i] / used_objects, 
        tag_words[i], 100 * tag_words[i] / used_words
      );
    }
  }

  printf("   Per size: \n");
  for(i=0; i < 32; i++){
    if(sz_objects[i] > 0){
      printf("      < %6d:  %6d objects %6d words [%2d%%]\n",
        1 << i,
        sz_objects[i],
        sz_words[i], 100 * sz_words[i] / used_words);
    }
  }


  printf("   Size per tag: \n");
  for(i=0; i < 256; i++){
    if(tag_objects[i] > 0){
      switch(i){
        case 200: printf("     Client  "); break;
        case 201: printf("     Server  "); break;
        case 202: printf("     File    "); break;

        case 246: printf("     Lazy    "); break;
        case 247: printf("     Closure "); break;
        case 248: printf("     Object  "); break;
        case 249: printf("     Infix   "); break;
        case 250: printf("     Forward "); break;
        case 251: printf("     Abstract"); break;
        case 252: printf("     String  "); break;
        case 253: printf("     Double  "); break;
        case 254: printf("     Double[]"); break;
        case 255: printf("     Custom  "); break;
        default:  printf("     %8d",i);
      }
      printf(" :    ");
      for(j=0; j < 32; j++){
        if(tag_sz_objects[i][j]>0) {
          int p = 100 * tag_sz_words[i][j] / used_words;
          if(p>0)
            printf("%d<%d[%2d%%],", tag_sz_objects[i][j], (int) (1 << j), p);
          else
            printf("%d<%d,", tag_sz_objects[i][j], (int) (1 << j));
        }
      }
      printf("\n");
    }
  }

  printf("***************************************\n");

}



#if 0
#define Next(hp) ((hp) + Bhsize_hp (hp))

static void check_block (FILE *oc, char *hp)
{
  mlsize_t nfields = Wosize_hp (hp);
  mlsize_t i;
  value v = Val_hp (hp);
  value f;
  mlsize_t lastbyte;
  int tag = Tag_hp (hp);
  
  fprintf(oc, "[%x][%d][%d]", v, tag, nfields);
  switch(tag){
    case String_tag:
    case Double_tag:
    case Double_array_tag:
    case Custom_tag: break;
    
    default:
    if(Tag_hp (hp) < No_scan_tag){
      for (i = 0; i < Wosize_hp (hp); i++){
        f = Field (v, i);
        if (Is_block (f) && Is_in_heap (f)) {
          fprintf(oc, "(%x)", f);
        }
      }
    }
  }
  fprintf(oc,"\n");
}

/* Check the heap structure (if compiled in debug mode) and
   gather statistics; return the stats if [returnstats] is true,
   otherwise return [Val_unit].
*/
value heap_dump (value unit)
{

  char *chunk = heap_start, *chunk_end;
  char *cur_hp, *prev_hp;
  header_t cur_hd;

  FILE *oc = fopen("heap.dump", "w");  

  minor_collection();

  while (chunk != NULL){
    chunk_end = chunk + Chunk_size (chunk);
    prev_hp = NULL;
    cur_hp = chunk;
    while (cur_hp < chunk_end){
      cur_hd = Hd_hp (cur_hp);
                                           Assert (Next (cur_hp) <= chunk_end);
      switch (Color_hd (cur_hd)){
      case Caml_white:
        if ((Wosize_hd (cur_hd) == 0)
            || (gc_phase == Phase_sweep && cur_hp >= gc_sweep_hp)){
            /* free block */
          }else{
            check_block (oc, cur_hp);
        }
        break;
      case Caml_gray: case Caml_black:
        check_block (oc, cur_hp);
        break;
      case Caml_blue:
        /* free block */
        break;
      }
      prev_hp = cur_hp;
      cur_hp = Next (cur_hp);
    }                                          Assert (cur_hp == chunk_end);
    chunk = Chunk_next (chunk);
  }
  fclose(oc);

  printf("HEAP DUMPED\n");

  return Val_unit;
}
#endif

value heap_set_tag(value v, value tag)
{
  if(Tag_val(v) == 0) Tag_val(v) = Int_val(tag);
  return Val_unit;
}

#else

#include "caml/mlvalues.h"

value heap_dump (value unit)
{
  return Val_unit;
}

value heap_set_tag(value v, value tag)
{
  return Val_unit;
}

#endif
