
#include "caml/mlvalues.h"

#ifdef DEBUG_MLDONKEY


#include "caml/alloc.h"
#include "caml/compact.h"
#include "caml/custom.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/stacks.h"
#include <stdio.h>

#define Next(hp) ((hp) + Bhsize_hp (hp))

static void check_block (FILE *oc, char *hp)
{
#if 0
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
#endif
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
  minor_collection();

  FILE *oc = fopen("heap.dump", "w");  
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

  return Val_unit;
}
#else

value heap_dump(value unit)
{
  return Val_unit;
}

#endif

value heap_set_tag(value v, value tag)
{
  Tag_val(v) = Int_val(tag);
  return Val_unit;
}

