/*
 * OCaml-Gd. An interface to the Gd library for generating simple images
 * Based on Shawn Wagner's OCamlGD 0.7.0. with some mods from GD4O
 * Copyright (C) 2002  Shawn Wagner
 * Copyright (C) 2003  Matthew C. Gushee
 * Copyright (C) 2005  beedauchon
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "../../../config/config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/alloc.h>

#include <gd.h>
#include <gdfontl.h>
#include <gdfonts.h>
#include <gdfontt.h>
#include <gdfontmb.h>
#include <gdfontg.h>

struct gd_wrapper {
  gdImagePtr im;
};

typedef struct gd_wrapper GdWrapper;

struct font_wrapper {
  gdFontPtr font;
};

typedef struct font_wrapper GdFWrapper;

#define IM_VAL(X) ((*((GdWrapper *)(Data_custom_val(X)))).im)
#define FONT_VAL(X) ((*((GdFWrapper *)(Data_custom_val(X)))).font)

static void ml_gd_finalize(value);
static long ml_gd_hash(value);
static int ml_font_cmp(value, value);
static long ml_font_hash(value);

static struct custom_operations image_t_custom_operations = {
  "GD image/0.1",
  ml_gd_finalize,
  NULL,
  ml_gd_hash,
  NULL,
  NULL
};

static struct custom_operations font_t_custom_operations = {
  "GD font/0.1",
  NULL,
  ml_font_cmp,
  ml_font_hash,
  NULL,
  NULL
};

static gdFontPtr fonts[5] =
{
  NULL,
  NULL,
  NULL,
  NULL,
  NULL
};

static int fonts_init = 0;

void ml_gd_finalize(value v) {
  if (IM_VAL(v))
    gdImageDestroy(IM_VAL(v));
}

long ml_gd_hash(value v) {
  return gdImageSX(IM_VAL(v));
}

int ml_font_cmp(value v1, value v2) {
  return (int)FONT_VAL(v1) - (int)FONT_VAL(v2);
}

static long ml_font_hash(value v) {
  return (long)FONT_VAL(v);
}

value ml_get_font(value i) {
  CAMLparam1(i);  
  CAMLlocal1(v);

  v = alloc_custom(&font_t_custom_operations, sizeof(GdFWrapper), 1, 10);

  if (!fonts_init) {
    fonts[0] = gdFontTiny;
    fonts[1] = gdFontSmall;
    fonts[2] = gdFontMediumBold;
    fonts[3] = gdFontLarge;
    fonts[4] = gdFontGiant;
    fonts_init = 1;
  }

  FONT_VAL(v) = fonts[Int_val(i)];

  CAMLreturn(v);
}

value ml_image_create(value sx, value sy) {
  CAMLparam2(sx, sy);
  CAMLlocal1(v);
  gdImagePtr im;

  im = gdImageCreate(Int_val(sx), Int_val(sy));
  if (!im) 
    raise_constant(*(value *)caml_named_value("gdopen failed"));

  v = alloc_custom(&image_t_custom_operations, sizeof(GdWrapper),
                   (Int_val(sx) * Int_val(sy)) + sizeof(gdImage), 10000);
  IM_VAL(v) = im;

  CAMLreturn(v);
}

value ml_image_open_png(value filename) {
#ifdef HAVE_GD_PNG
  CAMLparam1(filename);
  CAMLlocal1(v);
  FILE *in;
  gdImagePtr im;

  in = fopen(String_val(filename), "rb");
  if (!in)
    raise_not_found();
  
  im = gdImageCreateFromPng(in);

  fclose(in);

  if (!im) 
    raise_constant(*(value *)caml_named_value("gdopen failed"));

  v =  alloc_custom(&image_t_custom_operations, sizeof(GdWrapper),
                    sizeof(gdImage) + (gdImageSX(im) * gdImageSY(im)), 100000);
  IM_VAL(v) = im;

  CAMLreturn(v);
#else
  raise_constant(*(value *)caml_named_value("gd type not supported"));
  return Val_unit;
#endif
}

value ml_image_open_jpeg(value filename) {
#ifdef HAVE_GD_JPG
  FILE *in;
  gdImagePtr im;
  CAMLparam1(filename);
  CAMLlocal1(v);

  in = fopen(String_val(filename), "rb");
  if (!in)
    raise_not_found();
  
  im = gdImageCreateFromJpeg(in);

  fclose(in);

  if (!im) 
    raise_constant(*(value *)caml_named_value("gdopen failed"));

  v =  alloc_custom(&image_t_custom_operations, sizeof(GdWrapper),
                    sizeof(gdImage) + (gdImageSX(im) * gdImageSY(im)), 100000);
  IM_VAL(v) = im;
  CAMLreturn(v);
#else
  raise_constant(*(value *)caml_named_value("gd type not supported"));
  return Val_unit;
#endif
}

value ml_image_line_native(value gdw, value x1, value y1, value x2, value y2,
                           value c) {
  gdImageLine(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2),
              Int_val(c));
  return Val_unit;
}

value ml_image_line(value *argv, int argn) {
  return
    ml_image_line_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value ml_image_dline_native(value gdw, value x1, value y1, value x2, value y2,
                            value c) {
  gdImageDashedLine(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                    Int_val(y2), Int_val(c));
  return Val_unit;
}

value ml_image_dline(value *argv, int argn) {
  return
    ml_image_dline_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value ml_image_rect_native(value gdw, value x1, value y1, value x2, value y2,
                           value c) {
  gdImageRectangle(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                   Int_val(y2), Int_val(c));
  return Val_unit;
}

value ml_image_rect(value *argv, int argn) {
  return
    ml_image_line_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value ml_image_frect_native(value gdw, value x1, value y1, value x2, value y2,
                            value c) {
  gdImageFilledRectangle(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                         Int_val(y2), Int_val(c));
  return Val_unit;
}

value ml_image_frect(value *argv, int argn) {
  return
    ml_image_line_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value ml_image_arc_native(value gdw, value cx, value cy, value w, value h,
                          value s, value e, value c) {
  gdImageArc(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w), Int_val(h),
             Int_val(s), Int_val(e), Int_val(c));
  return Val_unit;
}

value ml_image_arc(value *argv, int argn) {
  return
    ml_image_arc_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                        argv[6], argv[7]);
}

value ml_image_arc_fill_native(value gdw, value cx, value cy, value w, value h,
                          value s, value e, value c, value style) {
  gdImageFilledArc(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w), Int_val(h),
             Int_val(s), Int_val(e), Int_val(c), Int_val(style));
  return Val_unit;
}

value ml_image_arc_fill(value *argv, int argn) {
  return
    ml_image_arc_fill_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                        argv[6], argv[7], argv[8]);
}

value ml_image_border_fill_native(value gdw, value x, value y, value b,
                                  value c) {
  gdImageFillToBorder(IM_VAL(gdw), Int_val(x), Int_val(y), Int_val(b),
                      Int_val(c));
  return Val_unit;
}

value ml_image_border_fill(value *argv, int argn) {
  return
    ml_image_border_fill_native(argv[0], argv[1], argv[2], argv[3], argv[4]);
}

value ml_image_fill(value gdw, value x, value y, value c) {
     gdImageFill(IM_VAL(gdw), Int_val(x), Int_val(y), Int_val(c));
     return Val_unit;
}


value ml_image_char_native(value gdw, value font, value x, value y, value c,
                           value color) {
  gdImageChar(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y), Int_val(c),
              Int_val(color));
  return Val_unit;
}

value ml_image_char(value *argv, int argc) {
  return ml_image_char_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}

value ml_image_charu_native(value gdw, value font, value x, value y, value c,
                            value color) {
  gdImageCharUp(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y), Int_val(c),
                Int_val(color));
  return Val_unit;
}

value ml_image_charu(value *argv, int argc) {
  return ml_image_charu_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                               argv[5]);
}

value ml_image_str_native(value gdw, value font, value x, value y, value s,
                           value color) {
  const char* _s = String_val(s);
  const size_t length = strlen(_s);
  char tmp[length + 1];
  strcpy(tmp, _s);
  gdImageString(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y),
                tmp, Int_val(color));
  return Val_unit;
}

value ml_image_str(value *argv, int argc) {
  return ml_image_str_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}

value ml_image_stru_native(value gdw, value font, value x, value y, value s,
                            value color) {
  const char* _s = String_val(s);
  const size_t length = strlen(_s);
  char tmp[length + 1];
  strcpy(tmp, _s);
  gdImageStringUp(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y),
                  tmp, Int_val(color));
  return Val_unit;
}

value ml_image_stru(value *argv, int argc) {
  return ml_image_stru_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                               argv[5]);
}

value ml_set_pixel(value gdw, value x, value y, value c) {
  gdImageSetPixel(IM_VAL(gdw), Int_val(x), Int_val(y), Int_val(c));
  return Val_unit;
}

value ml_get_pixel(value gdw, value x, value y) {
  return Val_int(gdImageGetPixel(IM_VAL(gdw), Int_val(x), Int_val(y)));
}

value ml_get_width(value gdw) {
  return Val_int(gdImageSX(IM_VAL(gdw)));
}

value ml_get_height(value gdw) {
  return Val_int(gdImageSY(IM_VAL(gdw)));
}

value ml_save_png(value gdw, value filename) {
#ifdef HAVE_GD_PNG
  FILE *out;
  
  out = fopen(String_val(filename), "wb");
  gdImagePng(IM_VAL(gdw), out);
  fclose(out);
#else
  raise_constant(*(value*)caml_named_value("gd type not supported"));
#endif
  return Val_unit;
}

value ml_save_jpeg(value gdw, value filename, value quality) {
#ifdef HAVE_GD_JPG
  FILE *out;

  out = fopen(String_val(filename), "wb");
  gdImageJpeg(IM_VAL(gdw), out, Int_val(quality));
  fclose(out);
#else
  raise_constant(*(value*)caml_named_value("gd type not supported"));
#endif
  return Val_unit;
}

/* Taken from the ocaml source... */
struct channel;
void really_putblock (struct channel *, char *, long);

/* Extract a struct channel * from the heap object representing it */
#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

value ml_dump_png(value gdw, value chan) {
#ifdef HAVE_GD_PNG
  int size;
  void* dat;

  dat = gdImagePngPtr(IM_VAL(gdw), &size);
  really_putblock(Channel(chan), dat, size);
  free(dat);
  
#else
  raise_constant(*(value*)caml_named_value("gd type not supported"));
#endif
  return Val_unit;
}

value ml_dump_jpeg(value gdw, value chan, value quality) {
#ifdef HAVE_GD_JPG
  int size;
  void* dat;

  dat = gdImageJpegPtr(IM_VAL(gdw), &size, Int_val(quality));
  really_putblock(Channel(chan), dat, size);
  free(dat);
  
#else
  raise_constant(*(value*)caml_named_value("gd type not supported"));
#endif
  return Val_unit;
}

value ml_image_color_alloc(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorAllocate(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_color_closest(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorClosest(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_color_closest_hwb(value gdw, value r, value g, value b) {
  int color;

  color =
    gdImageColorClosestHWB(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
  return Val_int(color);
}

value ml_image_color_exact(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorExact(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_color_resolve(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorResolve(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_red_part(value gdw, value c) {
  return Val_int(gdImageRed(IM_VAL(gdw), Int_val(c)));
}

value ml_image_green_part(value gdw, value c) {
  return Val_int(gdImageGreen(IM_VAL(gdw), Int_val(c)));
}

value ml_image_blue_part(value gdw, value c) {
  return Val_int(gdImageBlue(IM_VAL(gdw), Int_val(c)));
}

value ml_image_get_transparent(value gdw) {
  return Val_int(gdImageGetTransparent(IM_VAL(gdw)));
}

value ml_image_set_transparent(value gdw, value c) {
  gdImageColorTransparent(IM_VAL(gdw), Int_val(c));
  return Val_unit;
}

#ifdef HAVE_PNGVERSION
#include <png.h>
#endif

int ml_image_pngversion(void)
{
  CAMLparam0 ();
  CAMLlocal1 (v);
#ifdef HAVE_PNGVERSION
  #include <png.h>
  v = copy_int32 ((int32_t)png_access_version_number());
  CAMLreturn (v);
#else
  raise_constant(*(value *)caml_named_value("gd type not supported"));
  return Val_unit;
#endif
}
