/* Copyright 2005 b8_bavard, INRIA */
/*
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
*/

/* Stub code to interface with libiconv
 *
 * taken and modified from
 *   glib-2.4.7/glib/gconvert.c
 *   glib-2.4.7/glib/gutf8.c
 *   lablgtk-2.4.0/src/ml_glib.c
 *   libiconv-1.9.1/src/iconv.c
 *
 */

#include "../../../config/config.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifndef DISABLE_ICONV
  #ifdef HAVE_LOCALE_H
    #include <locale.h>
  #endif
  #ifdef HAVE_LIBCHARSET_H
    #include <libcharset.h>
  #endif
  #ifdef HAVE_LANGINFO_H
    #include <langinfo.h>
  #endif
  #include <iconv.h>
#else
  /* This is made up from the headers above. */

  #define LC_CTYPE 0
  char *setlocale (int __category, __const char *__locale)
  {
    return "EN";
  }

  #define CODESET 0
  typedef int nl_item;
  char *nl_langinfo (nl_item __item)
  {
    return "UTF-8";
  }

  typedef void *iconv_t;
  iconv_t iconv_open (__const char *__tocode, __const char *__fromcode)
  {
      /* I guess this is not the right thing to do, but it seems to work partly and does not crash *g*. */
      return (iconv_t) -1;
  }
  size_t iconv (iconv_t __cd, char **__restrict __inbuf,
                     size_t *__restrict __inbytesleft,
                     char **__restrict __outbuf,
                     size_t *__restrict __outbytesleft)
  {
    size_t count;
    if(*__inbytesleft > *__outbytesleft)
      count = *__outbytesleft;
    else
      count = *__inbytesleft;
    memcpy(*__outbuf,*__inbuf,count);
    return count;
  }
  int iconv_close (iconv_t __cd)
  {
      return 0;
  }
#endif

#ifdef WIN32
#define STRICT			/* Strict typing, please */
#include <windows.h>
#undef STRICT
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#define FALSE 0
#define TRUE  1

#define SizedString_val(x) String_val(x), string_length(x)

#define UTF8_COMPUTE(Char, Mask, Len)					      \
  if (Char < 128)							      \
    {									      \
      Len = 1;								      \
      Mask = 0x7f;							      \
    }									      \
  else if ((Char & 0xe0) == 0xc0)					      \
    {									      \
      Len = 2;								      \
      Mask = 0x1f;							      \
    }									      \
  else if ((Char & 0xf0) == 0xe0)					      \
    {									      \
      Len = 3;								      \
      Mask = 0x0f;							      \
    }									      \
  else if ((Char & 0xf8) == 0xf0)					      \
    {									      \
      Len = 4;								      \
      Mask = 0x07;							      \
    }									      \
  else if ((Char & 0xfc) == 0xf8)					      \
    {									      \
      Len = 5;								      \
      Mask = 0x03;							      \
    }									      \
  else if ((Char & 0xfe) == 0xfc)					      \
    {									      \
      Len = 6;								      \
      Mask = 0x01;							      \
    }									      \
  else									      \
    Len = -1;

#define UTF8_LENGTH(Char)              \
  ((Char) < 0x80 ? 1 :                 \
   ((Char) < 0x800 ? 2 :               \
    ((Char) < 0x10000 ? 3 :            \
     ((Char) < 0x200000 ? 4 :          \
      ((Char) < 0x4000000 ? 5 : 6)))))
   

#define UTF8_GET(Result, Chars, Count, Mask, Len)			      \
  (Result) = (Chars)[0] & (Mask);					      \
  for ((Count) = 1; (Count) < (Len); ++(Count))				      \
    {									      \
      if (((Chars)[(Count)] & 0xc0) != 0x80)				      \
	{								      \
	  (Result) = -1;						      \
	  break;							      \
	}								      \
      (Result) <<= 6;							      \
      (Result) |= ((Chars)[(Count)] & 0x3f);				      \
    }

#define UNICODE_VALID(Char)                   \
    ((Char) < 0x110000 &&                     \
     (((Char) & 0xFFFFF800) != 0xD800) &&     \
     ((Char) < 0xFDD0 || (Char) > 0xFDEF) &&  \
     ((Char) & 0xFFFE) != 0xFFFE)

void
raise_error(void)
{
  static const value * closure_f = NULL;
  if (closure_f == NULL) {
    /* First time around, look up by name */
    closure_f = caml_named_value("charset_error");
  }
  raise_constant(*closure_f);
}

int
utf8_validate (const char  *str,
               size_t      max_len,    
               const char  **end)
{

  const char *p;

  if (str == NULL)
    return FALSE;

  if (end)
    *end = str;
  
  p = str;
  
  while ((max_len < 0 || (p - str) < max_len) && *p)
    {
      int i, mask = 0, len;
      unsigned int result;
      unsigned char c = (unsigned char) *p;
      
      UTF8_COMPUTE (c, mask, len);

      if (len == -1)
        break;

      /* check that the expected number of bytes exists in str */
      if (max_len >= 0 &&
          ((max_len - (p - str)) < len))
        break;
        
      UTF8_GET (result, p, i, mask, len);

      if (UTF8_LENGTH (result) != len) /* Check for overlong UTF-8 */
	break;

      if (result == (unsigned int)-1)
        break;

      if (!UNICODE_VALID (result))
	break;
      
      p += len;
    }

  if (end)
    *end = p;

  /* See that we covered the entire length if a length was
   * passed in, or that we ended on a nul if not
   */
  if (max_len >= 0 &&
      p != (str + max_len))
    return FALSE;
  else if (max_len < 0 &&
           *p != '\0')
    return FALSE;
  else
    return TRUE;
}

/* taken and modify from glib gwin32.c */

#ifdef WIN32

/* Borrowed from GNU gettext 0.13.1: */
/* Mingw headers don't have latest language and sublanguage codes.  */

#ifndef LANG_AFRIKAANS
#define LANG_AFRIKAANS 0x36
#endif
#ifndef LANG_ALBANIAN
#define LANG_ALBANIAN 0x1c
#endif
#ifndef LANG_AMHARIC
#define LANG_AMHARIC 0x5e
#endif
#ifndef LANG_ARABIC
#define LANG_ARABIC 0x01
#endif
#ifndef LANG_ARMENIAN
#define LANG_ARMENIAN 0x2b
#endif
#ifndef LANG_ASSAMESE
#define LANG_ASSAMESE 0x4d
#endif
#ifndef LANG_AZERI
#define LANG_AZERI 0x2c
#endif
#ifndef LANG_BASQUE
#define LANG_BASQUE 0x2d
#endif
#ifndef LANG_BELARUSIAN
#define LANG_BELARUSIAN 0x23
#endif
#ifndef LANG_BENGALI
#define LANG_BENGALI 0x45
#endif
#ifndef LANG_BURMESE
#define LANG_BURMESE 0x55
#endif
#ifndef LANG_CAMBODIAN
#define LANG_CAMBODIAN 0x53
#endif
#ifndef LANG_CATALAN
#define LANG_CATALAN 0x03
#endif
#ifndef LANG_CHEROKEE
#define LANG_CHEROKEE 0x5c
#endif
#ifndef LANG_DIVEHI
#define LANG_DIVEHI 0x65
#endif
#ifndef LANG_EDO
#define LANG_EDO 0x66
#endif
#ifndef LANG_ESTONIAN
#define LANG_ESTONIAN 0x25
#endif
#ifndef LANG_FAEROESE
#define LANG_FAEROESE 0x38
#endif
#ifndef LANG_FARSI
#define LANG_FARSI 0x29
#endif
#ifndef LANG_FRISIAN
#define LANG_FRISIAN 0x62
#endif
#ifndef LANG_FULFULDE
#define LANG_FULFULDE 0x67
#endif
#ifndef LANG_GAELIC
#define LANG_GAELIC 0x3c
#endif
#ifndef LANG_GALICIAN
#define LANG_GALICIAN 0x56
#endif
#ifndef LANG_GEORGIAN
#define LANG_GEORGIAN 0x37
#endif
#ifndef LANG_GUARANI
#define LANG_GUARANI 0x74
#endif
#ifndef LANG_GUJARATI
#define LANG_GUJARATI 0x47
#endif
#ifndef LANG_HAUSA
#define LANG_HAUSA 0x68
#endif
#ifndef LANG_HAWAIIAN
#define LANG_HAWAIIAN 0x75
#endif
#ifndef LANG_HEBREW
#define LANG_HEBREW 0x0d
#endif
#ifndef LANG_HINDI
#define LANG_HINDI 0x39
#endif
#ifndef LANG_IBIBIO
#define LANG_IBIBIO 0x69
#endif
#ifndef LANG_IGBO
#define LANG_IGBO 0x70
#endif
#ifndef LANG_INDONESIAN
#define LANG_INDONESIAN 0x21
#endif
#ifndef LANG_INUKTITUT
#define LANG_INUKTITUT 0x5d
#endif
#ifndef LANG_KANNADA
#define LANG_KANNADA 0x4b
#endif
#ifndef LANG_KANURI
#define LANG_KANURI 0x71
#endif
#ifndef LANG_KASHMIRI
#define LANG_KASHMIRI 0x60
#endif
#ifndef LANG_KAZAK
#define LANG_KAZAK 0x3f
#endif
#ifndef LANG_KONKANI
#define LANG_KONKANI 0x57
#endif
#ifndef LANG_KYRGYZ
#define LANG_KYRGYZ 0x40
#endif
#ifndef LANG_LAO
#define LANG_LAO 0x54
#endif
#ifndef LANG_LATIN
#define LANG_LATIN 0x76
#endif
#ifndef LANG_LATVIAN
#define LANG_LATVIAN 0x26
#endif
#ifndef LANG_LITHUANIAN
#define LANG_LITHUANIAN 0x27
#endif
#ifndef LANG_MACEDONIAN
#define LANG_MACEDONIAN 0x2f
#endif
#ifndef LANG_MALAY
#define LANG_MALAY 0x3e
#endif
#ifndef LANG_MALAYALAM
#define LANG_MALAYALAM 0x4c
#endif
#ifndef LANG_MALTESE
#define LANG_MALTESE 0x3a
#endif
#ifndef LANG_MANIPURI
#define LANG_MANIPURI 0x58
#endif
#ifndef LANG_MARATHI
#define LANG_MARATHI 0x4e
#endif
#ifndef LANG_MONGOLIAN
#define LANG_MONGOLIAN 0x50
#endif
#ifndef LANG_NEPALI
#define LANG_NEPALI 0x61
#endif
#ifndef LANG_ORIYA
#define LANG_ORIYA 0x48
#endif
#ifndef LANG_OROMO
#define LANG_OROMO 0x72
#endif
#ifndef LANG_PAPIAMENTU
#define LANG_PAPIAMENTU 0x79
#endif
#ifndef LANG_PASHTO
#define LANG_PASHTO 0x63
#endif
#ifndef LANG_PUNJABI
#define LANG_PUNJABI 0x46
#endif
#ifndef LANG_RHAETO_ROMANCE
#define LANG_RHAETO_ROMANCE 0x17
#endif
#ifndef LANG_SAAMI
#define LANG_SAAMI 0x3b
#endif
#ifndef LANG_SANSKRIT
#define LANG_SANSKRIT 0x4f
#endif
#ifndef LANG_SERBIAN
#define LANG_SERBIAN 0x1a
#endif
#ifndef LANG_SINDHI
#define LANG_SINDHI 0x59
#endif
#ifndef LANG_SINHALESE
#define LANG_SINHALESE 0x5b
#endif
#ifndef LANG_SLOVAK
#define LANG_SLOVAK 0x1b
#endif
#ifndef LANG_SOMALI
#define LANG_SOMALI 0x77
#endif
#ifndef LANG_SORBIAN
#define LANG_SORBIAN 0x2e
#endif
#ifndef LANG_SUTU
#define LANG_SUTU 0x30
#endif
#ifndef LANG_SWAHILI
#define LANG_SWAHILI 0x41
#endif
#ifndef LANG_SYRIAC
#define LANG_SYRIAC 0x5a
#endif
#ifndef LANG_TAGALOG
#define LANG_TAGALOG 0x64
#endif
#ifndef LANG_TAJIK
#define LANG_TAJIK 0x28
#endif
#ifndef LANG_TAMAZIGHT
#define LANG_TAMAZIGHT 0x5f
#endif
#ifndef LANG_TAMIL
#define LANG_TAMIL 0x49
#endif
#ifndef LANG_TATAR
#define LANG_TATAR 0x44
#endif
#ifndef LANG_TELUGU
#define LANG_TELUGU 0x4a
#endif
#ifndef LANG_THAI
#define LANG_THAI 0x1e
#endif
#ifndef LANG_TIBETAN
#define LANG_TIBETAN 0x51
#endif
#ifndef LANG_TIGRINYA
#define LANG_TIGRINYA 0x73
#endif
#ifndef LANG_TSONGA
#define LANG_TSONGA 0x31
#endif
#ifndef LANG_TSWANA
#define LANG_TSWANA 0x32
#endif
#ifndef LANG_TURKMEN
#define LANG_TURKMEN 0x42
#endif
#ifndef LANG_UKRAINIAN
#define LANG_UKRAINIAN 0x22
#endif
#ifndef LANG_URDU
#define LANG_URDU 0x20
#endif
#ifndef LANG_UZBEK
#define LANG_UZBEK 0x43
#endif
#ifndef LANG_VENDA
#define LANG_VENDA 0x33
#endif
#ifndef LANG_VIETNAMESE
#define LANG_VIETNAMESE 0x2a
#endif
#ifndef LANG_WELSH
#define LANG_WELSH 0x52
#endif
#ifndef LANG_XHOSA
#define LANG_XHOSA 0x34
#endif
#ifndef LANG_YI
#define LANG_YI 0x78
#endif
#ifndef LANG_YIDDISH
#define LANG_YIDDISH 0x3d
#endif
#ifndef LANG_YORUBA
#define LANG_YORUBA 0x6a
#endif
#ifndef LANG_ZULU
#define LANG_ZULU 0x35
#endif
#ifndef SUBLANG_ARABIC_SAUDI_ARABIA
#define SUBLANG_ARABIC_SAUDI_ARABIA 0x01
#endif
#ifndef SUBLANG_ARABIC_IRAQ
#define SUBLANG_ARABIC_IRAQ 0x02
#endif
#ifndef SUBLANG_ARABIC_EGYPT
#define SUBLANG_ARABIC_EGYPT 0x03
#endif
#ifndef SUBLANG_ARABIC_LIBYA
#define SUBLANG_ARABIC_LIBYA 0x04
#endif
#ifndef SUBLANG_ARABIC_ALGERIA
#define SUBLANG_ARABIC_ALGERIA 0x05
#endif
#ifndef SUBLANG_ARABIC_MOROCCO
#define SUBLANG_ARABIC_MOROCCO 0x06
#endif
#ifndef SUBLANG_ARABIC_TUNISIA
#define SUBLANG_ARABIC_TUNISIA 0x07
#endif
#ifndef SUBLANG_ARABIC_OMAN
#define SUBLANG_ARABIC_OMAN 0x08
#endif
#ifndef SUBLANG_ARABIC_YEMEN
#define SUBLANG_ARABIC_YEMEN 0x09
#endif
#ifndef SUBLANG_ARABIC_SYRIA
#define SUBLANG_ARABIC_SYRIA 0x0a
#endif
#ifndef SUBLANG_ARABIC_JORDAN
#define SUBLANG_ARABIC_JORDAN 0x0b
#endif
#ifndef SUBLANG_ARABIC_LEBANON
#define SUBLANG_ARABIC_LEBANON 0x0c
#endif
#ifndef SUBLANG_ARABIC_KUWAIT
#define SUBLANG_ARABIC_KUWAIT 0x0d
#endif
#ifndef SUBLANG_ARABIC_UAE
#define SUBLANG_ARABIC_UAE 0x0e
#endif
#ifndef SUBLANG_ARABIC_BAHRAIN
#define SUBLANG_ARABIC_BAHRAIN 0x0f
#endif
#ifndef SUBLANG_ARABIC_QATAR
#define SUBLANG_ARABIC_QATAR 0x10
#endif
#ifndef SUBLANG_AZERI_LATIN
#define SUBLANG_AZERI_LATIN 0x01
#endif
#ifndef SUBLANG_AZERI_CYRILLIC
#define SUBLANG_AZERI_CYRILLIC 0x02
#endif
#ifndef SUBLANG_BENGALI_INDIA
#define SUBLANG_BENGALI_INDIA 0x01
#endif
#ifndef SUBLANG_BENGALI_BANGLADESH
#define SUBLANG_BENGALI_BANGLADESH 0x02
#endif
#ifndef SUBLANG_CHINESE_MACAU
#define SUBLANG_CHINESE_MACAU 0x05
#endif
#ifndef SUBLANG_ENGLISH_SOUTH_AFRICA
#define SUBLANG_ENGLISH_SOUTH_AFRICA 0x07
#endif
#ifndef SUBLANG_ENGLISH_JAMAICA
#define SUBLANG_ENGLISH_JAMAICA 0x08
#endif
#ifndef SUBLANG_ENGLISH_CARIBBEAN
#define SUBLANG_ENGLISH_CARIBBEAN 0x09
#endif
#ifndef SUBLANG_ENGLISH_BELIZE
#define SUBLANG_ENGLISH_BELIZE 0x0a
#endif
#ifndef SUBLANG_ENGLISH_TRINIDAD
#define SUBLANG_ENGLISH_TRINIDAD 0x0b
#endif
#ifndef SUBLANG_ENGLISH_ZIMBABWE
#define SUBLANG_ENGLISH_ZIMBABWE 0x0c
#endif
#ifndef SUBLANG_ENGLISH_PHILIPPINES
#define SUBLANG_ENGLISH_PHILIPPINES 0x0d
#endif
#ifndef SUBLANG_ENGLISH_INDONESIA
#define SUBLANG_ENGLISH_INDONESIA 0x0e
#endif
#ifndef SUBLANG_ENGLISH_HONGKONG
#define SUBLANG_ENGLISH_HONGKONG 0x0f
#endif
#ifndef SUBLANG_ENGLISH_INDIA
#define SUBLANG_ENGLISH_INDIA 0x10
#endif
#ifndef SUBLANG_ENGLISH_MALAYSIA
#define SUBLANG_ENGLISH_MALAYSIA 0x11
#endif
#ifndef SUBLANG_ENGLISH_SINGAPORE
#define SUBLANG_ENGLISH_SINGAPORE 0x12
#endif
#ifndef SUBLANG_FRENCH_LUXEMBOURG
#define SUBLANG_FRENCH_LUXEMBOURG 0x05
#endif
#ifndef SUBLANG_FRENCH_MONACO
#define SUBLANG_FRENCH_MONACO 0x06
#endif
#ifndef SUBLANG_FRENCH_WESTINDIES
#define SUBLANG_FRENCH_WESTINDIES 0x07
#endif
#ifndef SUBLANG_FRENCH_REUNION
#define SUBLANG_FRENCH_REUNION 0x08
#endif
#ifndef SUBLANG_FRENCH_CONGO
#define SUBLANG_FRENCH_CONGO 0x09
#endif
#ifndef SUBLANG_FRENCH_SENEGAL
#define SUBLANG_FRENCH_SENEGAL 0x0a
#endif
#ifndef SUBLANG_FRENCH_CAMEROON
#define SUBLANG_FRENCH_CAMEROON 0x0b
#endif
#ifndef SUBLANG_FRENCH_COTEDIVOIRE
#define SUBLANG_FRENCH_COTEDIVOIRE 0x0c
#endif
#ifndef SUBLANG_FRENCH_MALI
#define SUBLANG_FRENCH_MALI 0x0d
#endif
#ifndef SUBLANG_FRENCH_MOROCCO
#define SUBLANG_FRENCH_MOROCCO 0x0e
#endif
#ifndef SUBLANG_FRENCH_HAITI
#define SUBLANG_FRENCH_HAITI 0x0f
#endif
#ifndef SUBLANG_GERMAN_LUXEMBOURG
#define SUBLANG_GERMAN_LUXEMBOURG 0x04
#endif
#ifndef SUBLANG_GERMAN_LIECHTENSTEIN
#define SUBLANG_GERMAN_LIECHTENSTEIN 0x05
#endif
#ifndef SUBLANG_KASHMIRI_INDIA
#define SUBLANG_KASHMIRI_INDIA 0x02
#endif
#ifndef SUBLANG_MALAY_MALAYSIA
#define SUBLANG_MALAY_MALAYSIA 0x01
#endif
#ifndef SUBLANG_MALAY_BRUNEI_DARUSSALAM
#define SUBLANG_MALAY_BRUNEI_DARUSSALAM 0x02
#endif
#ifndef SUBLANG_NEPALI_INDIA
#define SUBLANG_NEPALI_INDIA 0x02
#endif
#ifndef SUBLANG_PUNJABI_INDIA
#define SUBLANG_PUNJABI_INDIA 0x01
#endif
#ifndef SUBLANG_PUNJABI_PAKISTAN
#define SUBLANG_PUNJABI_PAKISTAN 0x02
#endif
#ifndef SUBLANG_ROMANIAN_ROMANIA
#define SUBLANG_ROMANIAN_ROMANIA 0x01
#endif
#ifndef SUBLANG_ROMANIAN_MOLDOVA
#define SUBLANG_ROMANIAN_MOLDOVA 0x02
#endif
#ifndef SUBLANG_SERBIAN_LATIN
#define SUBLANG_SERBIAN_LATIN 0x02
#endif
#ifndef SUBLANG_SERBIAN_CYRILLIC
#define SUBLANG_SERBIAN_CYRILLIC 0x03
#endif
#ifndef SUBLANG_SINDHI_PAKISTAN
#define SUBLANG_SINDHI_PAKISTAN 0x01
#endif
#ifndef SUBLANG_SINDHI_AFGHANISTAN
#define SUBLANG_SINDHI_AFGHANISTAN 0x02
#endif
#ifndef SUBLANG_SPANISH_GUATEMALA
#define SUBLANG_SPANISH_GUATEMALA 0x04
#endif
#ifndef SUBLANG_SPANISH_COSTA_RICA
#define SUBLANG_SPANISH_COSTA_RICA 0x05
#endif
#ifndef SUBLANG_SPANISH_PANAMA
#define SUBLANG_SPANISH_PANAMA 0x06
#endif
#ifndef SUBLANG_SPANISH_DOMINICAN_REPUBLIC
#define SUBLANG_SPANISH_DOMINICAN_REPUBLIC 0x07
#endif
#ifndef SUBLANG_SPANISH_VENEZUELA
#define SUBLANG_SPANISH_VENEZUELA 0x08
#endif
#ifndef SUBLANG_SPANISH_COLOMBIA
#define SUBLANG_SPANISH_COLOMBIA 0x09
#endif
#ifndef SUBLANG_SPANISH_PERU
#define SUBLANG_SPANISH_PERU 0x0a
#endif
#ifndef SUBLANG_SPANISH_ARGENTINA
#define SUBLANG_SPANISH_ARGENTINA 0x0b
#endif
#ifndef SUBLANG_SPANISH_ECUADOR
#define SUBLANG_SPANISH_ECUADOR 0x0c
#endif
#ifndef SUBLANG_SPANISH_CHILE
#define SUBLANG_SPANISH_CHILE 0x0d
#endif
#ifndef SUBLANG_SPANISH_URUGUAY
#define SUBLANG_SPANISH_URUGUAY 0x0e
#endif
#ifndef SUBLANG_SPANISH_PARAGUAY
#define SUBLANG_SPANISH_PARAGUAY 0x0f
#endif
#ifndef SUBLANG_SPANISH_BOLIVIA
#define SUBLANG_SPANISH_BOLIVIA 0x10
#endif
#ifndef SUBLANG_SPANISH_EL_SALVADOR
#define SUBLANG_SPANISH_EL_SALVADOR 0x11
#endif
#ifndef SUBLANG_SPANISH_HONDURAS
#define SUBLANG_SPANISH_HONDURAS 0x12
#endif
#ifndef SUBLANG_SPANISH_NICARAGUA
#define SUBLANG_SPANISH_NICARAGUA 0x13
#endif
#ifndef SUBLANG_SPANISH_PUERTO_RICO
#define SUBLANG_SPANISH_PUERTO_RICO 0x14
#endif
#ifndef SUBLANG_SWEDISH_FINLAND
#define SUBLANG_SWEDISH_FINLAND 0x02
#endif
#ifndef SUBLANG_TAMAZIGHT_ARABIC
#define SUBLANG_TAMAZIGHT_ARABIC 0x01
#endif
#ifndef SUBLANG_TAMAZIGHT_ALGERIA_LATIN
#define SUBLANG_TAMAZIGHT_ALGERIA_LATIN 0x02
#endif
#ifndef SUBLANG_TIGRINYA_ETHIOPIA
#define SUBLANG_TIGRINYA_ETHIOPIA 0x01
#endif
#ifndef SUBLANG_TIGRINYA_ERITREA
#define SUBLANG_TIGRINYA_ERITREA 0x02
#endif
#ifndef SUBLANG_URDU_PAKISTAN
#define SUBLANG_URDU_PAKISTAN 0x01
#endif
#ifndef SUBLANG_URDU_INDIA
#define SUBLANG_URDU_INDIA 0x02
#endif
#ifndef SUBLANG_UZBEK_LATIN
#define SUBLANG_UZBEK_LATIN 0x01
#endif
#ifndef SUBLANG_UZBEK_CYRILLIC
#define SUBLANG_UZBEK_CYRILLIC 0x02
#endif

char *
ml_win32_get_default_language (void)
{
  LCID lcid;
  LANGID langid;
  char *ev;
  int primary, sub;
  char *l = "C", *sl = NULL;
  char bfr[20];

  /* Let the user override the system settings through environment
     variables, as on POSIX systems.  */
  if (((ev = getenv ("LC_ALL")) != NULL && ev[0] != '\0')
      || ((ev = getenv ("LC_MESSAGES")) != NULL && ev[0] != '\0')
      || ((ev = getenv ("LANG")) != NULL && ev[0] != '\0'))
    return strdup (ev);

  /* Use native Win32 API locale ID.  */
  lcid = GetThreadLocale ();

  /* Strip off the sorting rules, keep only the language part.  */
  langid = LANGIDFROMLCID (lcid);

  /* Split into language and territory part.  */
  primary = PRIMARYLANGID (langid);
  sub = SUBLANGID (langid);
  switch (primary)
    {
    case LANG_AFRIKAANS: l = "af"; sl = "ZA"; break;
    case LANG_ALBANIAN: l = "sq"; sl = "AL"; break;
    case LANG_ARABIC:
      l = "ar";
      switch (sub)
	{
	case SUBLANG_ARABIC_SAUDI_ARABIA: sl = "SA"; break;
	case SUBLANG_ARABIC_IRAQ: sl = "IQ"; break;
	case SUBLANG_ARABIC_EGYPT: sl = "EG"; break;
	case SUBLANG_ARABIC_LIBYA: sl = "LY"; break;
	case SUBLANG_ARABIC_ALGERIA: sl = "DZ"; break;
	case SUBLANG_ARABIC_MOROCCO: sl = "MA"; break;
	case SUBLANG_ARABIC_TUNISIA: sl = "TN"; break;
	case SUBLANG_ARABIC_OMAN: sl = "OM"; break;
	case SUBLANG_ARABIC_YEMEN: sl = "YE"; break;
	case SUBLANG_ARABIC_SYRIA: sl = "SY"; break;
	case SUBLANG_ARABIC_JORDAN: sl = "JO"; break;
	case SUBLANG_ARABIC_LEBANON: sl = "LB"; break;
	case SUBLANG_ARABIC_KUWAIT: sl = "KW"; break;
	case SUBLANG_ARABIC_UAE: sl = "AE"; break;
	case SUBLANG_ARABIC_BAHRAIN: sl = "BH"; break;
	case SUBLANG_ARABIC_QATAR: sl = "QA"; break;
	}
      break;
    case LANG_ARMENIAN: l = "hy"; sl = "AM"; break;
    case LANG_ASSAMESE: l = "as"; sl = "IN"; break;
    case LANG_AZERI:
      l = "az";
      switch (sub)
	{
	/* FIXME: Adjust this when Azerbaijani locales appear on Unix.  */
	case SUBLANG_AZERI_LATIN: sl = "AZ@latin"; break;
	case SUBLANG_AZERI_CYRILLIC: sl = "AZ@cyrillic"; break;
	}
      break;
    case LANG_BASQUE:
      l = "eu"; /* sl could be "ES" or "FR".  */
      break;
    case LANG_BELARUSIAN: l = "be"; sl = "BY"; break;
    case LANG_BENGALI:
      l = "bn";
      switch (sub)
	{
	case SUBLANG_BENGALI_INDIA: sl = "IN"; break;
	case SUBLANG_BENGALI_BANGLADESH: sl = "BD"; break;
	}
      break;
    case LANG_BULGARIAN: l = "bg"; sl = "BG"; break;
    case LANG_BURMESE: l = "my"; sl = "MM"; break;
    case LANG_CAMBODIAN: l = "km"; sl = "KH"; break;
    case LANG_CATALAN: l = "ca"; sl = "ES"; break;
    case LANG_CHINESE:
      l = "zh";
      switch (sub)
	{
	case SUBLANG_CHINESE_TRADITIONAL: sl = "TW"; break;
	case SUBLANG_CHINESE_SIMPLIFIED: sl = "CN"; break;
	case SUBLANG_CHINESE_HONGKONG: sl = "HK"; break;
	case SUBLANG_CHINESE_SINGAPORE: sl = "SG"; break;
	case SUBLANG_CHINESE_MACAU: sl = "MO"; break;
	}
      break;
    case LANG_CROATIAN:		/* LANG_CROATIAN == LANG_SERBIAN */
      switch (sub)
	{
	/* FIXME: How to distinguish Croatian and Latin Serbian locales?  */
	case SUBLANG_SERBIAN_LATIN: l = "sr"; sl = "@Latn"; break;
	case SUBLANG_SERBIAN_CYRILLIC: l = "sr"; break;
	default: l = "hr"; sl = "HR";
	}
      break;
    case LANG_CZECH: l = "cs"; sl = "CZ"; break;
    case LANG_DANISH: l = "da"; sl = "DK"; break;
    case LANG_DIVEHI: l = "div"; sl = "MV"; break;
    case LANG_DUTCH:
      l = "nl";
      switch (sub)
	{
	case SUBLANG_DUTCH: sl = "NL"; break;
	case SUBLANG_DUTCH_BELGIAN: sl = "BE"; break;
	}
      break;
    case LANG_ENGLISH:
      l = "en";
      switch (sub)
	{
	case SUBLANG_ENGLISH_US: sl = "US"; break;
	case SUBLANG_ENGLISH_UK: sl = "GB"; break;
	case SUBLANG_ENGLISH_AUS: sl = "AU"; break;
	case SUBLANG_ENGLISH_CAN: sl = "CA"; break;
	case SUBLANG_ENGLISH_NZ: sl = "NZ"; break;
	case SUBLANG_ENGLISH_EIRE: sl = "IE"; break;
	case SUBLANG_ENGLISH_SOUTH_AFRICA: sl = "ZA"; break;
	case SUBLANG_ENGLISH_JAMAICA: sl = "JM"; break;
	case SUBLANG_ENGLISH_CARIBBEAN: sl = "GD"; break; /* Grenada? */
	case SUBLANG_ENGLISH_BELIZE: sl = "BZ"; break;
	case SUBLANG_ENGLISH_TRINIDAD: sl = "TT"; break;
	case SUBLANG_ENGLISH_ZIMBABWE: sl = "ZW"; break;
	case SUBLANG_ENGLISH_PHILIPPINES: sl = "PH"; break;
	case SUBLANG_ENGLISH_INDONESIA: sl = "ID"; break;
	case SUBLANG_ENGLISH_HONGKONG: sl = "HK"; break;
	case SUBLANG_ENGLISH_INDIA: sl = "IN"; break;
	case SUBLANG_ENGLISH_MALAYSIA: sl = "MY"; break;
	case SUBLANG_ENGLISH_SINGAPORE: sl = "SG"; break;
	}
      break;
    case LANG_ESTONIAN: l = "et"; sl = "EE"; break;
    case LANG_FAEROESE: l = "fo"; sl = "FO"; break;
    case LANG_FARSI: l = "fa"; sl = "IR"; break;
    case LANG_FINNISH: l = "fi"; sl = "FI"; break;
    case LANG_FRENCH:
      l = "fr";
      switch (sub)
	{
	case SUBLANG_FRENCH: sl = "FR"; break;
	case SUBLANG_FRENCH_BELGIAN: sl = "BE"; break;
	case SUBLANG_FRENCH_CANADIAN: sl = "CA"; break;
	case SUBLANG_FRENCH_SWISS: sl = "CH"; break;
	case SUBLANG_FRENCH_LUXEMBOURG: sl = "LU"; break;
	case SUBLANG_FRENCH_MONACO: sl = "MC"; break;
	case SUBLANG_FRENCH_WESTINDIES: break;
	case SUBLANG_FRENCH_REUNION: sl = "RE"; break;
	case SUBLANG_FRENCH_CONGO: sl = "CG"; break;
	case SUBLANG_FRENCH_SENEGAL: sl = "SN"; break;
	case SUBLANG_FRENCH_CAMEROON: sl = "CM"; break;
	case SUBLANG_FRENCH_COTEDIVOIRE: sl = "CI"; break;
	case SUBLANG_FRENCH_MALI: sl = "ML"; break;
	case SUBLANG_FRENCH_MOROCCO: sl = "MA"; break;
	case SUBLANG_FRENCH_HAITI: sl = "HT"; break;
	}
      break;
    case LANG_FRISIAN: l = "fy"; sl ="NL"; break;
    case LANG_FULFULDE: l = "ful"; sl = "NG"; break;
    case LANG_GAELIC:
      switch (sub)
	{
	case 0x01: /* SCOTTISH */ l = "gd"; sl = "GB"; break;
	case 0x02: /* IRISH */ l = "ga"; sl = "IE"; break;
	}
      break;
    case LANG_GALICIAN: l = "gl"; sl = "ES"; break;
    case LANG_GEORGIAN: l = "ka"; sl = "GE"; break;
    case LANG_GERMAN:
      l = "de";
      switch (sub)
	{
	case SUBLANG_GERMAN: sl = "DE"; break;
	case SUBLANG_GERMAN_SWISS: sl = "CH"; break;
	case SUBLANG_GERMAN_AUSTRIAN: sl = "AT"; break;
	case SUBLANG_GERMAN_LUXEMBOURG: sl = "LU"; break;
	case SUBLANG_GERMAN_LIECHTENSTEIN: sl = "LI"; break;
	}
      break;
    case LANG_GREEK: l = "el"; sl = "GR"; break;
    case LANG_GUARANI: l = "gn"; sl = "PY"; break;
    case LANG_GUJARATI: l = "gu"; sl = "IN"; break;
    case LANG_HAUSA: l = "ha"; sl = "NG"; break;
    case LANG_HAWAIIAN:
      /* FIXME: Do they mean Hawaiian ("haw_US", 1000 speakers)
       * or Hawaii Creole English ("cpe_US", 600000 speakers)?
       */
      l = "cpe";
      sl = "US";
      break;
    case LANG_HEBREW: l = "he"; sl = "IL"; break;
    case LANG_HINDI: l = "hi"; sl = "IN"; break;
    case LANG_HUNGARIAN: l = "hu"; sl = "HU"; break;
    case LANG_IBIBIO: l = "nic"; sl = "NG"; break;
    case LANG_ICELANDIC: l = "is"; sl = "IS"; break;
    case LANG_IGBO: l = "ibo"; sl = "NG"; break;
    case LANG_INDONESIAN: l = "id"; sl = "ID"; break;
    case LANG_INUKTITUT: l = "iu"; sl = "CA"; break;
    case LANG_ITALIAN:
      l = "it";
      switch (sub)
	{
	case SUBLANG_ITALIAN: sl = "IT"; break;
	case SUBLANG_ITALIAN_SWISS: sl = "CH"; break;
	}
      break;
    case LANG_JAPANESE: l = "ja"; sl = "JP"; break;
    case LANG_KANNADA: l = "kn"; sl = "IN"; break;
    case LANG_KANURI: l = "kau"; sl = "NG"; break;
    case LANG_KASHMIRI:
      l = "ks";
      switch (sub)
	{
	case SUBLANG_DEFAULT: sl = "PK"; break;
	case SUBLANG_KASHMIRI_INDIA: sl = "IN"; break;
	}
      break;
    case LANG_KAZAK: l = "kk"; sl = "KZ"; break;
    case LANG_KONKANI:
      /* FIXME: Adjust this when such locales appear on Unix.  */
      l = "kok";
      sl = "IN";
      break;
    case LANG_KOREAN: l = "ko"; sl = "KR"; break;
    case LANG_KYRGYZ: l = "ky"; sl = "KG"; break; 
    case LANG_LAO: l = "lo"; sl = "LA"; break;
    case LANG_LATIN: l = "la"; sl = "VA"; break;
    case LANG_LATVIAN: l = "lv"; sl = "LV"; break;
    case LANG_LITHUANIAN: l = "lt"; sl = "LT"; break;
    case LANG_MACEDONIAN: l = "mk"; sl = "MK"; break;
    case LANG_MALAY:
      l = "ms";
      switch (sub)
	{
	case SUBLANG_MALAY_MALAYSIA: sl = "MY"; break;
	case SUBLANG_MALAY_BRUNEI_DARUSSALAM: sl = "BN"; break;
	}
      break;
    case LANG_MALAYALAM: l = "ml"; sl = "IN"; break;
    case LANG_MANIPURI:
      /* FIXME: Adjust this when such locales appear on Unix.  */
      l = "mni";
      sl = "IN";
      break;
    case LANG_MARATHI: l = "mr"; sl = "IN"; break;
    case LANG_MONGOLIAN:
      /* Ambiguous: could be "mn_CN" or "mn_MN".  */
      l = "mn";
      break;
    case LANG_NEPALI:
      l = "ne";
      switch (sub)
	{
	case SUBLANG_DEFAULT: sl = "NP"; break;
	case SUBLANG_NEPALI_INDIA: sl = "IN"; break;
	}
      break;
    case LANG_NORWEGIAN:
      l = "no";
      switch (sub)
	{
	case SUBLANG_NORWEGIAN_BOKMAL: sl = "NO"; break;
	case SUBLANG_NORWEGIAN_NYNORSK: l = "nn"; sl = "NO"; break;
	}
      break;
    case LANG_ORIYA: l = "or"; sl = "IN"; break;
    case LANG_OROMO: l = "om"; sl = "ET"; break;
    case LANG_PAPIAMENTU: l = "pap"; sl = "AN"; break;
    case LANG_PASHTO:
      /* Ambiguous: could be "ps_PK" or "ps_AF".  */
      l = "ps";
      break;
    case LANG_POLISH: l = "pl"; sl = "PL"; break;
    case LANG_PORTUGUESE:
      l = "pt";
      switch (sub)
	{
	case SUBLANG_PORTUGUESE: sl = "PT"; break;
	case SUBLANG_PORTUGUESE_BRAZILIAN: sl = "BR"; break;
	}
      break;
    case LANG_PUNJABI:
      l = "pa";
      switch (sub)
	{
	case SUBLANG_PUNJABI_INDIA: sl = "IN"; break; /* Gurmukhi script */
	case SUBLANG_PUNJABI_PAKISTAN: sl = "PK"; break; /* Arabic script */
	}
      break;
    case LANG_RHAETO_ROMANCE: l = "rm"; sl = "CH"; break;
    case LANG_ROMANIAN:
      l = "ro";
      switch (sub)
	{
	case SUBLANG_ROMANIAN_ROMANIA: sl = "RO"; break;
	case SUBLANG_ROMANIAN_MOLDOVA: sl = "MD"; break;
	}
      break;
    case LANG_RUSSIAN:
      l = "ru";/* Ambiguous: could be "ru_RU" or "ru_UA" or "ru_MD". */
      break;
    case LANG_SAAMI: /* actually Northern Sami */ l = "se"; sl = "NO"; break;
    case LANG_SANSKRIT: l = "sa"; sl = "IN"; break;
    case LANG_SINDHI: l = "sd";
      switch (sub)
	{
	case SUBLANG_SINDHI_PAKISTAN: sl = "PK"; break;
	case SUBLANG_SINDHI_AFGHANISTAN: sl = "AF"; break;
	}
      break;
    case LANG_SINHALESE: l = "si"; sl = "LK"; break;
    case LANG_SLOVAK: l = "sk"; sl = "SK"; break;
    case LANG_SLOVENIAN: l = "sl"; sl = "SI"; break;
    case LANG_SOMALI: l = "so"; sl = "SO"; break;
    case LANG_SORBIAN:
      /* FIXME: Adjust this when such locales appear on Unix.  */
      l = "wen";
      sl = "DE";
      break;
    case LANG_SPANISH:
      l = "es";
      switch (sub)
	{
	case SUBLANG_SPANISH: sl = "ES"; break;
	case SUBLANG_SPANISH_MEXICAN: sl = "MX"; break;
	case SUBLANG_SPANISH_MODERN:
	  sl = "ES@modern"; break;	/* not seen on Unix */
	case SUBLANG_SPANISH_GUATEMALA: sl = "GT"; break;
	case SUBLANG_SPANISH_COSTA_RICA: sl = "CR"; break;
	case SUBLANG_SPANISH_PANAMA: sl = "PA"; break;
	case SUBLANG_SPANISH_DOMINICAN_REPUBLIC: sl = "DO"; break;
	case SUBLANG_SPANISH_VENEZUELA: sl = "VE"; break;
	case SUBLANG_SPANISH_COLOMBIA: sl = "CO"; break;
	case SUBLANG_SPANISH_PERU: sl = "PE"; break;
	case SUBLANG_SPANISH_ARGENTINA: sl = "AR"; break;
	case SUBLANG_SPANISH_ECUADOR: sl = "EC"; break;
	case SUBLANG_SPANISH_CHILE: sl = "CL"; break;
	case SUBLANG_SPANISH_URUGUAY: sl = "UY"; break;
	case SUBLANG_SPANISH_PARAGUAY: sl = "PY"; break;
	case SUBLANG_SPANISH_BOLIVIA: sl = "BO"; break;
	case SUBLANG_SPANISH_EL_SALVADOR: sl = "SV"; break;
	case SUBLANG_SPANISH_HONDURAS: sl = "HN"; break;
	case SUBLANG_SPANISH_NICARAGUA: sl = "NI"; break;
	case SUBLANG_SPANISH_PUERTO_RICO: sl = "PR"; break;
	}
      break;
    case LANG_SUTU: l = "bnt"; sl = "TZ"; break; /* or "st_LS" or "nso_ZA"? */ 
    case LANG_SWAHILI: l = "sw"; sl = "KE"; break;
    case LANG_SWEDISH:
      l = "sv";
      switch (sub)
	{
	case SUBLANG_DEFAULT: sl = "SE"; break;
	case SUBLANG_SWEDISH_FINLAND: sl = "FI"; break;
	}
      break;
    case LANG_SYRIAC: l = "syr"; sl = "TR"; break; /* An extinct language. */
    case LANG_TAGALOG: l = "tl"; sl = "PH"; break;
    case LANG_TAJIK: l = "tg"; sl = "TJ"; break;
    case LANG_TAMIL:
      l = "ta"; /* Ambiguous: could be "ta_IN" or "ta_LK" or "ta_SG". */
      break;
    case LANG_TATAR: l = "tt"; sl = "RU"; break;
    case LANG_TELUGU: l = "te"; sl = "IN"; break;
    case LANG_THAI: l = "th"; sl = "TH"; break;
    case LANG_TIBETAN: l = "bo"; sl = "CN"; break;
    case LANG_TIGRINYA:
      l = "ti";
      switch (sub)
	{
	case SUBLANG_TIGRINYA_ETHIOPIA: sl = "ET"; break;
	case SUBLANG_TIGRINYA_ERITREA: sl = "ER"; break;
	}
      break;
    case LANG_TSONGA: l = "ts"; sl = "ZA"; break;
    case LANG_TSWANA: l = "tn"; sl = "BW"; break;
    case LANG_TURKISH: l = "tr"; sl = "TR"; break;
    case LANG_TURKMEN: l = "tk"; sl = "TM"; break;
    case LANG_UKRAINIAN: l = "uk"; sl = "UA"; break;
    case LANG_URDU:
      l = "ur";
      switch (sub)
	{
	case SUBLANG_URDU_PAKISTAN: sl = "PK"; break;
	case SUBLANG_URDU_INDIA: sl = "IN"; break;
	}
      break;
    case LANG_UZBEK:
      l = "uz";
      switch (sub)
	{
	case SUBLANG_UZBEK_LATIN: sl = "UZ"; break;
	case SUBLANG_UZBEK_CYRILLIC: sl = "UZ@cyrillic"; break;
	}
      break;
    case LANG_VENDA:
      /* FIXME: It's not clear whether Venda has the ISO 639-2 two-letter code
	 "ve" or not.
	 http://www.loc.gov/standards/iso639-2/englangn.html has it, but
	 http://lcweb.loc.gov/standards/iso639-2/codechanges.html doesn't,  */
      l = "ven"; /* or "ve"? */
      sl = "ZA";
      break;
    case LANG_VIETNAMESE: l = "vi"; sl = "VN"; break;
    case LANG_WELSH: l = "cy"; sl = "GB"; break;
    case LANG_XHOSA: l = "xh"; sl = "ZA"; break;
    case LANG_YI: l = "sit"; sl = "CN"; break;
    case LANG_YIDDISH: l = "yi"; sl = "IL"; break;
    case LANG_YORUBA: l = "yo"; sl = "NG"; break;
    case LANG_ZULU: l = "zu"; sl = "ZA"; break;
    }
  strcpy (bfr, l);
  if (sl != NULL)
    {
      if (sl[0] != '@')
	strcat (bfr, "_");
      strcat (bfr, sl);
    }

  return strdup (bfr);
}

#endif /* WIN32 */

size_t 
ml_iconv (iconv_t cd,
          const char    **inbuf,
          size_t  *inbytes_left,
          char    **outbuf,
          size_t  *outbytes_left)
{
  /* iconv should not modify the input, according to specs, but it is not marked const */
  return iconv (cd, (char**)inbuf, inbytes_left, outbuf, outbytes_left);
}

#ifndef EILSEQ
#define EILSEQ ENOENT
#endif

char*
ml_convert_with_iconv (const char *str,
                       size_t     len,
                       iconv_t    cd,
                       size_t     *bytes_read, 
                       size_t     *bytes_written)
{
  char *dest;
  char *outp;
  const char *p;
  size_t inbytes_remaining;
  size_t outbytes_remaining;
  size_t outbuf_size;
  size_t err;
  int have_error = FALSE;

  if (len < 0)
    len = strlen (str);

  p = str;
  inbytes_remaining = len;
  outbuf_size = len + 1; /* + 1 for nul in case len == 1 */
  
  outbytes_remaining = outbuf_size - 1; /* -1 for nul */
  outp = dest = malloc (outbuf_size);

 again:
  
  err = ml_iconv (cd, (const char **)&p, &inbytes_remaining, &outp, &outbytes_remaining);

  if (err == (size_t) -1)
    {
      switch (errno)
	{
	case EINVAL:
	  /* Incomplete text, do not report an error */
	  break;
	case E2BIG:
	  {
	    size_t used = outp - dest;

	    outbuf_size *= 2;
	    dest = realloc (dest, outbuf_size);
		
	    outp = dest + used;
	    outbytes_remaining = outbuf_size - used - 1; /* -1 for nul */

	    goto again;
	  }
	case EILSEQ:
	  have_error = TRUE;
	  break;
	default:
	  have_error = TRUE;
	  break;
	}
    }

  *outp = '\0';
  
  if (bytes_read)
    *bytes_read = p - str;
  else
    {
      if ((p - str) != len) 
	{
          if (!have_error)
            {
              have_error = TRUE;
            }
	}
    }

  if (bytes_written)
    *bytes_written = outp - dest;	/* Doesn't include '\0' */

  if (have_error)
    {
      free (dest);
      return NULL;
    }
  else
    return dest;
}

char*
ml_convert (const char *str,
            size_t     len,  
            const char *to_codeset,
            const char *from_codeset,
            size_t     *bytes_read, 
	    size_t     *bytes_written)
{
  char *res;
  iconv_t cd;

  cd = iconv_open (to_codeset, from_codeset);

  if (cd == (iconv_t) -1)
    {
      if (bytes_read)
        *bytes_read = 0;
      
      if (bytes_written)
        *bytes_written = 0;
      
      return NULL;
    }

  res = ml_convert_with_iconv (str, len, cd,
			      bytes_read, bytes_written);
  
  iconv_close (cd);

  return res;
}

static value
ml_copy_string_len_and_free (char *str, size_t len)
{
  value v;

  if (!str)
    raise_error ();

  v = caml_alloc_initialized_string(len, str);

  free (str);
  return v;
}

value
ml_convert_string(value str, value to, value from)
{
  size_t bw = 0;
  char* c_res;

  c_res = ml_convert(String_val(str),string_length(str),
                    String_val(to),String_val(from),
                    NULL,&bw);

  return ml_copy_string_len_and_free (c_res, bw);
}

value
ml_utf8_validate(value s)
{
  return Val_bool(utf8_validate(SizedString_val(s),NULL));
}

value
ml_locale_charset(void)
{

  const char *str;

  setlocale(LC_CTYPE, "");
  #ifdef HAVE_LOCALE_CHARSET
    str = locale_charset ();
  #else
    str = nl_langinfo(CODESET);
  #endif

  if (!str)
    raise_error ();

  return (copy_string ((char*) str));
}

/* taken and modify from gtk+ gtkmain.c */

value
ml_get_default_language (void)
{
  char *lang;
  char *p;
  
#ifdef WIN32

  p = getenv ("LC_ALL");
  if (p != NULL)
    lang = strdup (p);
  else
    {
      p = getenv ("LANG");
      if (p != NULL)
	lang = strdup (p);
      else
	{
	  p = getenv ("LC_CTYPE");
	  if (p != NULL)
	    lang = strdup (p);
	  else
	    lang = ml_win32_get_default_language ();
	}
    }
#else
  lang = strdup (setlocale (LC_CTYPE, NULL));
#endif
  p = strchr (lang, '.');
  if (p)
    *p = '\0';
  p = strchr (lang, '@');
  if (p)
    *p = '\0';

  return ml_copy_string_len_and_free (lang, strlen (lang));
}
