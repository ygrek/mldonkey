/* Copyright 2004 b8_bavard, INRIA */
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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* Stub code to interface with Shell_NotifyIcon */

#include <windows.h>
#include <wingdi.h>

#include <gdk/gdk.h> 
#include <gdk/gdkwin32.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include <stdio.h>
#include <lablgtk2/wrappers.h>
#include <lablgtk2/ml_gdkpixbuf.h>
#include <lablgtk2/ml_gobject.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#define WM_TRAYMESSAGE           WM_USER /* User defined WM Message */
#define SYSTRAY_LBUTTONDBLCLK    0
#define SYSTRAY_LBUTTONCLK       1
#define SYSTRAY_RBUTTONCLK       2

static UINT timerID = 3;
static NOTIFYICONDATA nid;
static HWND systray_hwnd=0;
static HICON systray_hicon=0;

void call_caml_systray_clicked(int button_type)
{

  static value * closure_f = NULL;

  if (closure_f == NULL) {
    closure_f = caml_named_value("on_systray_clicked");
  }

  callback(*closure_f, Val_int(button_type));
}

LRESULT msg_handler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  static UINT taskbarRestartMsg; /* static here means value is kept across multiple calls to this func */

  switch(msg) {
    case WM_CREATE:
      taskbarRestartMsg = RegisterWindowMessage("TaskbarCreated");
      break;

    case WM_TIMER:
      KillTimer(hWnd, timerID);
      call_caml_systray_clicked(SYSTRAY_LBUTTONCLK);
      break;

    case WM_DESTROY:
      break;

    case WM_TRAYMESSAGE:
      switch (lParam) {
        case WM_RBUTTONUP:
          call_caml_systray_clicked(SYSTRAY_RBUTTONCLK);
          break;

        case WM_LBUTTONDBLCLK:
          KillTimer(hWnd, timerID);
          call_caml_systray_clicked(SYSTRAY_LBUTTONDBLCLK);
          break;

        case WM_LBUTTONDOWN:
          SetTimer(hWnd, timerID, GetDoubleClickTime(), NULL);
          break;
      }

    default: 
      if (msg == taskbarRestartMsg) {
        /* explorer crashed and left us hanging... 
           This will put the systray icon back in it's place, when it restarts */
        Shell_NotifyIcon(NIM_ADD,&nid);
      }
      break;

  }/* end switch */

  return DefWindowProc(hWnd, msg, wParam, lParam);

}

/* Create hidden window to process systray messages */

static HWND systray_create_hiddenwin(void)
{

  WNDCLASSEX wcex;
  TCHAR wname[32];

  strcpy(wname, "Win");

  wcex.cbSize = sizeof(WNDCLASSEX);

  wcex.style = 0;
  wcex.lpfnWndProc = (WNDPROC)msg_handler;
  wcex.cbClsExtra = 0;
  wcex.cbWndExtra = 0;
  wcex.hInstance = GetModuleHandle(NULL);
  wcex.hIcon = NULL;
  wcex.hCursor = NULL,
  wcex.hbrBackground = NULL;
  wcex.lpszMenuName = NULL;
  wcex.lpszClassName = wname;
  wcex.hIconSm = NULL;

  RegisterClassEx(&wcex);

  /* Create the window */
  return (CreateWindow(wname, "", 0, 0, 0, 0, 0, GetDesktopWindow(), NULL, GetModuleHandle(NULL), 0));
}

/* taken and modified from gdk/win32/gdkwindow-win32.c */

HICON pixmap_to_hicon (GdkPixmap *pixmap, GdkBitmap *mask)
{
  ICONINFO ii;
  HICON hIcon;
  gint w = 0, h = 0;
  HANDLE hpixmap = NULL;
  HBITMAP hbitmap = NULL;

  /* Create Drawing Context */
  HDC hdc1 = NULL;
  HBITMAP hbitmaptmp1 = NULL;

    if (!pixmap) {
      return NULL;
    }

    gdk_drawable_get_size (GDK_DRAWABLE (pixmap), &w, &h);

    hpixmap = gdk_win32_drawable_get_handle (pixmap); 

    /* we need the inverted mask for the XOR op */
    hdc1 = CreateCompatibleDC (NULL);
    hbitmap = CreateCompatibleBitmap (hdc1, w, h);
    hbitmaptmp1 = SelectObject (hdc1, hbitmap);

    if (mask) {
      HANDLE hmask = NULL;
      HDC hdc2 = NULL;
      HBITMAP hbitmaptmp2 = NULL;

      hmask = gdk_win32_drawable_get_handle (mask); 

      hdc2 = CreateCompatibleDC (NULL);
      hbitmaptmp2 = SelectObject (hdc2, hmask);

      BitBlt (hdc1, 0,0,w,h, hdc2, 0,0, NOTSRCCOPY);    

      SelectObject (hdc2, hbitmaptmp2);
      DeleteDC (hdc2);
    } else {
      RECT rect;
      GetClipBox (hdc1, &rect);
      FillRect (hdc1, &rect, GetStockObject (WHITE_BRUSH));
    }

    SelectObject (hdc1, hbitmaptmp1); 
    DeleteDC (hdc1); 
 
    ii.fIcon = TRUE;
    ii.xHotspot = ii.yHotspot = 0; /* ignored for icons */
    ii.hbmMask = hbitmap;
    ii.hbmColor = hpixmap; 

    hIcon = CreateIconIndirect (&ii);
    if (!hIcon) {
      return NULL;
    }

    DeleteObject (hbitmap);

    return hIcon;
}

NOTIFYICONDATA systray_init(GdkPixmap *pm, GdkBitmap *mask, char* text) {

  char* locenc=NULL;

  ZeroMemory(&nid,sizeof(nid));
  systray_hicon = pixmap_to_hicon (pm, mask);
  systray_hwnd = systray_create_hiddenwin();

  nid.cbSize=sizeof(NOTIFYICONDATA);
  nid.hWnd=systray_hwnd;
  nid.uID=0;
  nid.uFlags=NIF_ICON | NIF_MESSAGE | NIF_TIP ;
  nid.uCallbackMessage=WM_TRAYMESSAGE;
  nid.hIcon=systray_hicon;

  if(text) {
    locenc = g_locale_from_utf8(text, -1, NULL, NULL, NULL);
    lstrcpy(nid.szTip, locenc);
    g_free(locenc);
  }

  Shell_NotifyIcon(NIM_ADD,&nid);
  return nid;
}

static void systray_modify_icon (GdkPixmap *pm, GdkBitmap *mask) {

  systray_hicon = pixmap_to_hicon (pm, mask);
  nid.hIcon = systray_hicon;
  Shell_NotifyIcon(NIM_MODIFY,&nid);
}

static void systray_modify_tooltip (char* text) {

  char *locenc=NULL;

  if(text) {
    locenc = g_locale_from_utf8(text, -1, NULL, NULL, NULL);
    lstrcpy(nid.szTip, locenc);
    g_free(locenc);
  }
  Shell_NotifyIcon(NIM_MODIFY,&nid);
}

static void systray_remove_nid(void) {

  DestroyWindow(nid.hWnd);
  Shell_NotifyIcon(NIM_DELETE,&nid);
}

void caml_systray_modify_icon (value pixbuf)
{
  CAMLparam1 (pixbuf);
  GdkPixmap *pm;
  GdkBitmap *mask;

  gdk_pixbuf_render_pixmap_and_mask(GdkPixbuf_val(pixbuf), &pm, &mask, 128);
  systray_modify_icon (pm, mask);
  CAMLreturn0;
}

void caml_systray_modify_tooltip (value tooltip)
{
  CAMLparam1 (tooltip);

  systray_modify_tooltip (String_val(tooltip));
  CAMLreturn0;
}

void caml_systray_create (value pixbuf, value tooltip)
{
  CAMLparam2 (pixbuf, tooltip);
  GdkPixmap *pm;
  GdkBitmap *mask;

  gdk_pixbuf_render_pixmap_and_mask(GdkPixbuf_val(pixbuf), &pm, &mask, 128);

  /* dummy window to process systray messages */

  systray_init (pm, mask, String_val(tooltip));

  CAMLreturn0;
}

void caml_systray_destroy (void)
{

  systray_remove_nid();
  return;
}
