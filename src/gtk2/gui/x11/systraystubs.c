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

/* Stub code to interface with x11 embedded */

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkimage.h>
#include <gtk/gtktooltips.h>

#include <stdio.h>
#include <lablgtk2/wrappers.h>
#include <lablgtk2/ml_gdkpixbuf.h>
#include <lablgtk2/ml_gobject.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

#include "eggtrayicon.h"

#define SYSTRAY_LBUTTONDBLCLK    0
#define SYSTRAY_LBUTTONCLK       1
#define SYSTRAY_RBUTTONCLK       2

static GtkWidget   *systray_icon = NULL;
static GtkWidget   *evbox = NULL;
static GtkTooltips *systray_icon_tooltips = NULL;

void call_caml_systray_clicked(int button_type)
{

  static value * closure_f = NULL;

  if (closure_f == NULL) {
    closure_f = caml_named_value("on_systray_clicked");
  }

  callback(*closure_f, Val_int(button_type));
}


static void msg_handler(GtkWidget *tray, GdkEventButton *ev)
{

  if (ev->type == GDK_BUTTON_PRESS) {

    switch (ev->button)	{
      case 3:
	call_caml_systray_clicked(SYSTRAY_RBUTTONCLK);
	return;

      case 1:
	call_caml_systray_clicked(SYSTRAY_LBUTTONCLK);
	return;

      case 2:
	call_caml_systray_clicked(SYSTRAY_LBUTTONDBLCLK);
	return;

      default : break;

    }

  }
}


void caml_systray_modify_icon (value pixbuf)
{
  CAMLparam1 (pixbuf);
  GtkWidget   *img, *old_img;

  img = gtk_image_new_from_pixbuf (GdkPixbuf_val(pixbuf));
  old_img = gtk_bin_get_child(GTK_BIN(evbox));
  gtk_widget_destroy(old_img);
  gtk_container_add (GTK_CONTAINER (evbox), img);
  CAMLreturn0;
}

void caml_systray_modify_tooltip (value tooltip)
{
  CAMLparam1 (tooltip);

  gtk_tooltips_set_tip (systray_icon_tooltips, systray_icon, String_val(tooltip), "");
  CAMLreturn0;
}

void caml_systray_create (value pixbuf, value tooltip)
{
  CAMLparam2 (pixbuf, tooltip);
  GtkWidget   *img;

  systray_icon = GTK_WIDGET (egg_tray_icon_new ("MLDonkey"));
  img = gtk_image_new_from_pixbuf (GdkPixbuf_val(pixbuf));
  evbox = gtk_event_box_new ();
  gtk_container_add (GTK_CONTAINER (systray_icon), evbox);
  gtk_container_add (GTK_CONTAINER (evbox), img);
  gtk_widget_show_all (systray_icon);
  gtk_widget_realize (systray_icon);
  gtk_widget_show (systray_icon);

  systray_icon_tooltips = gtk_tooltips_new ();
  gtk_tooltips_enable (systray_icon_tooltips);

  gtk_tooltips_set_tip (systray_icon_tooltips, systray_icon, String_val(tooltip), "");

  g_signal_connect (G_OBJECT (evbox), "button_press_event", G_CALLBACK (msg_handler), NULL);

  CAMLreturn0;
}

void caml_systray_destroy (void)
{

  gtk_widget_destroy(systray_icon);
  return;
}
