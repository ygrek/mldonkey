#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>

#define PACKAGE "mldonkey_applet"
#define VERSION "1.0"

#include <gnome.h>
#include <applet-widget.h>
#include <gdk_imlib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "edonkey.xpm"

GtkWidget *applet;
static gint sizehint = 48;
static GtkWidget *icon;

static void ShowAbout(gpointer a_data)
{
  GtkWidget *about = NULL;
  static const gchar     *authors [] =
  {
    "MLDonkey Team <mldonkey-users@savannah.nongnu.org>",
    NULL
  };
  
  about = gnome_about_new ( _("MLDonkey Applet"), "1.0",
    _("(C) 2002, MLDonkey Team\n\n"),
    authors,
    
    _("MLDonkey Applet is a simple applet to display\n"
      "in the Gnome Panel the current state of the\n"
      "MLDonkey file-sharing daemon\n"),
    NULL);
  gtk_widget_show(about);
}


void AppletClicked(GtkWidget *sender, GdkEventButton *ev, gpointer data)
{
  if (!ev || ev->button != 1 || ev->type != GDK_BUTTON_PRESS)
    return;

  printf("AppletClicked\n");
}

void AppletQuit()
{
  exit(0);
}

static gboolean UpdateApplet()
{
  return TRUE;
}


#ifdef HAVE_PANEL_PIXEL_SIZE

static void AppletChangePixellSize(GtkWidget *w, int size, gpointer data)
{
	sizehint = size;
	gtk_widget_set_usize(icon, sizehint, sizehint);
	/* we need to wait for the mainloop to finish updating the
	   background pixel size, otherwise we'll end up getting the
	   wrong background size when loading and drawing the new
	   icon */
	gtk_timeout_add(100, UpdateApplet,			NULL);
}
#endif
#include "../api/gui_protocol.h"
static mldonkey_config config;
static mldonkey_info info;

gint update_mldonkey_status()
{
  int retcode = get_mldonkey_status(&config, &info);
  if(retcode == MLDONKEY_CONNECTED){
    printf("Connected!!!\n");
  } else {
    if(retcode == MLDONKEY_CONNECTING){
      printf("Not yet connected\n");
    } else {
      printf("Disconnected!!!\n");
    }
  }
}

void make_mldonkey_applet()
{
  GdkPixmap *pm;

  config.mldonkey_hostname = NULL;
  config.mldonkey_port = 4001;
  config.mldonkey_login = NULL;
  config.mldonkey_password = NULL;

  printf("mldonkey_applet version 1.00\n");

  applet = applet_widget_new("mldonkey_applet");
  gtk_widget_set_events(applet, 
    gtk_widget_get_events(applet) | GDK_BUTTON_PRESS_MASK);
  gtk_widget_realize(applet);
  
  pm = gdk_pixmap_create_from_xpm_d(applet->window, NULL, NULL, edonkey);

  printf("applet icon loaded\n");
  icon = gtk_pixmap_new(pm, NULL);

  #ifdef HAVE_PANEL_PIXEL_SIZE
  gtk_widget_set_usize(icon, 5, 5);
  #else
  gtk_widget_set_usize(icon, 48, 48);
  #endif

  gdk_pixmap_unref(pm);
  applet_widget_add(APPLET_WIDGET(applet), icon);

  
  applet_widget_register_stock_callback(APPLET_WIDGET(applet),
    "about",
    GNOME_STOCK_MENU_ABOUT,
    _("About..."), (AppletCallbackFunc)ShowAbout, NULL);
  
/*
  gtk_signal_connect(GTK_OBJECT(applet), "clicked", 
    GTK_SIGNAL_FUNC(AppletClicked),
    NULL);
*/
  
  gtk_signal_connect(GTK_OBJECT(applet), "destroy", 
    GTK_SIGNAL_FUNC(AppletQuit), NULL);
  
  #ifdef HAVE_PANEL_PIXEL_SIZE
  gtk_signal_connect(GTK_OBJECT(applet), "change_pixel_size",
    GTK_SIGNAL_FUNC(AppletChangePixellSize), NULL);
  #endif

  gtk_timeout_add(1000, update_mldonkey_status, NULL);
  update_mldonkey_status();
  
  applet_widget_send_draw(APPLET_WIDGET(applet), TRUE);
  gtk_signal_connect(GTK_OBJECT(applet), "do-draw", 
    GTK_SIGNAL_FUNC(UpdateApplet), NULL);
  gtk_widget_show(icon);
  gtk_widget_show(applet);
}

int main(int argc, char **argv)
{
    applet_widget_init("mldonkey_applet", "1.0", argc, argv, NULL, 0, NULL);
    make_mldonkey_applet();
    applet_widget_gtk_main();
}
