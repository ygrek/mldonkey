

# Find installation directories 
GCONFIG=gnome-config
PREFIX=`$(GCONFIG) --prefix gnome`
BINDIR=`$(GCONFIG) --bindir gnome`
DATADIR=`$(GCONFIG) --datadir gnome`
GNORBA_DIR = `$(GCONFIG) --sysconfdir gnorba`/CORBA/servers


TARGET=mldonkey_applet

BINARY = $(TARGET)
DESKTOP_FILE = $(TARGET).desktop
DESKTOP_DIR = Network
GNORBA_FILE = $(TARGET).gnorba

GNOME_INCLUDE = `$(GCONFIG) --cflags applets`
GNOME_LIB = `$(GCONFIG) --libs applets`
CFLAGS = $(GNOME_INCLUDE)  -I.
LDFLAGS = $(GNOME_LIB)
CC = gcc $(CFLAGS)
CCPLUS = c++ 

OBJS = $(TARGET).o ../api/gui_protocol.o ../api/endianess.o

$(BINARY): $(OBJS)
	$(CC) $(OBJS) -o $(BINARY) $(LDFLAGS)

$(TARGET).o : $(TARGET).c
	$(CC) -c $(TARGET).c

install:
	install -c $(BINARY) $(BINDIR)/$(BINARY)
	install -c $(DESKTOP_FILE) $(DATADIR)/applets/$(DESKTOP_DIR)/$(DESKTOP_FILE)
	install -c $(GNORBA_FILE) $(GNORBA_DIR)

uninstall:
	rm -f $(BINDIR)/$(BINARY)
	rm -f $(DATADIR)/applets/$(DESKTOP_DIR)/$(DESKTOP_FILE)
	rm -f $(GNORBA_DIR)/$(GNORBA_FILE)
	rm -f $(PREFIX)/etc/CORBA/servers/$(GNORBA_FILE) 	

clean:
	rm -f *.o *~ $(BINARY) core


