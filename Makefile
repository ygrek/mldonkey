include config/Makefile.config

OCAMLYACC=ocamlyacc
OCAMLDEP=ocamlrun ocamldep

TARGET=byte

EXT_byte=cmo
EXT_opt=cmx
LIBEXT_byte=cma
LIBEXT_opt=cmxa

CUSTOM_byte=-custom
CUSTOM_opt=

CDK_byte=
CDK_opt=-opt

COMP_byte=$(OCAMLC)
COMP_opt=$(OCAMLOPT)

EXE_byte=.byte
EXE_opt=

GTK_LIBS_byte=-I +lablgtk $(LABLGL_CMA) lablgtk.cma
GTK_LIBS_opt=-I +lablgtk  $(LABLGL_CMXA) lablgtk.cmxa
GTK_STATIC_LIBS_opt=-I +lablgtk lablgtk.cmxa
STR_LIBS_byte=str.cma
STR_LIBS_opt=str.cmxa

EXT=$(EXT_$(TARGET))
LIBEXT=$(LIBEXT_$(TARGET))
EXE=$(EXE_$(TARGET))
CDK=$(CDK_$(TARGET))
CUSTOM=$(CUSTOM_$(TARGET))
COMP=$(COMP_$(TARGET))
GTK_LIBS=$(GTK_LIBS_$(TARGET))
STR_LIBS=$(STR_LIBS_$(TARGET))

SUBDIRS=cdk configwin mp3tagui okey lib net proto \
  client server gui $(MORE_SUBDIRS)

INC_PACKAGES=lablgtk
INCLUDES +=-I +lablgtk $(foreach file, $(SUBDIRS), -I $(file))

LIBS=$(CUSTOM) unix.$(LIBEXT)

CFLAGS:=$(CFLAGS) $(CONFIG_INCLUDES)

CDK_CMOS=cdk/printexc.$(EXT) cdk/genlex2.$(EXT) cdk/sysenv.$(EXT) \
  cdk/netbase.$(EXT) cdk/filepath.$(EXT) cdk/string2.$(EXT) \
  cdk/filename2.$(EXT) cdk/list2.$(EXT) cdk/hashtbl2.$(EXT) \
  cdk/file.$(EXT) cdk/unix2.$(EXT) cdk/heap.$(EXT)

CONFIGWIN_CMOS=configwin/configwin_types.$(EXT) \
  configwin/configwin_messages.$(EXT) \
  configwin/configwin_ihm.$(EXT) configwin/configwin.$(EXT)

MP3TAG_CMOS=     mp3tagui/mp3_info.$(EXT)  mp3tagui/mp3_genres.$(EXT) \
  mp3tagui/mp3_misc.$(EXT)\
  mp3tagui/mp3_tag.$(EXT) mp3tagui/mp3tag.$(EXT)

MP3TAGUI_CMOS=  mp3tagui/mp3_messages.$(EXT) mp3tagui/mp3_ui.$(EXT)

OKEY_CMOS= okey/okey.$(EXT)

LIB_CMOS= lib/autoconf.$(EXT) \
  lib/int32ops.$(EXT) lib/options.$(EXT) lib/ip.$(EXT)  lib/numset.$(EXT)  \
  lib/fifo.$(EXT) lib/intmap.$(EXT) \
  lib/hole_tab.$(EXT) lib/store.$(EXT) lib/indexer.$(EXT) lib/indexer1.$(EXT) lib/indexer2.$(EXT) lib/host.$(EXT)  \
  lib/misc.$(EXT) lib/unix32.$(EXT)  lib/md4.$(EXT) \
  lib/avifile.$(EXT) lib/http_lexer.$(EXT) lib/url.$(EXT) \
  lib/mailer.$(EXT) lib/date.$(EXT)

NET_CMOS = \
  net/basicSocket.$(EXT) net/tcpBufferedSocket.$(EXT) \
  net/tcpClientSocket.$(EXT) net/tcpServerSocket.$(EXT) \
  net/udpSocket.$(EXT) net/http_server.$(EXT) net/http_client.$(EXT)

MIN_PROTO_CMOS= proto/mftp.$(EXT) proto/files.$(EXT) proto/openProtocol.$(EXT)

PROTO_CMOS= \
  secret/mftp_client.$(EXT) secret/mftp_server.$(EXT)  \
  secret/mftp_comm.$(EXT)  

OBJS=lib/md4_comp.o lib/md4_c.o lib/unix32_c.o lib/inet_c.o cdk/select_c.o cdk/heap_c.o

MIN_GUI_CMOS= gui/gui_types.$(EXT) gui/gui_proto.$(EXT)

GUI_CMOS= gui/gui_messages.$(EXT) gui/gui_keys.$(EXT) \
  gui/gui_options.$(EXT) gui/gui.$(EXT) \
  gui/myCList.$(EXT) gui/gui_handler.$(EXT) \
  gui/gui_misc.$(EXT) gui/gui_config.$(EXT) \
  gui/gui_main.$(EXT)

CLIENT_CMOS=client/downloadTypes.$(EXT) \
  client/downloadOptions.$(EXT) \
  client/downloadGlobals.$(EXT) \
  client/downloadComplexOptions.$(EXT) \
  client/downloadMultimedia.$(EXT) client/downloadIndexer.$(EXT) \
  client/downloadShare.$(EXT) \
  client/downloadServers.$(EXT) client/downloadOneFile.$(EXT) \
  client/downloadClient.$(EXT) client/downloadFiles.$(EXT)  \
  client/downloadSearch.$(EXT) \
  client/downloadInteractive.$(EXT)  \
  client/downloadCommands.$(EXT)  \
  client/downloadControlers.$(EXT)  \
  client/downloadInterface.$(EXT) \
  client/downloadMain.$(EXT)

SERVER_CMOS=\
  server/serverTypes.$(EXT) \
  server/serverOptions.$(EXT) \
  server/serverGlobals.$(EXT) \
  server/serverLocate.$(EXT) \
  server/serverIndexer.$(EXT) \
  server/serverClients.$(EXT) \
  server/serverUdp.$(EXT)  \
  server/serverMain.$(EXT)

TARGETS= use_tags$(EXE) mldonkey_gui$(EXE) $(MORE_TARGETS)

GUI= \
  $(CDK_CMOS) $(LIB_CMOS) $(NET_CMOS) \
  $(MP3TAG_CMOS)  $(CONFIGWIN_CMOS) $(MP3TAGUI_CMOS) \
  $(MIN_PROTO_CMOS) $(OKEY_CMOS) \
  $(MIN_GUI_CMOS) $(GUI_CMOS)

OPEN_CLIENT=  $(CDK_CMOS) $(LIB_CMOS) $(NET_CMOS)  $(MP3TAG_CMOS) 

SECRET_CLIENT=  \
  $(MIN_PROTO_CMOS) \
  $(PROTO_CMOS) \
  $(MIN_GUI_CMOS) \
  $(CLIENT_CMOS) 

SECRET_SERVER =   \
  $(MIN_PROTO_CMOS) \
  $(PROTO_CMOS) \
  $(MIN_GUI_CMOS) \
  $(SERVER_CMOS) 

USE_TAGS = \
  $(CDK_CMOS) $(LIB_CMOS) \
  $(MP3TAG_CMOS) \
  lib/cddb_lexer.$(EXT) lib/cddb_file.$(EXT) \
  client/use_tags.$(EXT)

CLIENT= $(OPEN_CLIENT) $(SECRET_CLIENT)
SERVER= $(OPEN_CLIENT) $(SECRET_SERVER)
BIN_CLIENT=client.$(EXT)

all: byte

.byte:
.opt:
.static:

lambda:  $(SECRET_CLIENT:.$(EXT)=.ml)
	ocaml ./secret/make_client  $(SECRET_CLIENT:.$(EXT)=.ml)
	$(OCAMLOPT)  -I +lablgtk  -I cdk  -I configwin  -I mp3tagui -I okey -I lib  -I net  -I proto  -I client  -I gui  -I secret -c -dol client.ml	

client.cmo: client.lam
	$(OCAMLC)  -I +lablgtk  -I cdk  -I configwin  -I mp3tagui  -I okey -I lib  -I net  -I proto  -I client  -I gui  -I secret -c -dil -impl client.lam

client.cmx: client.lam
	$(OCAMLOPT)  -I +lablgtk  -I cdk  -I configwin  -I mp3tagui  -I okey -I lib  -I net  -I proto  -I client  -I gui  -I secret -c -dil -impl client.lam

gui/gui_zog.ml: gui/gui.zog
	camlp4 pa_o.cmo -I `cdk_config -ocamllib` pa_zog.cma pr_o.cmo -impl gui/gui.zog > gui/gui_zog.ml

gui/gui.ml: gui/gui_header.ml gui/gui_zog.ml gui/gui_trailer.ml
	cat gui/gui_header.ml > gui/gui.ml
	cat gui/gui_zog.ml >> gui/gui.ml
	cat gui/gui_trailer.ml >> gui/gui.ml


lib/md4_cc.o: lib/md4.c
	ocamlc.opt -ccopt "$(CFLAGS) -O6 -I /byterun -o lib/md4_cc.o" -ccopt "" -c lib/md4.c

lib/md4_as.o: lib/md4_$(MD4ARCH).s
	as -o lib/md4_as.o lib/md4_$(MD4ARCH).s

lib/md4_comp.o: lib/md4_$(MD4COMP).o
	cp -f lib/md4_$(MD4COMP).o lib/md4_comp.o

byte: $(TARGETS)
opt:
	$(MAKE) TARGET=opt

static:
	$(MAKE) TARGET=opt mldonkey_gui.static mldonkey.static

######## TAGS

use_tags$(EXE): $(USE_TAGS) $(OBJS)
	$(COMP) -o use_tags$(EXE) $(LIBS) $(STR_LIBS) $(USE_TAGS) $(OBJS)

######## GUI

mldonkey_gui$(EXE): $(GUI) $(OBJS)
	$(COMP) -o mldonkey_gui$(EXE) $(LIBS) $(GTK_LIBS) $(GUI) $(OBJS)

mldonkey_gui.static: $(CMOS)  $(GUI) $(OBJS)
	$(COMP) -ccopt -static -o mldonkey_gui.static $(LIBS) $(GTK_STATIC_LIBS_opt) $(GUI) $(OBJS)

######## CLIENT

mldonkey$(EXE): $(CMOS) $(CLIENT) $(OBJS)
	$(COMP) -o mldonkey$(EXE) $(LIBS) $(CMOS) $(CLIENT) $(OBJS)

mldonkey.static: $(CMOS) $(CLIENT) $(OBJS)
	$(COMP) -ccopt -static -o mldonkey.static  $(LIBS) $(CMOS) $(CLIENT) $(OBJS)

mldonkey_s$(EXE): $(CMOS) $(SERVER) $(OBJS)
	$(COMP) -o mldonkey_s$(EXE) $(LIBS) $(CMOS) $(SERVER) $(OBJS)

mldonkey_s.static: $(CMOS) $(SERVER) $(OBJS)
	$(COMP) -ccopt -static -o mldonkey_s.static  $(LIBS) $(CMOS) $(SERVER) $(OBJS)

open_mldonkey:
	$(MAKE) TARGET=opt open_mldonkey.opt

open_mldonkey.opt: $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)
	$(COMP) -o mldonkey$(EXE) $(LIBS) $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)

open_mldonkey.byte:

open_mldonkey.static: $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)
	$(COMP) -ccopt -static -o mldonkey.static  $(LIBS) $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)


include Makefile.cdk

clean: 
	rm -f *.cm? donkey_* *.byte *.cmi $(TARGETS) *~ *.o core *.static
	rm -f mldonkey mldonkey_gui
	(for i in $(SUBDIRS); do \
		rm -f  $$i/*.cm? $$i/*.o ; \
	done)

distclean: clean
	rm -f config/config.cache config/config.log config/config.status
	rm -f config/config.h config/Makefile.config
	rm -f lib/http_lexer.ml

maintainerclean: distclean
	rm -f gui/gui.ml gui/gui_zog.ml 

depend: lib/http_lexer.ml
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .depend
	(for i in $(SUBDIRS); do \
		$(OCAMLDEP) $(INCLUDES) $$i/*.ml $$i/*.mli  >> .depend; \
	done)

DISDIR=mldonkey-distrib
distrib/Readme.txt: gui/gui_messages.ml
	grep -A 1000 help_text gui/gui_messages.ml | grep -v '"' > distrib/Readme.txt

distrib: $(DISDIR)

$(DISDIR):  static distrib/Readme.txt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	rm -rf $(DISDIR)/CVS
	cp mldonkey.static $(DISDIR)/mldonkey
	strip  $(DISDIR)/mldonkey
	cp mldonkey_gui.static $(DISDIR)/mldonkey_gui
	strip $(DISDIR)/mldonkey_gui
	tar cf $(DISDIR).tar $(DISDIR)
	grep Release: gui/gui_messages.ml | awk -F : '{ print $$2 }' | awk '{ print $$1 }' > VERSION
	mv $(DISDIR).tar mldonkey-`cat VERSION`.static.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-`cat VERSION`.static.$(MD4ARCH)-`uname -s`.tar

macosx:  opt distrib/Readme.txt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	rm -rf $(DISDIR)/CVS
	cp mldonkey $(DISDIR)/mldonkey
	strip  $(DISDIR)/mldonkey
	cp mldonkey_gui $(DISDIR)/mldonkey_gui
	strip $(DISDIR)/mldonkey_gui
	tar cf $(DISDIR).tar $(DISDIR)
	grep Release: gui/gui_messages.ml | awk -F : '{ print $$2 }' | awk '{ print $$1 }' > VERSION
	mv $(DISDIR).tar mldonkey-`cat VERSION`.shared.ppc-MacOS-X.tar
	gzip mldonkey-`cat VERSION`.shared.ppc-MacOS-X.tar

SHADIR=mldonkey-shared

shared: $(SHADIR)

$(SHADIR):  static distrib/Readme.txt
	rm -rf mldonkey-*
	cp -R distrib $(SHADIR)
	rm -rf $(SHADIR)/CVS
	cp mldonkey.static $(SHADIR)/mldonkey
	strip  $(SHADIR)/mldonkey
	cp mldonkey_gui $(SHADIR)/mldonkey_gui
	strip $(SHADIR)/mldonkey_gui
	tar cf $(SHADIR).tar $(SHADIR)
	grep Release: gui/gui_messages.ml | awk -F : '{ print $$2 }' | awk '{ print $$1 }' > VERSION
	mv $(SHADIR).tar mldonkey-`cat VERSION`.shared.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-`cat VERSION`.shared.$(MD4ARCH)-`uname -s`.tar

VERSION: gui/gui_messages.ml
	grep Release: gui/gui_messages.ml | awk -F : '{ print $$2 }' | awk '{ print $$1 }' > VERSION	

auto-release: VERSION
# i386
	mkdir -p $(HOME)/release-`cat VERSION`
	cp -f config/Makefile.config.i386 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-`cat VERSION`.static.i386-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/
	$(MAKE) shared
	cp mldonkey-`cat VERSION`.shared.i386-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/
# i686
	mkdir -p $(HOME)/release-`cat VERSION`
	cp -f config/Makefile.config.i686 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-`cat VERSION`.static.i686-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/
	$(MAKE) shared
	cp mldonkey-`cat VERSION`.shared.i686-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/
# i586
	mkdir -p $(HOME)/release-`cat VERSION`
	cp -f config/Makefile.config.i586 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-`cat VERSION`.static.i586-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/
	$(MAKE) shared
	cp mldonkey-`cat VERSION`.shared.i586-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/
# i486
	mkdir -p $(HOME)/release-`cat VERSION`
	cp -f config/Makefile.config.i486 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-`cat VERSION`.static.i486-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/
	$(MAKE) shared
	cp mldonkey-`cat VERSION`.shared.i486-Linux.tar.bz2 $(HOME)/release-`cat VERSION`/

-include .depend

