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


EXT=$(EXT_$(TARGET))
LIBEXT=$(LIBEXT_$(TARGET))
EXE=$(EXE_$(TARGET))
CDK=$(CDK_$(TARGET))
CUSTOM=$(CUSTOM_$(TARGET))
COMP=$(COMP_$(TARGET))
GTK_LIBS=$(GTK_LIBS_$(TARGET))

SUBDIRS=cdk configwin mp3tagui lib net proto client gui $(MORE_SUBDIRS)

INC_PACKAGES=lablgtk
INCLUDES +=-I +lablgtk $(foreach file, $(SUBDIRS), -I $(file))

LIBS=$(CUSTOM) unix.$(LIBEXT)

CFLAGS=-g $(CONFIG_INCLUDES)

CDK_CMOS=cdk/printexc.$(EXT) cdk/genlex2.$(EXT) cdk/sysenv.$(EXT) \
  cdk/netbase.$(EXT) cdk/filepath.$(EXT) cdk/string2.$(EXT) \
  cdk/filename2.$(EXT) cdk/list2.$(EXT) cdk/hashtbl2.$(EXT) \
  cdk/file.$(EXT) cdk/unix2.$(EXT)

CONFIGWIN_CMOS=configwin/configwin_types.$(EXT) \
  configwin/configwin_messages.$(EXT) \
  configwin/configwin_ihm.$(EXT) configwin/configwin.$(EXT)

MP3TAG_CMOS=     mp3tagui/mp3_info.$(EXT)  mp3tagui/mp3_genres.$(EXT) \
  mp3tagui/mp3_misc.$(EXT)\
  mp3tagui/mp3_tag.$(EXT) mp3tagui/mp3tag.$(EXT)

MP3TAGUI_CMOS=  mp3tagui/mp3_messages.$(EXT) mp3tagui/mp3_ui.$(EXT)

LIB_CMOS= \
  lib/int32ops.$(EXT) lib/ip.$(EXT) lib/options.$(EXT) lib/numset.$(EXT)  \
  lib/fifo.$(EXT) \
  lib/hole_tab.$(EXT) lib/indexer.$(EXT) lib/host.$(EXT)  \
  lib/misc.$(EXT) lib/md4.$(EXT) lib/unix32.$(EXT) \
  lib/avifile.$(EXT) lib/http_lexer.$(EXT) lib/url.$(EXT)

NET_CMOS = \
  net/basicSocket.$(EXT) \
  net/tcpClientSocket.$(EXT) net/tcpServerSocket.$(EXT) \
  net/udpSocket.$(EXT) net/http_server.$(EXT)

MIN_PROTO_CMOS= proto/mftp.$(EXT) proto/files.$(EXT) 

PROTO_CMOS= \
  secret/mftp_client.$(EXT) secret/mftp_server.$(EXT)  \
  secret/mftp_comm.$(EXT)  

OBJS=lib/md4_c.o lib/unix32_c.o lib/inet_c.o cdk/select_c.o

MIN_GUI_CMOS= gui/gui_types.$(EXT) gui/gui_proto.$(EXT)

GUI_CMOS= gui/gui_messages.$(EXT) gui/gui.$(EXT)

CLIENT_CMOS=client/downloadTypes.$(EXT) client/downloadGlobals.$(EXT) \
  client/downloadOptions.$(EXT) \
  client/downloadMultimedia.$(EXT) client/downloadIndexer.$(EXT) \
  client/downloadServers.$(EXT) client/downloadOneFile.$(EXT) \
  client/downloadClient.$(EXT) client/downloadFiles.$(EXT)  \
  client/downloadInteractive.$(EXT)  client/downloadInterface.$(EXT) \
  client/downloadMain.$(EXT)

TARGETS= mldonkey_gui$(EXE) $(MORE_TARGETS)$(EXE)

GUI= \
  $(CDK_CMOS) $(LIB_CMOS) $(NET_CMOS) \
  $(MP3TAG_CMOS)  $(CONFIGWIN_CMOS) $(MP3TAGUI_CMOS) \
  $(MIN_PROTO_CMOS) \
  $(MIN_GUI_CMOS) $(GUI_CMOS)

OPEN_CLIENT=  $(CDK_CMOS) $(LIB_CMOS) $(NET_CMOS) \
  $(MP3TAG_CMOS) \
  $(MIN_PROTO_CMOS)

SECRET_CLIENT= $(PROTO_CMOS) \
  $(MIN_GUI_CMOS) \
  $(CLIENT_CMOS) 

CLIENT= $(OPEN_CLIENT) $(SECRET_CLIENT)
BIN_CLIENT=client.$(EXT)

all: byte

.byte:
.opt:
.static:

lambda:  $(SECRET_CLIENT:.$(EXT)=.ml)
	ocaml ./secret/make_client.ml  $(SECRET_CLIENT:.$(EXT)=.ml)
	$(OCAMLOPT)  -I +lablgtk  -I cdk  -I configwin  -I mp3tagui  -I lib  -I net  -I proto  -I client  -I gui  -I secret -c -dol client.ml	

client.cmo: client.lam
	$(OCAMLC)  -I +lablgtk  -I cdk  -I configwin  -I mp3tagui  -I lib  -I net  -I proto  -I client  -I gui  -I secret -c -dil -impl client.lam

client.cmx: client.lam
	$(OCAMLOPT)  -I +lablgtk  -I cdk  -I configwin  -I mp3tagui  -I lib  -I net  -I proto  -I client  -I gui  -I secret -c -dil -impl client.lam

after_zoggy: gui/gui.zog
	camlp4 pa_o.cmo -I `cdk_config -ocamllib` pa_zog.cma pr_o.cmo -impl gui/gui.zog > gui/gui_zog.ml

gui/gui.ml: gui/gui_header.ml gui/gui_zog.ml gui/gui_trailer.ml
	cat gui/gui_header.ml > gui/gui.ml
	cat gui/gui_zog.ml >> gui/gui.ml
	cat gui/gui_trailer.ml >> gui/gui.ml

gui/md4_c.o: gui/md4_c.c
	ocamlc.opt -ccopt "-O6 -I /byterun -o gui/md4_c.o" -ccopt "" -c gui/md4_c.c


byte: $(TARGETS)
opt:
	$(MAKE) TARGET=opt

static:
	$(MAKE) TARGET=opt mldonkey_gui.static mldonkey.static

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

open_mldonkey:
	$(MAKE) TARGET=opt open_mldonkey.opt

open_mldonkey.opt: $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)
	$(COMP) -o mldonkey$(EXE) $(LIBS) $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)

open_mldonkey.byte:

open_mldonkey.static: $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)
	$(COMP) -ccopt -static -o mldonkey.static  $(LIBS) $(CMOS) $(OPEN_CLIENT) $(BIN_CLIENT) $(OBJS)


include Makefile.cdk

clean: 
	rm -f *.cm? donkey_* *.byte *.cmi $(TARGETS) *~ *.o core
	rm -f mldonkey mldonkey_gui
	(for i in $(SUBDIRS); do \
		rm -f  $$i/*.cm? $$i/*.o ; \
	done)

distclean: clean
	rm -f config/config.cache config/config.log config/config.status
	rm -f config/config.h config/Makefile.config
	rm -f gui/gui.ml gui/gui_zog.ml lib/http_lexer.ml

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
	mv $(DISDIR).tar mldonkey-`cat VERSION`.static.`uname -m`-`uname -s`.tar
	bzip2 mldonkey-`cat VERSION`.static.`uname -m`-`uname -s`.tar

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
	mv $(SHADIR).tar mldonkey-`cat VERSION`.shared.`uname -m`-`uname -s`.tar
	bzip2 mldonkey-`cat VERSION`.shared.`uname -m`-`uname -s`.tar

-include .depend
