
# 
#
#
#   Do not edit Makefile, edit config/Makefile.in instead
#
#
#
#
#



include config/Makefile.config

##################################################################

##             Bytecode or Native ?

##################################################################

NO_LIBS_byte=
NO_LIBS_opt=
NO_STATIC_LIBS_opt=


LIBS_byte=-custom unix.cma str.cma
LIBS_opt= unix.cmxa str.cmxa


#######################################################################

##              General options

#######################################################################


ifeq ("$(BUILD_NEWGUI)", "yes")
  ICONS_CHOICE=tux
  GUI=NEWGUI
  SRC_GUI=src/gtk/newgui
else
  SRC_GUI=src/gtk/gui
  ICONS_CHOICE=kde
  GUI=OLDGUI
endif


CDK=src/utils/cdk
LIB=src/utils/lib
NET=src/utils/net

CHAT=src/daemon/chat
COMMON=src/daemon/common
DRIVER=src/daemon/driver
MP3=src/utils/mp3tagui

GPATTERN=src/gtk/gpattern
OKEY=src/gtk/okey
CONFIGWIN=src/gtk/configwin
SRC_GUI2=src/gtk/gui2
SRC_PROGRESS=src/gtk/progress

SRC_AUDIOGALAXY=src/networks/audio_galaxy
SRC_DONKEY=src/networks/donkey
SRC_BITTORRENT=src/networks/bittorrent
SRC_CYMES=src/networks/cymes
SRC_OPENNAP=src/networks/opennap
SRC_GNUTELLA=src/networks/gnutella
SRC_GNUTELLA2=src/networks/gnutella2
SRC_OPENFT=src/networks/openFT
SRC_FASTTRACK=src/networks/fasttrack
SRC_SOULSEEK=src/networks/soulseek
SRC_DIRECTCONNECT=src/networks/direct_connect
SRC_FILETP=src/networks/fileTP

IM=src/im

SUBDIRS=$(CDK) $(CHAT) $(LIB) $(NET) tools \
   $(COMMON) $(DRIVER) $(MP3) src/config/$(OS_FILES) $(EXTRA_DIRS)

INCLUDES += $(foreach file, $(SUBDIRS), -I $(file))

CFLAGS:=$(CFLAGS) $(CONFIG_INCLUDES)

# use_tags$(EXE) 
TARGETS= mlnet$(EXE) 


#######################################################################

##              Objects files for "mldonkey"

#######################################################################


ifeq ("$(OS_FILES)", "mingw")
  LIBS_flags += -cclib  -lws2_32 -cclib resfile.o
#  LIBS_byte += -cclib -lws2_32
endif

ifeq ("$(ZLIB)" , "yes")
  LIBS_flags += -cclib -lz
#  LIBS_byte += -cclib -lz
  CDK_SRCS +=  $(CDK)/zlib.ml $(CDK)/zlibstubs.c
endif


ifeq ("$(ICONV)" , "yes")
  LIBS_flags += -cclib -liconv
#  LIBS_byte += -cclib -liconv
endif

CDK_SRCS += $(LIB)/autoconf.ml

CDK_SRCS+= $(LIB)/fifo.ml  $(CDK)/printf2.ml \
   $(CDK)/heap.ml $(CDK)/dprintf.ml \
  $(CDK)/printexc2.ml $(CDK)/genlex2.ml $(CDK)/sysenv.ml \
  $(CDK)/netbase.ml $(CDK)/filepath.ml $(CDK)/string2.ml \
  $(CDK)/filename2.ml $(CDK)/list2.ml $(CDK)/hashtbl2.ml \
  $(CDK)/file.ml $(CDK)/unix2.ml $(CDK)/weak2.ml \
  $(CDK)/heap_c.c $(CDK)/array2.ml $(CDK)/sort2.ml \
  $(CDK)/xmllex.mll $(CDK)/xmlyacc.mly $(CDK)/xml.ml

ifneq ("$(PTHREAD_CFLAGS)" , "")
  CFLAGS += $(PTHREAD_CFLAGS)
  LIBS_flags += -ccopt "$(PTHREAD_CFLAGS)"
#  LIBS_byte += -ccopt "$(PTHREAD_CFLAGS)"
endif

ifneq ("$(PTHREAD_LIBS)" , "")
  LIBS_flags += -cclib "$(PTHREAD_LIBS)"
#  LIBS_byte += -cclib "$(PTHREAD_LIBS)"
endif

MP3TAG_SRCS=     $(MP3)/mp3_info.ml  $(MP3)/mp3_genres.ml \
  $(MP3)/mp3_misc.ml\
  $(MP3)/mp3_tag.ml $(MP3)/mp3tag.ml


LIB_SRCS=   \
  src/config/$(OS_FILES)/mlUnix.ml \
  src/config/$(OS_FILES)/os_stubs_c.c \
  $(LIB)/intmap.ml $(LIB)/stringMap.ml \
  $(LIB)/int32ops.ml $(LIB)/options.ml4 $(LIB)/numset.ml  \
  $(LIB)/fifo2.ml $(LIB)/intset.ml \
  $(LIB)/hole_tab.ml $(LIB)/store.ml \
  $(LIB)/indexer.ml $(LIB)/indexer1.ml $(LIB)/indexer2.ml $(LIB)/host.ml  \
  $(LIB)/misc.ml $(LIB)/unix32.ml  $(LIB)/md4.ml \
  $(LIB)/avifile.ml $(LIB)/http_lexer.mll $(LIB)/url.ml \
  $(LIB)/date.ml  $(LIB)/fst_hash.c \
  $(LIB)/md4_comp.c $(LIB)/md4_c.c \
  $(LIB)/gettext.ml4 $(LIB)/md5_c.c $(LIB)/$(SHA1_VERSION)_c.c \
  $(LIB)/tiger.c \
  $(LIB)/stubs_c.c  $(LIB)/queues.ml

NET_SRCS = \
  $(NET)/basicSocket.ml \
  $(NET)/ip.ml $(NET)/mailer.ml $(NET)/base64.ml  \
  $(NET)/anyEndian.ml $(NET)/bigEndian.ml $(NET)/littleEndian.ml \
  $(NET)/tcpBufferedSocket.ml \
  $(NET)/tcpServerSocket.ml \
  $(NET)/udpSocket.ml $(NET)/http_server.ml $(NET)/http_client.ml \
  $(NET)/multicast.ml $(NET)/multicast_c.c  \
  $(NET)/cobs.ml \
  $(NET)/terminal.ml

#  $(NET)/tcpClientSocket.ml 

CHAT_SRCS = $(CHAT)/chat_messages.ml\
	$(CHAT)/chat_misc.ml\
        $(CHAT)/chat_proto.ml\
        $(CHAT)/chat_types.ml\
        $(CHAT)/chat_options.ml\
        $(CHAT)/chat_config.ml\

COMMON_SRCS=$(COMMON)/commonTypes.ml \
  $(COMMON)/guiTypes.ml \
  $(COMMON)/guiProto.ml \
  $(COMMON)/commonEvent.ml \
  $(COMMON)/commonOptions.ml \
  $(COMMON)/commonMessages.ml \
  $(COMMON)/commonGlobals.ml \
  $(COMMON)/commonBitzi.ml \
  $(COMMON)/guiDecoding.ml \
  $(COMMON)/guiEncoding.ml \
  $(COMMON)/giftLexer.mll \
  $(COMMON)/giftParser.mly \
  $(COMMON)/giftEncoding.ml \
  $(COMMON)/giftDecoding.ml \
  $(COMMON)/commonChat.ml \
  $(COMMON)/commonHasher.ml \
  $(COMMON)/commonHosts.ml \
  $(COMMON)/commonHasher_c.c

COMMON_CLIENT_SRCS= \
  $(COMMON)/commonUser.ml \
  $(COMMON)/commonServer.ml \
  $(COMMON)/commonClient.ml \
  $(COMMON)/commonFile.ml \
  $(COMMON)/commonResult.ml \
  $(COMMON)/commonNetwork.ml \
  $(COMMON)/commonComplexOptions.ml \
  $(COMMON)/commonShared.ml \
  $(COMMON)/commonRoom.ml \
  $(COMMON)/commonSearch.ml \
  $(COMMON)/commonMultimedia.ml \
  $(COMMON)/commonWeb.ml \
  $(COMMON)/commonInteractive.ml \
  $(COMMON)/commonDownloads.ml \
  $(COMMON)/commonUploads.ml \
  $(COMMON)/commonSwarming.ml

all: Makefile config/Makefile.config $(TARGET_TYPE)

config/configure: config/configure.in
	cd config; autoconf

ifeq ("$(MYCONFIG_ARGS_DEFINED)" , "yes")

config/Makefile.config: config/configure config/Makefile.config.in $(LIB)/autoconf.ml.new.in packages/rpm/Makefile.in
	./configure $(MYCONFIG_ARGS)

else

config/Makefile.config: Makefile config/configure config/Makefile.config.in
	@echo '******************************************'
	@echo 
	@echo 
	@echo ' You should rerun ./configure now         '
	@echo 
	@echo 
	@echo '******************************************'
endif

Makefile: config/Makefile.in
	(cd config; m4 Makefile.in > ../Makefile)

#######################################################################

#              PLUGINS

#######################################################################

MAIN_SRCS=$(COMMON)/commonMain.ml

DONKEY_SRCS= \
  \
  $(SRC_DONKEY)/donkeyTypes.ml \
  $(SRC_DONKEY)/donkeyOptions.ml \
  $(SRC_DONKEY)/donkeyMftp.ml $(SRC_DONKEY)/donkeyImport.ml \
  $(SRC_DONKEY)/donkeyOpenProtocol.ml \
  $(SRC_DONKEY)/donkeyProtoClient.ml \
  $(SRC_DONKEY)/donkeyProtoServer.ml  \
  $(SRC_DONKEY)/donkeyProtoUdp.ml  \
  \
  $(SRC_DONKEY)/donkeyGlobals.ml \
  $(SRC_DONKEY)/donkeyProtoCom.ml  \
  $(SRC_DONKEY)/donkeySourcesMisc.ml \
  $(SRC_DONKEY)/donkeySources.ml  \
 \
  $(SRC_DONKEY)/donkeyComplexOptions.ml \
  $(SRC_DONKEY)/donkeySupernode.ml \
  $(SRC_DONKEY)/donkeyIndexer.ml \
  $(SRC_DONKEY)/donkeyShare.ml \
  $(SRC_DONKEY)/donkeyReliability.ml \
  $(SRC_DONKEY)/donkeyThieves.ml \
  $(SRC_DONKEY)/donkeyChunks.ml \
  $(SRC_DONKEY)/donkeyOneFile.ml \
  $(SRC_DONKEY)/donkeyStats.ml \
  $(SRC_DONKEY)/donkeyClient.ml \
  $(SRC_DONKEY)/donkeyProtoOvernet.ml \
  \
  $(SRC_DONKEY)/donkeyOvernet.ml \
  $(SRC_DONKEY)/donkeyUdp.ml \
  $(SRC_DONKEY)/donkeyFiles.ml  \
  $(SRC_DONKEY)/donkeyServers.ml \
  $(SRC_DONKEY)/donkeySearch.ml \
  $(SRC_DONKEY)/donkeyInteractive.ml \
  $(SRC_DONKEY)/donkeyMain.ml


OBSERVER_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(COMMON_CLIENT_SRCS) $(DONKEY_SRCS) \
  tools/observer.ml

ED2K_HASH_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  tools/ed2k_hash.ml

MLPIC_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  src/pic/picTypes.ml \
  src/pic/picOptions.ml \
  src/pic/picGlobals.ml \
  src/pic/picDB.ml \
  src/pic/picGenerator.ml \
  src/pic/picThemeFrames.ml \
  src/pic/picThemeFilm.ml \
  src/pic/picThemeEditor.ml \
  src/pic/picThemeAlbum.ml \
  src/pic/picHttp.ml \
  src/pic/picMain.ml

SPIDER_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  \
  $(COMMON)/commonTypes.ml \
  $(COMMON)/commonOptions.ml \
  $(COMMON)/commonMessages.ml \
  $(COMMON)/commonGlobals.ml \
  $(COMMON)/commonWeb.ml \
  $(COMMON)/commonHasher.ml \
  $(COMMON)/commonHasher_c.c \
  \
  src/spider/spiderTypes.ml \
  src/spider/spiderOptions.ml \
  src/spider/spiderMftp.ml \
  src/spider/spiderImport.ml \
  src/spider/spiderOpenProtocol.ml \
  src/spider/spiderProtoClient.ml \
  src/spider/spiderProtoServer.ml  \
  src/spider/spiderProtoUdp.ml  \
  \
  src/spider/spiderGlobals.ml \
  src/spider/spiderArgs.ml \
  src/spider/spiderProtoCom.ml  \
 \
  src/spider/spiderComplexOptions.ml \
  src/spider/spiderSupernode.ml \
  src/spider/spiderIndexer.ml \
  src/spider/spiderShare.ml \
  src/spider/spiderReliability.ml \
  src/spider/spiderChunks.ml \
  src/spider/spiderOneFile.ml \
  \
  src/spider/spiderUtils.ml \
  src/spider/spiderDependencies.ml \
  src/spider/spiderFiles.ml \
  src/spider/spiderGenTables.ml \
  src/spider/spiderTables.ml \
  src/spider/spiderCommands.ml \
  src/spider/spiderClustering.ml \
  src/spider/spiderCorruption.ml \
  src/spider/spiderRoutes.ml \
  src/spider/spiderStats.ml \
  \
  src/spider/spiderClient.ml \
  src/spider/spiderProtoOvernet.ml \
  \
  src/spider/spiderTrap.ml \
  src/spider/spiderOvernet.ml \
  src/spider/spiderServers.ml \
  src/spider/spiderSearch.ml \
  src/spider/spiderInteractive.ml \
  \
  src/spider/spiderMain.ml \
  src/daemon/common/commonMain.ml

COPYSOURCES_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) tools/copysources.ml

CYMES_SRCS=\
  $(SRC_CYMES)/serverTypes.ml \
  $(SRC_CYMES)/serverOptions.ml \
  $(SRC_CYMES)/serverGlobals.ml \
  $(SRC_CYMES)/serverMessages.ml \
  $(SRC_CYMES)/serverLocate.ml \
  $(SRC_CYMES)/serverIndexer.ml \
  $(SRC_CYMES)/serverLog.ml \
  $(SRC_CYMES)/serverSubscriptions.ml \
  $(SRC_CYMES)/serverServer.ml \
  $(SRC_CYMES)/serverClients.ml \
  $(SRC_CYMES)/serverUdp.ml  \
  $(SRC_CYMES)/serverMain.ml

OPENNAP_SRCS= \
 $(SRC_OPENNAP)/napigator.mll \
 $(SRC_OPENNAP)/winmx_xor_c.c \
 $(SRC_OPENNAP)/opennapTypes.ml \
 $(SRC_OPENNAP)/opennapProtocol.ml \
 $(SRC_OPENNAP)/opennapOptions.ml \
 $(SRC_OPENNAP)/opennapGlobals.ml \
 $(SRC_OPENNAP)/opennapComplexOptions.ml \
 $(SRC_OPENNAP)/opennapClients.ml \
 $(SRC_OPENNAP)/opennapServers.ml \
 $(SRC_OPENNAP)/opennapInteractive.ml \
 $(SRC_OPENNAP)/opennapMain.ml 

GNUTELLA_SRCS= \
  $(SRC_GNUTELLA)/gnutellaTypes.ml \
  $(SRC_GNUTELLA)/gnutellaOptions.ml \
  $(SRC_GNUTELLA)/gnutellaGlobals.ml \
  $(SRC_GNUTELLA)/gnutellaComplexOptions.ml \
  $(SRC_GNUTELLA)/gnutellaProtocol.ml \
  $(SRC_GNUTELLA)/gnutellaProto.ml \
  $(SRC_GNUTELLA)/gnutellaClients.ml \
  $(SRC_GNUTELLA)/gnutellaHandler.ml \
  $(SRC_GNUTELLA)/gnutellaRedirector.ml \
  $(SRC_GNUTELLA)/gnutella.ml \
  $(SRC_GNUTELLA)/gnutellaServers.ml \
  $(SRC_GNUTELLA)/gnutellaInteractive.ml \
  $(SRC_GNUTELLA)/gnutellaMain.ml

GNUTELLA2_SRCS= \
  $(SRC_GNUTELLA2)/g2Types.ml \
  $(SRC_GNUTELLA2)/g2Options.ml \
  $(SRC_GNUTELLA2)/g2Globals.ml \
  $(SRC_GNUTELLA2)/g2ComplexOptions.ml \
  $(SRC_GNUTELLA2)/g2Protocol.ml \
  $(SRC_GNUTELLA2)/g2Proto.ml \
  $(SRC_GNUTELLA2)/g2Clients.ml \
  $(SRC_GNUTELLA2)/g2Handler.ml \
  $(SRC_GNUTELLA2)/g2Redirector.ml \
  $(SRC_GNUTELLA2)/g2Scheduler.ml \
  $(SRC_GNUTELLA2)/g2Servers.ml \
  $(SRC_GNUTELLA2)/g2Interactive.ml \
  $(SRC_GNUTELLA2)/g2Main.ml

BITTORRENT_SRCS= \
  $(SRC_BITTORRENT)/bencode.ml \
  $(SRC_BITTORRENT)/bTRate.ml \
  $(SRC_BITTORRENT)/bTTypes.ml \
  $(SRC_BITTORRENT)/bTOptions.ml \
  $(SRC_BITTORRENT)/bTProtocol.ml \
  $(SRC_BITTORRENT)/bTGlobals.ml \
  $(SRC_BITTORRENT)/bTComplexOptions.ml \
  $(SRC_BITTORRENT)/bTTracker.ml \
  $(SRC_BITTORRENT)/bTChooser.ml \
  $(SRC_BITTORRENT)/bTClients.ml \
  $(SRC_BITTORRENT)/bTInteractive.ml \
  $(SRC_BITTORRENT)/bTMain.ml
  
OPENFT_SRCS= \
  $(SRC_OPENFT)/openFTTypes.ml \
  $(SRC_OPENFT)/openFTOptions.ml \
  $(SRC_OPENFT)/openFTGlobals.ml \
  $(SRC_OPENFT)/openFTComplexOptions.ml \
  $(SRC_OPENFT)/openFTProtocol.ml \
  $(SRC_OPENFT)/openFTClients.ml \
  $(SRC_OPENFT)/openFTServers.ml \
  $(SRC_OPENFT)/openFTInteractive.ml \
  $(SRC_OPENFT)/openFTMain.ml

FASTTRACK_SRCS= \
  $(SRC_FASTTRACK)/enc_type_1.c \
  $(SRC_FASTTRACK)/enc_type_2.c \
  $(SRC_FASTTRACK)/enc_type_20.c \
  $(SRC_FASTTRACK)/enc_type_80.c \
  $(SRC_FASTTRACK)/fst_crypt.c \
  $(SRC_FASTTRACK)/fst_crypt_ml.c \
  $(SRC_FASTTRACK)/fasttrackTypes.ml \
  $(SRC_FASTTRACK)/fasttrackOptions.ml \
  $(SRC_FASTTRACK)/fasttrackGlobals.ml \
  $(SRC_FASTTRACK)/fasttrackComplexOptions.ml \
  $(SRC_FASTTRACK)/fasttrackProtocol.ml \
  $(SRC_FASTTRACK)/fasttrackProto.ml \
  $(SRC_FASTTRACK)/fasttrackClients.ml \
  $(SRC_FASTTRACK)/fasttrackHandler.ml \
  $(SRC_FASTTRACK)/fasttrackServers.ml \
  $(SRC_FASTTRACK)/fasttrackPandora.ml \
  $(SRC_FASTTRACK)/fasttrackInteractive.ml \
  $(SRC_FASTTRACK)/fasttrackMain.ml

FILETP_SRCS= \
  $(SRC_FILETP)/fileTPTypes.ml \
  $(SRC_FILETP)/fileTPOptions.ml \
  $(SRC_FILETP)/fileTPGlobals.ml \
  $(SRC_FILETP)/fileTPComplexOptions.ml \
  $(SRC_FILETP)/fileTPProtocol.ml \
  $(SRC_FILETP)/fileTPClients.ml \
  $(SRC_FILETP)/fileTPHTTP.ml \
  $(SRC_FILETP)/fileTPFTP.ml \
  $(SRC_FILETP)/fileTPSSH.ml \
  $(SRC_FILETP)/fileTPInteractive.ml \
  $(SRC_FILETP)/fileTPMain.ml

SOULSEEK_SRCS= \
  $(SRC_SOULSEEK)/slskTypes.ml \
  $(SRC_SOULSEEK)/slskOptions.ml \
  $(SRC_SOULSEEK)/slskGlobals.ml \
  $(SRC_SOULSEEK)/slskComplexOptions.ml \
  $(SRC_SOULSEEK)/slskProtocol.ml \
  $(SRC_SOULSEEK)/slskClients.ml \
  $(SRC_SOULSEEK)/slskServers.ml \
  $(SRC_SOULSEEK)/slskInteractive.ml \
  $(SRC_SOULSEEK)/slskMain.ml

DIRECT_CONNECT_SRCS= \
  $(SRC_DIRECTCONNECT)/dcTypes.ml \
  $(SRC_DIRECTCONNECT)/dcOptions.ml \
  $(SRC_DIRECTCONNECT)/che3_c.c \
  $(SRC_DIRECTCONNECT)/che3.ml \
  $(SRC_DIRECTCONNECT)/dcGlobals.ml \
  $(SRC_DIRECTCONNECT)/dcComplexOptions.ml \
  $(SRC_DIRECTCONNECT)/dcKey.ml \
  $(SRC_DIRECTCONNECT)/dcProtocol.ml \
  $(SRC_DIRECTCONNECT)/dcClients.ml \
  $(SRC_DIRECTCONNECT)/dcServers.ml \
  $(SRC_DIRECTCONNECT)/dcInteractive.ml \
  $(SRC_DIRECTCONNECT)/dcMain.ml

AUDIOGALAXY_SRCS= \
  $(SRC_AUDIOGALAXY)/agTypes.ml \
  $(SRC_AUDIOGALAXY)/agOptions.ml \
  $(SRC_AUDIOGALAXY)/agGlobals.ml \
  $(SRC_AUDIOGALAXY)/agComplexOptions.ml \
  $(SRC_AUDIOGALAXY)/agProtocol.ml \
  $(SRC_AUDIOGALAXY)/agClients.ml \
  $(SRC_AUDIOGALAXY)/agServers.ml \
  $(SRC_AUDIOGALAXY)/agInteractive.ml \
  $(SRC_AUDIOGALAXY)/agHttpForward.ml \
  $(SRC_AUDIOGALAXY)/agMain.ml


MAKE_TORRENT_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(COMMON_CLIENT_SRCS) $(BITTORRENT_SRCS) \
  tools/make_torrent.ml

GET_RANGE_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS) \
  $(CHAT_SRCS)   tools/get_range.ml

KDE_APPLET=yes

ifeq ("$(OPENFT)" , "yes")
SUBDIRS += $(SRC_OPENFT)

CORE_SRCS += $(OPENFT_SRCS)
endif

ifeq ("$(AUDIO_GALAXY)" , "yes")
SUBDIRS += $(SRC_AUDIOGALAXY)

CORE_SRCS += $(AUDIOGALAXY_SRCS)
endif

DRIVER_SRCS= \
  $(DRIVER)/driverInteractive.ml  \
  $(DRIVER)/driverCommands.ml  \
  $(DRIVER)/driverControlers.ml  \
  $(DRIVER)/driverInterface.ml \
  $(DRIVER)/driverMain.ml 

MLCAST_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  tools/mlcast.ml

ICONS_CMXA=icons.cmxa

CDK_CMXA=cdk.cmxa
MLNET_CMXA=cdk.cmxa common.cmxa client.cmxa core.cmxa driver.cmxa
MLNET_SRCS= $(MAIN_SRCS)

mlnet+gui_CMXA=cdk.cmxa common.cmxa client.cmxa core.cmxa driver.cmxa \
  gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlnet+gui_SRCS=$(MAIN_SRCS)



#######################################################################

#              Objects files for "mlgui"

#######################################################################

uninstall::
	rm -f $(BINDIR)/mlnet
	rm -f $(BINDIR)/mlgui

install:: opt 
	mkdir -p $(prefix)/bin
	if test -e mlnet; then \
             rm -f $(prefix)/bin/mlnet; cp -f mlnet $(prefix)/bin/mlnet; \
             for link in mlslsk mldonkey mlgnut mldc mlbt; do \
               rm -f $(prefix)/bin/$$link; ln -s mlnet $(prefix)/bin/$$link; \
             done; \
         fi
	if test -e mlgui; then \
             rm -f $(prefix)/bin/mlgui; cp -f mlgui $(prefix)/bin/mlgui; \
             rm -f $(prefix)/bin/mldonkey_gui; ln -s mlgui $(prefix)/bin/mldonkey_gui; \
         fi
	if test -e mlnet+gui; then \
             rm -f $(prefix)/bin/mlnet+gui; cp -f mlnet+gui $(prefix)/bin/mlnet+gui; \
             for link in mlslsk+gui mldonkey+gui mlgnut+gui mldc+gui mlbt+gui; do \
               rm -f $(prefix)/bin/$$link; ln -s mlnet+gui $(prefix)/bin/$$link; \
             done; \
         fi
	if test -e mlim; then \
             rm -f $(prefix)/bin/mlim; cp -f mlim $(prefix)/bin/mlim; \
         fi


ifeq ("$(COMPILE_GUI)" , "yes")

SUBDIRS += $(SRC_GUI) $(SRC_GUI2) $(CONFIGWIN) $(OKEY) $(GPATTERN) icons/$(ICONS_CHOICE) +lablgtk $(SRC_PROGRESS)

GTK_LIBS_byte=-I +lablgtk $(LABLGL_CMA) lablgtk.cma
GTK_LIBS_opt=-I +lablgtk  $(LABLGL_CMXA) lablgtk.cmxa
GTK_STATIC_LIBS_opt=-I +lablgtk lablgtk.cmxa


CONFIGWIN_SRCS= \
  $(CONFIGWIN)/configwin_types.ml \
  $(CONFIGWIN)/configwin_messages.ml \
  $(CONFIGWIN)/configwin_ihm.ml \
  $(CONFIGWIN)/configwin.ml

MP3TAGUI_SRCS=  $(MP3)/mp3_messages.ml $(MP3)/mp3_ui.ml

GPATTERN_SRCS=  $(LIB)/gAutoconf.ml $(GPATTERN)/gpattern.ml

OKEY_SRCS= $(OKEY)/okey.ml

NEWGUI_ICONS= \
  icons/$(ICONS_CHOICE)/extend_search.xpm \
  icons/$(ICONS_CHOICE)/local_search.xpm \
  icons/$(ICONS_CHOICE)/trash.xpm \
  icons/$(ICONS_CHOICE)/subscribe_search.xpm \
  icons/$(ICONS_CHOICE)/submit_search.xpm \
  icons/$(ICONS_CHOICE)/close_search.xpm \
  icons/$(ICONS_CHOICE)/stop_search.xpm \
  icons/$(ICONS_CHOICE)/nbk_networks_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_networks_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_servers_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_servers_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_downloads_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_downloads_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_friends_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_friends_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_search_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_search_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_rooms_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_rooms_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_uploads_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_uploads_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_console_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_console_menu.xpm \
  icons/$(ICONS_CHOICE)/nbk_graphs_on.xpm \
  icons/$(ICONS_CHOICE)/nbk_graphs_menu.xpm \
  icons/$(ICONS_CHOICE)/about.xpm \
  icons/$(ICONS_CHOICE)/im.xpm \
  icons/$(ICONS_CHOICE)/settings.xpm \
  icons/$(ICONS_CHOICE)/exit.xpm \
  icons/$(ICONS_CHOICE)/gui.xpm \
  icons/$(ICONS_CHOICE)/kill_core.xpm \
  icons/$(ICONS_CHOICE)/splash_screen.xpm \
  icons/$(ICONS_CHOICE)/album_search.xpm \
  icons/$(ICONS_CHOICE)/movie_search.xpm \
  icons/$(ICONS_CHOICE)/mp3_search.xpm \
  icons/$(ICONS_CHOICE)/complex_search.xpm \
  icons/$(ICONS_CHOICE)/sharereactor_search.xpm \
  icons/$(ICONS_CHOICE)/jigle_search.xpm \
  icons/$(ICONS_CHOICE)/freedb_search.xpm \
  icons/$(ICONS_CHOICE)/imdb_search.xpm \
  icons/$(ICONS_CHOICE)/bt.xpm \
  icons/$(ICONS_CHOICE)/dc.xpm \
  icons/$(ICONS_CHOICE)/ed2k.xpm \
  icons/$(ICONS_CHOICE)/fasttrack.xpm \
  icons/$(ICONS_CHOICE)/gnutella.xpm \
  icons/$(ICONS_CHOICE)/napster.xpm \
  icons/$(ICONS_CHOICE)/slsk.xpm \
  icons/$(ICONS_CHOICE)/unknown.xpm \
  icons/$(ICONS_CHOICE)/downloading.xpm \
  icons/$(ICONS_CHOICE)/connect_y.xpm \
  icons/$(ICONS_CHOICE)/connect_m.xpm \
  icons/$(ICONS_CHOICE)/connect_n.xpm \
  icons/$(ICONS_CHOICE)/removedhost.xpm \
  icons/$(ICONS_CHOICE)/blacklistedhost.xpm \
  icons/$(ICONS_CHOICE)/files_listed.xpm \
  icons/$(ICONS_CHOICE)/server_c_high.xpm \
  icons/$(ICONS_CHOICE)/server_c_low.xpm \
  icons/$(ICONS_CHOICE)/server_ci.xpm \
  icons/$(ICONS_CHOICE)/server_nc.xpm \
  icons/$(ICONS_CHOICE)/toggle_display_all_servers.xpm \
  icons/$(ICONS_CHOICE)/view_pending_slots.xpm \
  icons/$(ICONS_CHOICE)/add_server.xpm \
  icons/$(ICONS_CHOICE)/add_shared_directory.xpm \
  icons/$(ICONS_CHOICE)/download_directory.xpm \
  icons/$(ICONS_CHOICE)/friend_user.xpm \
  icons/$(ICONS_CHOICE)/contact_user.xpm \
  icons/$(ICONS_CHOICE)/normal_user.xpm \
  icons/$(ICONS_CHOICE)/priority_0.xpm \
  icons/$(ICONS_CHOICE)/priority_1.xpm \
  icons/$(ICONS_CHOICE)/priority_2.xpm \
  icons/$(ICONS_CHOICE)/mimetype_binary.xpm \
  icons/$(ICONS_CHOICE)/mimetype_cdimage.xpm \
  icons/$(ICONS_CHOICE)/mimetype_debian.xpm \
  icons/$(ICONS_CHOICE)/mimetype_html.xpm \
  icons/$(ICONS_CHOICE)/mimetype_images.xpm \
  icons/$(ICONS_CHOICE)/mimetype_java.xpm \
  icons/$(ICONS_CHOICE)/mimetype_pdf.xpm \
  icons/$(ICONS_CHOICE)/mimetype_postscript.xpm \
  icons/$(ICONS_CHOICE)/mimetype_real.xpm \
  icons/$(ICONS_CHOICE)/mimetype_recycled.xpm \
  icons/$(ICONS_CHOICE)/mimetype_rpm.xpm \
  icons/$(ICONS_CHOICE)/mimetype_shellscript.xpm \
  icons/$(ICONS_CHOICE)/mimetype_soffice.xpm \
  icons/$(ICONS_CHOICE)/mimetype_sound.xpm \
  icons/$(ICONS_CHOICE)/mimetype_source.xpm \
  icons/$(ICONS_CHOICE)/mimetype_spreadsheet.xpm \
  icons/$(ICONS_CHOICE)/mimetype_tex.xpm \
  icons/$(ICONS_CHOICE)/mimetype_text.xpm \
  icons/$(ICONS_CHOICE)/mimetype_tgz.xpm \
  icons/$(ICONS_CHOICE)/mimetype_video.xpm \
  icons/$(ICONS_CHOICE)/mimetype_wordprocessing.xpm \
  icons/$(ICONS_CHOICE)/mimetype_unknown.xpm \
  icons/$(ICONS_CHOICE)/tree_closed.xpm \
  icons/$(ICONS_CHOICE)/tree_opened.xpm \
  icons/$(ICONS_CHOICE)/bt_net_on.xpm \
  icons/$(ICONS_CHOICE)/dc_net_on.xpm \
  icons/$(ICONS_CHOICE)/ed2k_net_on.xpm \
  icons/$(ICONS_CHOICE)/ftt_net_on.xpm \
  icons/$(ICONS_CHOICE)/gnut_net_on.xpm \
  icons/$(ICONS_CHOICE)/nap_net_on.xpm \
  icons/$(ICONS_CHOICE)/slsk_net_on.xpm \
  icons/$(ICONS_CHOICE)/mld_tux_on.xpm

NEWGUI_SMALL_ICONS= \
  icons/small/add_to_friends_small.xpm icons/small/cancel_small.xpm \
  icons/small/connect_more_small.xpm icons/small/connect_small.xpm \
  icons/small/disconnect_small.xpm icons/small/download_small.xpm \
  icons/small/edit_mp3_small.xpm icons/small/extend_search_small.xpm \
  icons/small/get_format_small.xpm icons/small/local_search_small.xpm \
  icons/small/preview_small.xpm icons/small/refres_small.xpm \
  icons/small/save_all_small.xpm icons/small/save_as_small.xpm icons/small/save_small.xpm \
  icons/small/trash_small.xpm icons/small/verify_chunks_small.xpm \
  icons/small/view_users_small.xpm

OLDGUI_ICONS= \
  icons/$(ICONS_CHOICE)/add_to_friends.xpm \
  icons/$(ICONS_CHOICE)/cancel.xpm icons/$(ICONS_CHOICE)/connect_more.xpm \
  icons/$(ICONS_CHOICE)/connect.xpm icons/$(ICONS_CHOICE)/disconnect.xpm \
  icons/$(ICONS_CHOICE)/download.xpm \
  icons/$(ICONS_CHOICE)/edit_mp3.xpm icons/$(ICONS_CHOICE)/extend_search.xpm \
  icons/$(ICONS_CHOICE)/get_format.xpm \
  icons/$(ICONS_CHOICE)/local_search.xpm icons/$(ICONS_CHOICE)/preview.xpm \
  icons/$(ICONS_CHOICE)/refres.xpm \
  icons/$(ICONS_CHOICE)/save_all.xpm icons/$(ICONS_CHOICE)/save_as.xpm \
  icons/$(ICONS_CHOICE)/save.xpm \
  icons/$(ICONS_CHOICE)/trash.xpm icons/$(ICONS_CHOICE)/verify_chunks.xpm \
  icons/$(ICONS_CHOICE)/view_users.xpm \
  icons/$(ICONS_CHOICE)/pause_resume.xpm \
  icons/$(ICONS_CHOICE)/remove_all_friends.xpm 

OLDGUI_SMALL_ICONS= \
  icons/small/add_to_friends_small.xpm icons/small/cancel_small.xpm \
  icons/small/connect_more_small.xpm icons/small/connect_small.xpm \
  icons/small/disconnect_small.xpm icons/small/download_small.xpm \
  icons/small/edit_mp3_small.xpm icons/small/extend_search_small.xpm \
  icons/small/get_format_small.xpm icons/small/local_search_small.xpm \
  icons/small/preview_small.xpm icons/small/refres_small.xpm \
  icons/small/save_all_small.xpm icons/small/save_as_small.xpm icons/small/save_small.xpm \
  icons/small/trash_small.xpm icons/small/verify_chunks_small.xpm \
  icons/small/view_users_small.xpm

ICONS= $($(GUI)_ICONS)
SMALL_ICONS= $($(GUI)_SMALL_ICONS)

ALL_ICONS=$(foreach file, $(ICONS),   $(basename $(file)).ml_icons)
ALL_ICONS_SRCS=$(foreach file, $(ICONS),   $(basename $(file))_xpm.ml)

$(ALL_ICONS_SRCS): $(ALL_ICONS)

GUI_BASE_SRCS= \
  $(SRC_GUI)/gui_messages.ml   $(SRC_GUI)/gui_global.ml \
  $(SRC_GUI)/gui_columns.ml \
  $(SRC_GUI)/gui_keys.ml \
  $(SRC_GUI)/gui_options.ml

NEWGUI_SRCS=  \
  $(SRC_PROGRESS)/gui_progress.ml \
  $(SRC_GUI)/gui_misc.ml \
  $(SRC_GUI)/gui_com.ml \
  $(SRC_GUI)/gui_types.ml \
  $(SRC_GUI)/gui_graph_base.ml $(SRC_GUI)/gui_graph.ml \
  $(SRC_GUI)/gui_console_base.zog $(SRC_GUI)/gui_console.ml \
  $(SRC_GUI)/gui_users_base.ml $(SRC_GUI)/gui_users.ml \
  $(SRC_GUI)/gui_results_base.ml $(SRC_GUI)/gui_results.ml \
  $(SRC_GUI)/gui_rooms_base.ml $(SRC_GUI)/gui_rooms.ml \
  $(SRC_GUI)/gui_friends_base.ml $(SRC_GUI)/gui_friends.ml \
  $(SRC_GUI)/gui_cdget_base.zog $(SRC_GUI)/gui_cdget.ml \
  $(SRC_GUI)/gui_queries_base.ml $(SRC_GUI)/gui_queries.ml \
  $(SRC_GUI)/gui_servers_base.ml $(SRC_GUI)/gui_servers.ml \
  $(SRC_GUI)/gui_uploads_base.ml $(SRC_GUI)/gui_uploads.ml \
  $(SRC_GUI)/gui_downloads_base.ml $(SRC_GUI)/gui_downloads.ml \
  $(SRC_GUI)/gui_networks.ml \
  $(SRC_GUI)/gui_window_base.ml $(SRC_GUI)/gui_window.ml \
  $(IM_GUI_CORE) \
  $(SRC_GUI)/gui_config.ml \
  $(SRC_GUI)/gui_main.ml

PROGRESS_SRCS = \
  $(SRC_PROGRESS)/gui_progress.ml \
  $(SRC_GUI)/gui_misc.ml \
  $(SRC_GUI)/gui_com.ml \
  $(SRC_PROGRESS)/gui_progress_main.ml

OLDGUI_SRCS=  \
  $(SRC_GUI)/gui_misc.ml \
  $(SRC_GUI)/gui_com.ml \
  $(SRC_GUI)/gui_help_base.zog $(SRC_GUI)/gui_help.ml \
  $(SRC_GUI)/gui_console_base.zog $(SRC_GUI)/gui_console.ml \
  $(SRC_GUI)/gui_uploads_base.zog $(SRC_GUI)/gui_uploads.ml \
  $(SRC_GUI)/gui_users_base.zog $(SRC_GUI)/gui_users.ml \
  $(SRC_GUI)/gui_results_base.zog $(SRC_GUI)/gui_results.ml \
  $(SRC_GUI)/gui_rooms_base.zog $(SRC_GUI)/gui_rooms.ml \
  $(SRC_GUI)/gui_friends_base.zog $(SRC_GUI)/gui_friends.ml \
  $(SRC_GUI)/gui_cdget_base.zog $(SRC_GUI)/gui_cdget.ml \
  $(SRC_GUI)/gui_queries_base.ml $(SRC_GUI)/gui_queries.ml \
  $(SRC_GUI)/gui_servers_base.zog $(SRC_GUI)/gui_servers.ml \
  $(SRC_GUI)/gui_downloads_base.zog $(SRC_GUI)/gui_downloads.ml \
  $(SRC_GUI)/gui_window_base.zog $(SRC_GUI)/gui_window.ml \
  $(IM_GUI_CORE) \
  $(SRC_GUI)/gui_config.ml \
  $(SRC_GUI)/gui_main.ml

GUI_SRCS= $($(GUI)_SRCS)

GUI2_SRCS= $(SRC_GUI2)/gui2_messages.ml $(SRC_GUI2)/gui2_keys.ml \
  $(SRC_GUI2)/gui2_options.ml $(SRC_GUI2)/gui2_GList.ml $(SRC_GUI2)/gui2.zog \
  $(SRC_GUI2)/myCList.ml $(SRC_GUI2)/gui2_handler.ml \
  $(SRC_GUI2)/gui2_misc.ml $(SRC_GUI2)/gui2_config.ml \
  $(SRC_GUI2)/gui2_main.ml

MLDONKEYGUI_CMXA= cdk.cmxa gmisc.cmxa common.cmxa icons.cmxa guibase.cmxa gui.cmxa
MLDONKEYGUI_SRCS= $(MAIN_SRCS)

MLDONKEYGUI2_CMXA= cdk.cmxa gmisc.cmxa common.cmxa icons.cmxa guibase.cmxa
MLDONKEYGUI2_SRCS= $(GUI2_SRCS) $(MAIN_SRCS)

MLDONKEY_IM_CMXA= cdk.cmxa gmisc.cmxa common.cmxa icons.cmxa guibase.cmxa
MLDONKEY_IM_SRCS= \
   $(IM_GUI_CORE) $(IM)/gui_im_main.ml  $(MAIN_SRCS)

STARTER_SRCS= $(SRC_GUI)/gui_starter.ml
STARTER_CMXA=cdk.cmxa

INSTALLER_CMXA= cdk.cmxa gmisc.cmxa common.cmxa
INSTALLER_SRCS= \
  $(SRC_GUI)/gui_installer_base.zog $(SRC_GUI)/gui_installer.ml

MLPROGRESS_CMXA= cdk.cmxa gmisc.cmxa common.cmxa icons.cmxa guibase.cmxa

MLPROGRESS_SRCS = \
  $(PROGRESS_SRCS) $(MAIN_SRCS)


#######################################################################

#              Objects files for "mlchat"

#######################################################################


CHAT_EXE_SRCS= \
        $(CHAT)/chat_data.ml\
        $(CHAT)/chat_icons.ml\
        $(CHAT)/chat_gui_base.ml\
        $(CHAT)/chat_gui.ml\
        $(CHAT)/chat_app.ml \
        $(CHAT)/mlchat.ml \
        $(CHAT)/chat_args.ml \
	$(CHAT)/chat_main.ml

MLCHAT_CMXA= cdk.cmxa gmisc.cmxa
MLCHAT_SRCS=  $(CHAT_SRCS) $(CHAT_EXE_SRCS)


TARGETS += mlgui$(EXE) mlchat$(EXE) mlguistarter$(EXE)
ifeq ("$(BUILD_NEWGUI)", "yes")
  TARGETS += mlprogress$(EXE)
endif

TARGETS +=  mlnet+gui$(EXE)

ifeq ("$(DEVEL)", "yes")

TARGETS += mldonkey_installer$(EXE)

endif


#### IM stuff is now automatically included in the GUI


SUBDIRS += $(IM) $(IM)/yahoo  $(IM)/irc
  
IM_CORE += $(IM)/imTypes.ml $(IM)/imEvent.ml \
   $(IM)/imProtocol.ml $(IM)/imIdentity.ml $(IM)/imAccount.ml \
   $(IM)/imChat.ml $(IM)/imRoom.ml \
   $(IM)/imOptions.ml

IM_CORE +=  $(IM)/irc/irc.ml

IM_GUI_CORE += $(IM)/gui_im_base.zog   \
    $(IM)/gui_im.ml

TARGETS += mlim

ifeq ("$(DEVEL)", "yes")
  SUBDIRS += $(IM)/msn

  IM_CORE +=    $(IM)/yahoo/yahoo.ml   $(IM)/msn/msn.ml
endif

IM_CORE +=   $(IM)/imMain.ml

endif

top: mldonkeytop
runtop: top
	./mldonkeytop $(INCLUDES)

TOP_CMXA=cdk.cmxa common.cmxa client.cmxa core.cmxa
TOP_SRCS= 








ifeq ("$(DIRECT_CONNECT)" , "yes")
SUBDIRS += src/networks/direct_connect

CORE_SRCS += $(DIRECT_CONNECT_SRCS)

## TARGETS += mldc$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mldc+gui$(EXE)

endif
endif


mldc_CMXA= cdk.cmxa common.cmxa client.cmxa mldc.cmxa driver.cmxa
mldc_SRCS= $(MAIN_SRCS)


DIRECT_CONNECT_ZOG := $(filter %.zog, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_MLL := $(filter %.mll, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_MLY := $(filter %.mly, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_ML4 := $(filter %.ml4, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_C := $(filter %.c, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_CMOS=$(foreach file, $(DIRECT_CONNECT_ML),   $(basename $(file)).cmo) 
DIRECT_CONNECT_CMXS=$(foreach file, $(DIRECT_CONNECT_ML),   $(basename $(file)).cmx) 
DIRECT_CONNECT_OBJS=$(foreach file, $(DIRECT_CONNECT_C),   $(basename $(file)).o)    

TMPSOURCES += $(DIRECT_CONNECT_ML4:.ml4=.ml) $(DIRECT_CONNECT_MLL:.mll=.ml) $(DIRECT_CONNECT_MLY:.mly=.ml) $(DIRECT_CONNECT_MLY:.mly=.mli) $(DIRECT_CONNECT_ZOG:.zog=.ml) 
 
build/mldc.cmxa: $(DIRECT_CONNECT_OBJS) $(DIRECT_CONNECT_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(DIRECT_CONNECT_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DIRECT_CONNECT_CMXS) 
 
build/mldc.cma: $(DIRECT_CONNECT_OBJS) $(DIRECT_CONNECT_CMOS) 
	$(OCAMLC) -a -o $@  $(DIRECT_CONNECT_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DIRECT_CONNECT_CMOS) 
 


mldc+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mldc.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mldc+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(OPENNAP)" , "yes")
SUBDIRS += src/networks/opennap

CORE_SRCS += $(OPENNAP_SRCS)

## TARGETS += mlnap$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlnap+gui$(EXE)

endif
endif


mlnap_CMXA= cdk.cmxa common.cmxa client.cmxa mlnap.cmxa driver.cmxa
mlnap_SRCS= $(MAIN_SRCS)


OPENNAP_ZOG := $(filter %.zog, $(OPENNAP_SRCS)) 
OPENNAP_MLL := $(filter %.mll, $(OPENNAP_SRCS)) 
OPENNAP_MLY := $(filter %.mly, $(OPENNAP_SRCS)) 
OPENNAP_ML4 := $(filter %.ml4, $(OPENNAP_SRCS)) 
OPENNAP_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(OPENNAP_SRCS)) 
OPENNAP_C := $(filter %.c, $(OPENNAP_SRCS)) 
OPENNAP_CMOS=$(foreach file, $(OPENNAP_ML),   $(basename $(file)).cmo) 
OPENNAP_CMXS=$(foreach file, $(OPENNAP_ML),   $(basename $(file)).cmx) 
OPENNAP_OBJS=$(foreach file, $(OPENNAP_C),   $(basename $(file)).o)    

TMPSOURCES += $(OPENNAP_ML4:.ml4=.ml) $(OPENNAP_MLL:.mll=.ml) $(OPENNAP_MLY:.mly=.ml) $(OPENNAP_MLY:.mly=.mli) $(OPENNAP_ZOG:.zog=.ml) 
 
build/mlnap.cmxa: $(OPENNAP_OBJS) $(OPENNAP_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(OPENNAP_OBJS) $(LIBS_flags) $(_LIBS_flags) $(OPENNAP_CMXS) 
 
build/mlnap.cma: $(OPENNAP_OBJS) $(OPENNAP_CMOS) 
	$(OCAMLC) -a -o $@  $(OPENNAP_OBJS) $(LIBS_flags) $(_LIBS_flags) $(OPENNAP_CMOS) 
 


mlnap+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlnap.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlnap+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(GNUTELLA)" , "yes")
SUBDIRS += src/networks/gnutella

CORE_SRCS += $(GNUTELLA_SRCS)

## TARGETS += mlgnut$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlgnut+gui$(EXE)

endif
endif


mlgnut_CMXA= cdk.cmxa common.cmxa client.cmxa mlgnut.cmxa driver.cmxa
mlgnut_SRCS= $(MAIN_SRCS)


GNUTELLA_ZOG := $(filter %.zog, $(GNUTELLA_SRCS)) 
GNUTELLA_MLL := $(filter %.mll, $(GNUTELLA_SRCS)) 
GNUTELLA_MLY := $(filter %.mly, $(GNUTELLA_SRCS)) 
GNUTELLA_ML4 := $(filter %.ml4, $(GNUTELLA_SRCS)) 
GNUTELLA_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(GNUTELLA_SRCS)) 
GNUTELLA_C := $(filter %.c, $(GNUTELLA_SRCS)) 
GNUTELLA_CMOS=$(foreach file, $(GNUTELLA_ML),   $(basename $(file)).cmo) 
GNUTELLA_CMXS=$(foreach file, $(GNUTELLA_ML),   $(basename $(file)).cmx) 
GNUTELLA_OBJS=$(foreach file, $(GNUTELLA_C),   $(basename $(file)).o)    

TMPSOURCES += $(GNUTELLA_ML4:.ml4=.ml) $(GNUTELLA_MLL:.mll=.ml) $(GNUTELLA_MLY:.mly=.ml) $(GNUTELLA_MLY:.mly=.mli) $(GNUTELLA_ZOG:.zog=.ml) 
 
build/mlgnut.cmxa: $(GNUTELLA_OBJS) $(GNUTELLA_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(GNUTELLA_OBJS) $(LIBS_flags) $(_LIBS_flags) $(GNUTELLA_CMXS) 
 
build/mlgnut.cma: $(GNUTELLA_OBJS) $(GNUTELLA_CMOS) 
	$(OCAMLC) -a -o $@  $(GNUTELLA_OBJS) $(LIBS_flags) $(_LIBS_flags) $(GNUTELLA_CMOS) 
 


mlgnut+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlgnut.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlgnut+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(GNUTELLA2)" , "yes")
SUBDIRS += src/networks/gnutella2

CORE_SRCS += $(GNUTELLA2_SRCS)

## TARGETS += mlg2$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlg2+gui$(EXE)

endif
endif


mlg2_CMXA= cdk.cmxa common.cmxa client.cmxa mlg2.cmxa driver.cmxa
mlg2_SRCS= $(MAIN_SRCS)


GNUTELLA2_ZOG := $(filter %.zog, $(GNUTELLA2_SRCS)) 
GNUTELLA2_MLL := $(filter %.mll, $(GNUTELLA2_SRCS)) 
GNUTELLA2_MLY := $(filter %.mly, $(GNUTELLA2_SRCS)) 
GNUTELLA2_ML4 := $(filter %.ml4, $(GNUTELLA2_SRCS)) 
GNUTELLA2_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(GNUTELLA2_SRCS)) 
GNUTELLA2_C := $(filter %.c, $(GNUTELLA2_SRCS)) 
GNUTELLA2_CMOS=$(foreach file, $(GNUTELLA2_ML),   $(basename $(file)).cmo) 
GNUTELLA2_CMXS=$(foreach file, $(GNUTELLA2_ML),   $(basename $(file)).cmx) 
GNUTELLA2_OBJS=$(foreach file, $(GNUTELLA2_C),   $(basename $(file)).o)    

TMPSOURCES += $(GNUTELLA2_ML4:.ml4=.ml) $(GNUTELLA2_MLL:.mll=.ml) $(GNUTELLA2_MLY:.mly=.ml) $(GNUTELLA2_MLY:.mly=.mli) $(GNUTELLA2_ZOG:.zog=.ml) 
 
build/mlg2.cmxa: $(GNUTELLA2_OBJS) $(GNUTELLA2_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(GNUTELLA2_OBJS) $(LIBS_flags) $(_LIBS_flags) $(GNUTELLA2_CMXS) 
 
build/mlg2.cma: $(GNUTELLA2_OBJS) $(GNUTELLA2_CMOS) 
	$(OCAMLC) -a -o $@  $(GNUTELLA2_OBJS) $(LIBS_flags) $(_LIBS_flags) $(GNUTELLA2_CMOS) 
 


mlg2+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlg2.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlg2+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(FASTTRACK)" , "yes")
SUBDIRS += src/networks/fasttrack

CORE_SRCS += $(FASTTRACK_SRCS)

## TARGETS += mlfasttrack$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlfasttrack+gui$(EXE)

endif
endif


mlfasttrack_CMXA= cdk.cmxa common.cmxa client.cmxa mlfasttrack.cmxa driver.cmxa
mlfasttrack_SRCS= $(MAIN_SRCS)


FASTTRACK_ZOG := $(filter %.zog, $(FASTTRACK_SRCS)) 
FASTTRACK_MLL := $(filter %.mll, $(FASTTRACK_SRCS)) 
FASTTRACK_MLY := $(filter %.mly, $(FASTTRACK_SRCS)) 
FASTTRACK_ML4 := $(filter %.ml4, $(FASTTRACK_SRCS)) 
FASTTRACK_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(FASTTRACK_SRCS)) 
FASTTRACK_C := $(filter %.c, $(FASTTRACK_SRCS)) 
FASTTRACK_CMOS=$(foreach file, $(FASTTRACK_ML),   $(basename $(file)).cmo) 
FASTTRACK_CMXS=$(foreach file, $(FASTTRACK_ML),   $(basename $(file)).cmx) 
FASTTRACK_OBJS=$(foreach file, $(FASTTRACK_C),   $(basename $(file)).o)    

TMPSOURCES += $(FASTTRACK_ML4:.ml4=.ml) $(FASTTRACK_MLL:.mll=.ml) $(FASTTRACK_MLY:.mly=.ml) $(FASTTRACK_MLY:.mly=.mli) $(FASTTRACK_ZOG:.zog=.ml) 
 
build/mlfasttrack.cmxa: $(FASTTRACK_OBJS) $(FASTTRACK_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(FASTTRACK_OBJS) $(LIBS_flags) $(_LIBS_flags) $(FASTTRACK_CMXS) 
 
build/mlfasttrack.cma: $(FASTTRACK_OBJS) $(FASTTRACK_CMOS) 
	$(OCAMLC) -a -o $@  $(FASTTRACK_OBJS) $(LIBS_flags) $(_LIBS_flags) $(FASTTRACK_CMOS) 
 


mlfasttrack+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlfasttrack.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlfasttrack+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(FILETP)" , "yes")
SUBDIRS += src/networks/fileTP

CORE_SRCS += $(FILETP_SRCS)

## TARGETS += mlfileTP$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlfileTP+gui$(EXE)

endif
endif


mlfileTP_CMXA= cdk.cmxa common.cmxa client.cmxa mlfileTP.cmxa driver.cmxa
mlfileTP_SRCS= $(MAIN_SRCS)


FILETP_ZOG := $(filter %.zog, $(FILETP_SRCS)) 
FILETP_MLL := $(filter %.mll, $(FILETP_SRCS)) 
FILETP_MLY := $(filter %.mly, $(FILETP_SRCS)) 
FILETP_ML4 := $(filter %.ml4, $(FILETP_SRCS)) 
FILETP_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(FILETP_SRCS)) 
FILETP_C := $(filter %.c, $(FILETP_SRCS)) 
FILETP_CMOS=$(foreach file, $(FILETP_ML),   $(basename $(file)).cmo) 
FILETP_CMXS=$(foreach file, $(FILETP_ML),   $(basename $(file)).cmx) 
FILETP_OBJS=$(foreach file, $(FILETP_C),   $(basename $(file)).o)    

TMPSOURCES += $(FILETP_ML4:.ml4=.ml) $(FILETP_MLL:.mll=.ml) $(FILETP_MLY:.mly=.ml) $(FILETP_MLY:.mly=.mli) $(FILETP_ZOG:.zog=.ml) 
 
build/mlfileTP.cmxa: $(FILETP_OBJS) $(FILETP_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(FILETP_OBJS) $(LIBS_flags) $(_LIBS_flags) $(FILETP_CMXS) 
 
build/mlfileTP.cma: $(FILETP_OBJS) $(FILETP_CMOS) 
	$(OCAMLC) -a -o $@  $(FILETP_OBJS) $(LIBS_flags) $(_LIBS_flags) $(FILETP_CMOS) 
 


mlfileTP+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlfileTP.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlfileTP+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(BITTORRENT)" , "yes")
SUBDIRS += src/networks/bittorrent

CORE_SRCS += $(BITTORRENT_SRCS)

## TARGETS += mlbt$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlbt+gui$(EXE)

endif
endif


mlbt_CMXA= cdk.cmxa common.cmxa client.cmxa mlbt.cmxa driver.cmxa
mlbt_SRCS= $(MAIN_SRCS)


BITTORRENT_ZOG := $(filter %.zog, $(BITTORRENT_SRCS)) 
BITTORRENT_MLL := $(filter %.mll, $(BITTORRENT_SRCS)) 
BITTORRENT_MLY := $(filter %.mly, $(BITTORRENT_SRCS)) 
BITTORRENT_ML4 := $(filter %.ml4, $(BITTORRENT_SRCS)) 
BITTORRENT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(BITTORRENT_SRCS)) 
BITTORRENT_C := $(filter %.c, $(BITTORRENT_SRCS)) 
BITTORRENT_CMOS=$(foreach file, $(BITTORRENT_ML),   $(basename $(file)).cmo) 
BITTORRENT_CMXS=$(foreach file, $(BITTORRENT_ML),   $(basename $(file)).cmx) 
BITTORRENT_OBJS=$(foreach file, $(BITTORRENT_C),   $(basename $(file)).o)    

TMPSOURCES += $(BITTORRENT_ML4:.ml4=.ml) $(BITTORRENT_MLL:.mll=.ml) $(BITTORRENT_MLY:.mly=.ml) $(BITTORRENT_MLY:.mly=.mli) $(BITTORRENT_ZOG:.zog=.ml) 
 
build/mlbt.cmxa: $(BITTORRENT_OBJS) $(BITTORRENT_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(BITTORRENT_OBJS) $(LIBS_flags) $(_LIBS_flags) $(BITTORRENT_CMXS) 
 
build/mlbt.cma: $(BITTORRENT_OBJS) $(BITTORRENT_CMOS) 
	$(OCAMLC) -a -o $@  $(BITTORRENT_OBJS) $(LIBS_flags) $(_LIBS_flags) $(BITTORRENT_CMOS) 
 


mlbt+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlbt.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlbt+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(DONKEY)" , "yes")
SUBDIRS += src/networks/donkey

CORE_SRCS += $(DONKEY_SRCS)

## TARGETS += mldonkey$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mldonkey+gui$(EXE)

endif
endif


mldonkey_CMXA= cdk.cmxa common.cmxa client.cmxa mldonkey.cmxa driver.cmxa
mldonkey_SRCS= $(MAIN_SRCS)


DONKEY_ZOG := $(filter %.zog, $(DONKEY_SRCS)) 
DONKEY_MLL := $(filter %.mll, $(DONKEY_SRCS)) 
DONKEY_MLY := $(filter %.mly, $(DONKEY_SRCS)) 
DONKEY_ML4 := $(filter %.ml4, $(DONKEY_SRCS)) 
DONKEY_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(DONKEY_SRCS)) 
DONKEY_C := $(filter %.c, $(DONKEY_SRCS)) 
DONKEY_CMOS=$(foreach file, $(DONKEY_ML),   $(basename $(file)).cmo) 
DONKEY_CMXS=$(foreach file, $(DONKEY_ML),   $(basename $(file)).cmx) 
DONKEY_OBJS=$(foreach file, $(DONKEY_C),   $(basename $(file)).o)    

TMPSOURCES += $(DONKEY_ML4:.ml4=.ml) $(DONKEY_MLL:.mll=.ml) $(DONKEY_MLY:.mly=.ml) $(DONKEY_MLY:.mly=.mli) $(DONKEY_ZOG:.zog=.ml) 
 
build/mldonkey.cmxa: $(DONKEY_OBJS) $(DONKEY_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(DONKEY_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DONKEY_CMXS) 
 
build/mldonkey.cma: $(DONKEY_OBJS) $(DONKEY_CMOS) 
	$(OCAMLC) -a -o $@  $(DONKEY_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DONKEY_CMOS) 
 


mldonkey+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mldonkey.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mldonkey+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(CYMES)" , "yes")
SUBDIRS += src/networks/cymes

CORE_SRCS += $(CYMES_SRCS)

## TARGETS += mlcymes$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlcymes+gui$(EXE)

endif
endif


mlcymes_CMXA= cdk.cmxa common.cmxa client.cmxa mlcymes.cmxa driver.cmxa
mlcymes_SRCS= $(MAIN_SRCS)


CYMES_ZOG := $(filter %.zog, $(CYMES_SRCS)) 
CYMES_MLL := $(filter %.mll, $(CYMES_SRCS)) 
CYMES_MLY := $(filter %.mly, $(CYMES_SRCS)) 
CYMES_ML4 := $(filter %.ml4, $(CYMES_SRCS)) 
CYMES_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(CYMES_SRCS)) 
CYMES_C := $(filter %.c, $(CYMES_SRCS)) 
CYMES_CMOS=$(foreach file, $(CYMES_ML),   $(basename $(file)).cmo) 
CYMES_CMXS=$(foreach file, $(CYMES_ML),   $(basename $(file)).cmx) 
CYMES_OBJS=$(foreach file, $(CYMES_C),   $(basename $(file)).o)    

TMPSOURCES += $(CYMES_ML4:.ml4=.ml) $(CYMES_MLL:.mll=.ml) $(CYMES_MLY:.mly=.ml) $(CYMES_MLY:.mly=.mli) $(CYMES_ZOG:.zog=.ml) 
 
build/mlcymes.cmxa: $(CYMES_OBJS) $(CYMES_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(CYMES_OBJS) $(LIBS_flags) $(_LIBS_flags) $(CYMES_CMXS) 
 
build/mlcymes.cma: $(CYMES_OBJS) $(CYMES_CMOS) 
	$(OCAMLC) -a -o $@  $(CYMES_OBJS) $(LIBS_flags) $(_LIBS_flags) $(CYMES_CMOS) 
 


mlcymes+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlcymes.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlcymes+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(SOULSEEK)" , "yes")
SUBDIRS += src/networks/soulseek

CORE_SRCS += $(SOULSEEK_SRCS)

## TARGETS += mlslsk$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlslsk+gui$(EXE)

endif
endif


mlslsk_CMXA= cdk.cmxa common.cmxa client.cmxa mlslsk.cmxa driver.cmxa
mlslsk_SRCS= $(MAIN_SRCS)


SOULSEEK_ZOG := $(filter %.zog, $(SOULSEEK_SRCS)) 
SOULSEEK_MLL := $(filter %.mll, $(SOULSEEK_SRCS)) 
SOULSEEK_MLY := $(filter %.mly, $(SOULSEEK_SRCS)) 
SOULSEEK_ML4 := $(filter %.ml4, $(SOULSEEK_SRCS)) 
SOULSEEK_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(SOULSEEK_SRCS)) 
SOULSEEK_C := $(filter %.c, $(SOULSEEK_SRCS)) 
SOULSEEK_CMOS=$(foreach file, $(SOULSEEK_ML),   $(basename $(file)).cmo) 
SOULSEEK_CMXS=$(foreach file, $(SOULSEEK_ML),   $(basename $(file)).cmx) 
SOULSEEK_OBJS=$(foreach file, $(SOULSEEK_C),   $(basename $(file)).o)    

TMPSOURCES += $(SOULSEEK_ML4:.ml4=.ml) $(SOULSEEK_MLL:.mll=.ml) $(SOULSEEK_MLY:.mly=.ml) $(SOULSEEK_MLY:.mly=.mli) $(SOULSEEK_ZOG:.zog=.ml) 
 
build/mlslsk.cmxa: $(SOULSEEK_OBJS) $(SOULSEEK_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(SOULSEEK_OBJS) $(LIBS_flags) $(_LIBS_flags) $(SOULSEEK_CMXS) 
 
build/mlslsk.cma: $(SOULSEEK_OBJS) $(SOULSEEK_CMOS) 
	$(OCAMLC) -a -o $@  $(SOULSEEK_OBJS) $(LIBS_flags) $(_LIBS_flags) $(SOULSEEK_CMOS) 
 


mlslsk+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlslsk.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlslsk+gui_SRCS= $(MAIN_SRCS)



libcdk_SRCS=  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS)
libcommon_SRCS= $(CHAT_SRCS) $(COMMON_SRCS)
libclient_SRCS= $(COMMON_CLIENT_SRCS)
libgmisc_SRCS=  $(CONFIGWIN_SRCS) $(MP3TAGUI_SRCS) \
  $(OKEY_SRCS) $(GPATTERN_SRCS)
libguibase_SRCS= $(IM_CORE) $(GUI_BASE_SRCS)
libgui_SRCS=   $(GUI_SRCS)
libgui3_SRCS=   $(GUI3_SRCS)
libicons_SRCS= $(ALL_ICONS_SRCS)


libicons_ZOG := $(filter %.zog, $(libicons_SRCS)) 
libicons_MLL := $(filter %.mll, $(libicons_SRCS)) 
libicons_MLY := $(filter %.mly, $(libicons_SRCS)) 
libicons_ML4 := $(filter %.ml4, $(libicons_SRCS)) 
libicons_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(libicons_SRCS)) 
libicons_C := $(filter %.c, $(libicons_SRCS)) 
libicons_CMOS=$(foreach file, $(libicons_ML),   $(basename $(file)).cmo) 
libicons_CMXS=$(foreach file, $(libicons_ML),   $(basename $(file)).cmx) 
libicons_OBJS=$(foreach file, $(libicons_C),   $(basename $(file)).o)    

TMPSOURCES += $(libicons_ML4:.ml4=.ml) $(libicons_MLL:.mll=.ml) $(libicons_MLY:.mly=.ml) $(libicons_MLY:.mly=.mli) $(libicons_ZOG:.zog=.ml) 
 
build/icons.cmxa: $(libicons_OBJS) $(libicons_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libicons_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libicons_CMXS) 
 
build/icons.cma: $(libicons_OBJS) $(libicons_CMOS) 
	$(OCAMLC) -a -o $@  $(libicons_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libicons_CMOS) 
 


libcdk_ZOG := $(filter %.zog, $(libcdk_SRCS)) 
libcdk_MLL := $(filter %.mll, $(libcdk_SRCS)) 
libcdk_MLY := $(filter %.mly, $(libcdk_SRCS)) 
libcdk_ML4 := $(filter %.ml4, $(libcdk_SRCS)) 
libcdk_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(libcdk_SRCS)) 
libcdk_C := $(filter %.c, $(libcdk_SRCS)) 
libcdk_CMOS=$(foreach file, $(libcdk_ML),   $(basename $(file)).cmo) 
libcdk_CMXS=$(foreach file, $(libcdk_ML),   $(basename $(file)).cmx) 
libcdk_OBJS=$(foreach file, $(libcdk_C),   $(basename $(file)).o)    

TMPSOURCES += $(libcdk_ML4:.ml4=.ml) $(libcdk_MLL:.mll=.ml) $(libcdk_MLY:.mly=.ml) $(libcdk_MLY:.mly=.mli) $(libcdk_ZOG:.zog=.ml) 
 
build/cdk.cmxa: $(libcdk_OBJS) $(libcdk_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libcdk_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcdk_CMXS) 
 
build/cdk.cma: $(libcdk_OBJS) $(libcdk_CMOS) 
	$(OCAMLC) -a -o $@  $(libcdk_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcdk_CMOS) 
 


libcommon_ZOG := $(filter %.zog, $(libcommon_SRCS)) 
libcommon_MLL := $(filter %.mll, $(libcommon_SRCS)) 
libcommon_MLY := $(filter %.mly, $(libcommon_SRCS)) 
libcommon_ML4 := $(filter %.ml4, $(libcommon_SRCS)) 
libcommon_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(libcommon_SRCS)) 
libcommon_C := $(filter %.c, $(libcommon_SRCS)) 
libcommon_CMOS=$(foreach file, $(libcommon_ML),   $(basename $(file)).cmo) 
libcommon_CMXS=$(foreach file, $(libcommon_ML),   $(basename $(file)).cmx) 
libcommon_OBJS=$(foreach file, $(libcommon_C),   $(basename $(file)).o)    

TMPSOURCES += $(libcommon_ML4:.ml4=.ml) $(libcommon_MLL:.mll=.ml) $(libcommon_MLY:.mly=.ml) $(libcommon_MLY:.mly=.mli) $(libcommon_ZOG:.zog=.ml) 
 
build/common.cmxa: $(libcommon_OBJS) $(libcommon_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libcommon_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcommon_CMXS) 
 
build/common.cma: $(libcommon_OBJS) $(libcommon_CMOS) 
	$(OCAMLC) -a -o $@  $(libcommon_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcommon_CMOS) 
 


libclient_ZOG := $(filter %.zog, $(libclient_SRCS)) 
libclient_MLL := $(filter %.mll, $(libclient_SRCS)) 
libclient_MLY := $(filter %.mly, $(libclient_SRCS)) 
libclient_ML4 := $(filter %.ml4, $(libclient_SRCS)) 
libclient_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(libclient_SRCS)) 
libclient_C := $(filter %.c, $(libclient_SRCS)) 
libclient_CMOS=$(foreach file, $(libclient_ML),   $(basename $(file)).cmo) 
libclient_CMXS=$(foreach file, $(libclient_ML),   $(basename $(file)).cmx) 
libclient_OBJS=$(foreach file, $(libclient_C),   $(basename $(file)).o)    

TMPSOURCES += $(libclient_ML4:.ml4=.ml) $(libclient_MLL:.mll=.ml) $(libclient_MLY:.mly=.ml) $(libclient_MLY:.mly=.mli) $(libclient_ZOG:.zog=.ml) 
 
build/client.cmxa: $(libclient_OBJS) $(libclient_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libclient_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libclient_CMXS) 
 
build/client.cma: $(libclient_OBJS) $(libclient_CMOS) 
	$(OCAMLC) -a -o $@  $(libclient_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libclient_CMOS) 
 


DRIVER_ZOG := $(filter %.zog, $(DRIVER_SRCS)) 
DRIVER_MLL := $(filter %.mll, $(DRIVER_SRCS)) 
DRIVER_MLY := $(filter %.mly, $(DRIVER_SRCS)) 
DRIVER_ML4 := $(filter %.ml4, $(DRIVER_SRCS)) 
DRIVER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(DRIVER_SRCS)) 
DRIVER_C := $(filter %.c, $(DRIVER_SRCS)) 
DRIVER_CMOS=$(foreach file, $(DRIVER_ML),   $(basename $(file)).cmo) 
DRIVER_CMXS=$(foreach file, $(DRIVER_ML),   $(basename $(file)).cmx) 
DRIVER_OBJS=$(foreach file, $(DRIVER_C),   $(basename $(file)).o)    

TMPSOURCES += $(DRIVER_ML4:.ml4=.ml) $(DRIVER_MLL:.mll=.ml) $(DRIVER_MLY:.mly=.ml) $(DRIVER_MLY:.mly=.mli) $(DRIVER_ZOG:.zog=.ml) 
 
build/driver.cmxa: $(DRIVER_OBJS) $(DRIVER_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(DRIVER_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DRIVER_CMXS) 
 
build/driver.cma: $(DRIVER_OBJS) $(DRIVER_CMOS) 
	$(OCAMLC) -a -o $@  $(DRIVER_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DRIVER_CMOS) 
 


CORE_ZOG := $(filter %.zog, $(CORE_SRCS)) 
CORE_MLL := $(filter %.mll, $(CORE_SRCS)) 
CORE_MLY := $(filter %.mly, $(CORE_SRCS)) 
CORE_ML4 := $(filter %.ml4, $(CORE_SRCS)) 
CORE_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(CORE_SRCS)) 
CORE_C := $(filter %.c, $(CORE_SRCS)) 
CORE_CMOS=$(foreach file, $(CORE_ML),   $(basename $(file)).cmo) 
CORE_CMXS=$(foreach file, $(CORE_ML),   $(basename $(file)).cmx) 
CORE_OBJS=$(foreach file, $(CORE_C),   $(basename $(file)).o)    

TMPSOURCES += $(CORE_ML4:.ml4=.ml) $(CORE_MLL:.mll=.ml) $(CORE_MLY:.mly=.ml) $(CORE_MLY:.mly=.mli) $(CORE_ZOG:.zog=.ml) 
 
build/core.cmxa: $(CORE_OBJS) $(CORE_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(CORE_OBJS) $(LIBS_flags) $(_LIBS_flags) $(CORE_CMXS) 
 
build/core.cma: $(CORE_OBJS) $(CORE_CMOS) 
	$(OCAMLC) -a -o $@  $(CORE_OBJS) $(LIBS_flags) $(_LIBS_flags) $(CORE_CMOS) 
 


libgmisc_ZOG := $(filter %.zog, $(libgmisc_SRCS)) 
libgmisc_MLL := $(filter %.mll, $(libgmisc_SRCS)) 
libgmisc_MLY := $(filter %.mly, $(libgmisc_SRCS)) 
libgmisc_ML4 := $(filter %.ml4, $(libgmisc_SRCS)) 
libgmisc_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(libgmisc_SRCS)) 
libgmisc_C := $(filter %.c, $(libgmisc_SRCS)) 
libgmisc_CMOS=$(foreach file, $(libgmisc_ML),   $(basename $(file)).cmo) 
libgmisc_CMXS=$(foreach file, $(libgmisc_ML),   $(basename $(file)).cmx) 
libgmisc_OBJS=$(foreach file, $(libgmisc_C),   $(basename $(file)).o)    

TMPSOURCES += $(libgmisc_ML4:.ml4=.ml) $(libgmisc_MLL:.mll=.ml) $(libgmisc_MLY:.mly=.ml) $(libgmisc_MLY:.mly=.mli) $(libgmisc_ZOG:.zog=.ml) 
 
build/gmisc.cmxa: $(libgmisc_OBJS) $(libgmisc_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libgmisc_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgmisc_CMXS) 
 
build/gmisc.cma: $(libgmisc_OBJS) $(libgmisc_CMOS) 
	$(OCAMLC) -a -o $@  $(libgmisc_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgmisc_CMOS) 
 


libgui_ZOG := $(filter %.zog, $(libgui_SRCS)) 
libgui_MLL := $(filter %.mll, $(libgui_SRCS)) 
libgui_MLY := $(filter %.mly, $(libgui_SRCS)) 
libgui_ML4 := $(filter %.ml4, $(libgui_SRCS)) 
libgui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(libgui_SRCS)) 
libgui_C := $(filter %.c, $(libgui_SRCS)) 
libgui_CMOS=$(foreach file, $(libgui_ML),   $(basename $(file)).cmo) 
libgui_CMXS=$(foreach file, $(libgui_ML),   $(basename $(file)).cmx) 
libgui_OBJS=$(foreach file, $(libgui_C),   $(basename $(file)).o)    

TMPSOURCES += $(libgui_ML4:.ml4=.ml) $(libgui_MLL:.mll=.ml) $(libgui_MLY:.mly=.ml) $(libgui_MLY:.mly=.mli) $(libgui_ZOG:.zog=.ml) 
 
build/gui.cmxa: $(libgui_OBJS) $(libgui_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libgui_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgui_CMXS) 
 
build/gui.cma: $(libgui_OBJS) $(libgui_CMOS) 
	$(OCAMLC) -a -o $@  $(libgui_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgui_CMOS) 
 


libguibase_ZOG := $(filter %.zog, $(libguibase_SRCS)) 
libguibase_MLL := $(filter %.mll, $(libguibase_SRCS)) 
libguibase_MLY := $(filter %.mly, $(libguibase_SRCS)) 
libguibase_ML4 := $(filter %.ml4, $(libguibase_SRCS)) 
libguibase_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(libguibase_SRCS)) 
libguibase_C := $(filter %.c, $(libguibase_SRCS)) 
libguibase_CMOS=$(foreach file, $(libguibase_ML),   $(basename $(file)).cmo) 
libguibase_CMXS=$(foreach file, $(libguibase_ML),   $(basename $(file)).cmx) 
libguibase_OBJS=$(foreach file, $(libguibase_C),   $(basename $(file)).o)    

TMPSOURCES += $(libguibase_ML4:.ml4=.ml) $(libguibase_MLL:.mll=.ml) $(libguibase_MLY:.mly=.ml) $(libguibase_MLY:.mly=.mli) $(libguibase_ZOG:.zog=.ml) 
 
build/guibase.cmxa: $(libguibase_OBJS) $(libguibase_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libguibase_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libguibase_CMXS) 
 
build/guibase.cma: $(libguibase_OBJS) $(libguibase_CMOS) 
	$(OCAMLC) -a -o $@  $(libguibase_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libguibase_CMOS) 
 



#######################################################################

#              Objects files for "use_tags"

#######################################################################

USE_TAGS_CMXA=cdk.cmxa
USE_TAGS_SRCS = \
  $(LIB)/cddb_lexer.mll $(LIB)/cddb_file.ml \
  tools/use_tags.ml

HASH_FILES_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(SRC_DONKEY)/donkeyHasher.ml tools/hash_files.ml


######################################################################

#         From sources to objects files

######################################################################

NO_CMXA=




mldonkey_ZOG := $(filter %.zog, $(mldonkey_SRCS)) 
mldonkey_MLL := $(filter %.mll, $(mldonkey_SRCS)) 
mldonkey_MLY := $(filter %.mly, $(mldonkey_SRCS)) 
mldonkey_ML4 := $(filter %.ml4, $(mldonkey_SRCS)) 
mldonkey_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mldonkey_SRCS)) 
mldonkey_C := $(filter %.c, $(mldonkey_SRCS)) 
mldonkey_CMOS=$(foreach file, $(mldonkey_ML),   $(basename $(file)).cmo) 
mldonkey_CMXS=$(foreach file, $(mldonkey_ML),   $(basename $(file)).cmx) 
mldonkey_OBJS=$(foreach file, $(mldonkey_C),   $(basename $(file)).o)    

mldonkey_CMXAS := $(foreach file, $(mldonkey_CMXA),   build/$(basename $(file)).cmxa)
mldonkey_CMAS=$(foreach file, $(mldonkey_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldonkey_ML4:.ml4=.ml) $(mldonkey_MLL:.mll=.ml) $(mldonkey_MLY:.mly=.ml) $(mldonkey_MLY:.mly=.mli) $(mldonkey_ZOG:.zog=.ml) 
 
mldonkey: $(mldonkey_OBJS) $(mldonkey_CMXS) $(mldonkey_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mldonkey_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(mldonkey_CMXAS) $(mldonkey_CMXS) 
 
mldonkey.byte: $(mldonkey_OBJS) $(mldonkey_CMOS)  $(mldonkey_CMAS)
	$(OCAMLC) -linkall -o $@  $(mldonkey_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(mldonkey_CMAS) $(mldonkey_CMOS) 
 
mldonkey.static:  $(mldonkey_OBJS) $(mldonkey_CMXS)  $(mldonkey_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mldonkey_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(mldonkey_CMXAS) $(mldonkey_CMXS)


mldonkey+gui_ZOG := $(filter %.zog, $(mldonkey+gui_SRCS)) 
mldonkey+gui_MLL := $(filter %.mll, $(mldonkey+gui_SRCS)) 
mldonkey+gui_MLY := $(filter %.mly, $(mldonkey+gui_SRCS)) 
mldonkey+gui_ML4 := $(filter %.ml4, $(mldonkey+gui_SRCS)) 
mldonkey+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mldonkey+gui_SRCS)) 
mldonkey+gui_C := $(filter %.c, $(mldonkey+gui_SRCS)) 
mldonkey+gui_CMOS=$(foreach file, $(mldonkey+gui_ML),   $(basename $(file)).cmo) 
mldonkey+gui_CMXS=$(foreach file, $(mldonkey+gui_ML),   $(basename $(file)).cmx) 
mldonkey+gui_OBJS=$(foreach file, $(mldonkey+gui_C),   $(basename $(file)).o)    

mldonkey+gui_CMXAS := $(foreach file, $(mldonkey+gui_CMXA),   build/$(basename $(file)).cmxa)
mldonkey+gui_CMAS=$(foreach file, $(mldonkey+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldonkey+gui_ML4:.ml4=.ml) $(mldonkey+gui_MLL:.mll=.ml) $(mldonkey+gui_MLY:.mly=.ml) $(mldonkey+gui_MLY:.mly=.mli) $(mldonkey+gui_ZOG:.zog=.ml) 
 
mldonkey+gui: $(mldonkey+gui_OBJS) $(mldonkey+gui_CMXS) $(mldonkey+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mldonkey+gui_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(mldonkey+gui_CMXAS) $(mldonkey+gui_CMXS) 
 
mldonkey+gui.byte: $(mldonkey+gui_OBJS) $(mldonkey+gui_CMOS)  $(mldonkey+gui_CMAS)
	$(OCAMLC) -linkall -o $@  $(mldonkey+gui_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(mldonkey+gui_CMAS) $(mldonkey+gui_CMOS) 
 
mldonkey+gui.static:  $(mldonkey+gui_OBJS) $(mldonkey+gui_CMXS)  $(mldonkey+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mldonkey+gui_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(mldonkey+gui_CMXAS) $(mldonkey+gui_CMXS)


MLPROGRESS_ZOG := $(filter %.zog, $(MLPROGRESS_SRCS)) 
MLPROGRESS_MLL := $(filter %.mll, $(MLPROGRESS_SRCS)) 
MLPROGRESS_MLY := $(filter %.mly, $(MLPROGRESS_SRCS)) 
MLPROGRESS_ML4 := $(filter %.ml4, $(MLPROGRESS_SRCS)) 
MLPROGRESS_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MLPROGRESS_SRCS)) 
MLPROGRESS_C := $(filter %.c, $(MLPROGRESS_SRCS)) 
MLPROGRESS_CMOS=$(foreach file, $(MLPROGRESS_ML),   $(basename $(file)).cmo) 
MLPROGRESS_CMXS=$(foreach file, $(MLPROGRESS_ML),   $(basename $(file)).cmx) 
MLPROGRESS_OBJS=$(foreach file, $(MLPROGRESS_C),   $(basename $(file)).o)    

MLPROGRESS_CMXAS := $(foreach file, $(MLPROGRESS_CMXA),   build/$(basename $(file)).cmxa)
MLPROGRESS_CMAS=$(foreach file, $(MLPROGRESS_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLPROGRESS_ML4:.ml4=.ml) $(MLPROGRESS_MLL:.mll=.ml) $(MLPROGRESS_MLY:.mly=.ml) $(MLPROGRESS_MLY:.mly=.mli) $(MLPROGRESS_ZOG:.zog=.ml) 
 
mlprogress: $(MLPROGRESS_OBJS) $(MLPROGRESS_CMXS) $(MLPROGRESS_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLPROGRESS_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLPROGRESS_CMXAS) $(MLPROGRESS_CMXS) 
 
mlprogress.byte: $(MLPROGRESS_OBJS) $(MLPROGRESS_CMOS)  $(MLPROGRESS_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLPROGRESS_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLPROGRESS_CMAS) $(MLPROGRESS_CMOS) 
 
mlprogress.static:  $(MLPROGRESS_OBJS) $(MLPROGRESS_CMXS)  $(MLPROGRESS_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLPROGRESS_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(MLPROGRESS_CMXAS) $(MLPROGRESS_CMXS)


MLDONKEYGUI_ZOG := $(filter %.zog, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_MLL := $(filter %.mll, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_MLY := $(filter %.mly, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_ML4 := $(filter %.ml4, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_C := $(filter %.c, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_CMOS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmo) 
MLDONKEYGUI_CMXS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmx) 
MLDONKEYGUI_OBJS=$(foreach file, $(MLDONKEYGUI_C),   $(basename $(file)).o)    

MLDONKEYGUI_CMXAS := $(foreach file, $(MLDONKEYGUI_CMXA),   build/$(basename $(file)).cmxa)
MLDONKEYGUI_CMAS=$(foreach file, $(MLDONKEYGUI_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLDONKEYGUI_ML4:.ml4=.ml) $(MLDONKEYGUI_MLL:.mll=.ml) $(MLDONKEYGUI_MLY:.mly=.ml) $(MLDONKEYGUI_MLY:.mly=.mli) $(MLDONKEYGUI_ZOG:.zog=.ml) 
 
mlgui: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS) $(MLDONKEYGUI_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI_CMXAS) $(MLDONKEYGUI_CMXS) 
 
mlgui.byte: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMOS)  $(MLDONKEYGUI_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI_CMAS) $(MLDONKEYGUI_CMOS) 
 
mlgui.static:  $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS)  $(MLDONKEYGUI_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEYGUI_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(MLDONKEYGUI_CMXAS) $(MLDONKEYGUI_CMXS)


MLDONKEYGUI2_ZOG := $(filter %.zog, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLL := $(filter %.mll, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLY := $(filter %.mly, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_ML4 := $(filter %.ml4, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_C := $(filter %.c, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_CMOS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmo) 
MLDONKEYGUI2_CMXS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmx) 
MLDONKEYGUI2_OBJS=$(foreach file, $(MLDONKEYGUI2_C),   $(basename $(file)).o)    

MLDONKEYGUI2_CMXAS := $(foreach file, $(MLDONKEYGUI2_CMXA),   build/$(basename $(file)).cmxa)
MLDONKEYGUI2_CMAS=$(foreach file, $(MLDONKEYGUI2_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLDONKEYGUI2_ML4:.ml4=.ml) $(MLDONKEYGUI2_MLL:.mll=.ml) $(MLDONKEYGUI2_MLY:.mly=.ml) $(MLDONKEYGUI2_MLY:.mly=.mli) $(MLDONKEYGUI2_ZOG:.zog=.ml) 
 
mlgui2: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS) $(MLDONKEYGUI2_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI2_CMXAS) $(MLDONKEYGUI2_CMXS) 
 
mlgui2.byte: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMOS)  $(MLDONKEYGUI2_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI2_CMAS) $(MLDONKEYGUI2_CMOS) 
 
mlgui2.static:  $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS)  $(MLDONKEYGUI2_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEYGUI2_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(MLDONKEYGUI2_CMXAS) $(MLDONKEYGUI2_CMXS)


mldc_ZOG := $(filter %.zog, $(mldc_SRCS)) 
mldc_MLL := $(filter %.mll, $(mldc_SRCS)) 
mldc_MLY := $(filter %.mly, $(mldc_SRCS)) 
mldc_ML4 := $(filter %.ml4, $(mldc_SRCS)) 
mldc_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mldc_SRCS)) 
mldc_C := $(filter %.c, $(mldc_SRCS)) 
mldc_CMOS=$(foreach file, $(mldc_ML),   $(basename $(file)).cmo) 
mldc_CMXS=$(foreach file, $(mldc_ML),   $(basename $(file)).cmx) 
mldc_OBJS=$(foreach file, $(mldc_C),   $(basename $(file)).o)    

mldc_CMXAS := $(foreach file, $(mldc_CMXA),   build/$(basename $(file)).cmxa)
mldc_CMAS=$(foreach file, $(mldc_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldc_ML4:.ml4=.ml) $(mldc_MLL:.mll=.ml) $(mldc_MLY:.mly=.ml) $(mldc_MLY:.mly=.mli) $(mldc_ZOG:.zog=.ml) 
 
mldc: $(mldc_OBJS) $(mldc_CMXS) $(mldc_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mldc_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(mldc_CMXAS) $(mldc_CMXS) 
 
mldc.byte: $(mldc_OBJS) $(mldc_CMOS)  $(mldc_CMAS)
	$(OCAMLC) -linkall -o $@  $(mldc_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(mldc_CMAS) $(mldc_CMOS) 
 
mldc.static:  $(mldc_OBJS) $(mldc_CMXS)  $(mldc_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mldc_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(mldc_CMXAS) $(mldc_CMXS)


mldc+gui_ZOG := $(filter %.zog, $(mldc+gui_SRCS)) 
mldc+gui_MLL := $(filter %.mll, $(mldc+gui_SRCS)) 
mldc+gui_MLY := $(filter %.mly, $(mldc+gui_SRCS)) 
mldc+gui_ML4 := $(filter %.ml4, $(mldc+gui_SRCS)) 
mldc+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mldc+gui_SRCS)) 
mldc+gui_C := $(filter %.c, $(mldc+gui_SRCS)) 
mldc+gui_CMOS=$(foreach file, $(mldc+gui_ML),   $(basename $(file)).cmo) 
mldc+gui_CMXS=$(foreach file, $(mldc+gui_ML),   $(basename $(file)).cmx) 
mldc+gui_OBJS=$(foreach file, $(mldc+gui_C),   $(basename $(file)).o)    

mldc+gui_CMXAS := $(foreach file, $(mldc+gui_CMXA),   build/$(basename $(file)).cmxa)
mldc+gui_CMAS=$(foreach file, $(mldc+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldc+gui_ML4:.ml4=.ml) $(mldc+gui_MLL:.mll=.ml) $(mldc+gui_MLY:.mly=.ml) $(mldc+gui_MLY:.mly=.mli) $(mldc+gui_ZOG:.zog=.ml) 
 
mldc+gui: $(mldc+gui_OBJS) $(mldc+gui_CMXS) $(mldc+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mldc+gui_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(mldc+gui_CMXAS) $(mldc+gui_CMXS) 
 
mldc+gui.byte: $(mldc+gui_OBJS) $(mldc+gui_CMOS)  $(mldc+gui_CMAS)
	$(OCAMLC) -linkall -o $@  $(mldc+gui_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(mldc+gui_CMAS) $(mldc+gui_CMOS) 
 
mldc+gui.static:  $(mldc+gui_OBJS) $(mldc+gui_CMXS)  $(mldc+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mldc+gui_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(mldc+gui_CMXAS) $(mldc+gui_CMXS)


mlnap_ZOG := $(filter %.zog, $(mlnap_SRCS)) 
mlnap_MLL := $(filter %.mll, $(mlnap_SRCS)) 
mlnap_MLY := $(filter %.mly, $(mlnap_SRCS)) 
mlnap_ML4 := $(filter %.ml4, $(mlnap_SRCS)) 
mlnap_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlnap_SRCS)) 
mlnap_C := $(filter %.c, $(mlnap_SRCS)) 
mlnap_CMOS=$(foreach file, $(mlnap_ML),   $(basename $(file)).cmo) 
mlnap_CMXS=$(foreach file, $(mlnap_ML),   $(basename $(file)).cmx) 
mlnap_OBJS=$(foreach file, $(mlnap_C),   $(basename $(file)).o)    

mlnap_CMXAS := $(foreach file, $(mlnap_CMXA),   build/$(basename $(file)).cmxa)
mlnap_CMAS=$(foreach file, $(mlnap_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlnap_ML4:.ml4=.ml) $(mlnap_MLL:.mll=.ml) $(mlnap_MLY:.mly=.ml) $(mlnap_MLY:.mly=.mli) $(mlnap_ZOG:.zog=.ml) 
 
mlnap: $(mlnap_OBJS) $(mlnap_CMXS) $(mlnap_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlnap_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(mlnap_CMXAS) $(mlnap_CMXS) 
 
mlnap.byte: $(mlnap_OBJS) $(mlnap_CMOS)  $(mlnap_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlnap_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(mlnap_CMAS) $(mlnap_CMOS) 
 
mlnap.static:  $(mlnap_OBJS) $(mlnap_CMXS)  $(mlnap_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlnap_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(mlnap_CMXAS) $(mlnap_CMXS)


mlnap+gui_ZOG := $(filter %.zog, $(mlnap+gui_SRCS)) 
mlnap+gui_MLL := $(filter %.mll, $(mlnap+gui_SRCS)) 
mlnap+gui_MLY := $(filter %.mly, $(mlnap+gui_SRCS)) 
mlnap+gui_ML4 := $(filter %.ml4, $(mlnap+gui_SRCS)) 
mlnap+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlnap+gui_SRCS)) 
mlnap+gui_C := $(filter %.c, $(mlnap+gui_SRCS)) 
mlnap+gui_CMOS=$(foreach file, $(mlnap+gui_ML),   $(basename $(file)).cmo) 
mlnap+gui_CMXS=$(foreach file, $(mlnap+gui_ML),   $(basename $(file)).cmx) 
mlnap+gui_OBJS=$(foreach file, $(mlnap+gui_C),   $(basename $(file)).o)    

mlnap+gui_CMXAS := $(foreach file, $(mlnap+gui_CMXA),   build/$(basename $(file)).cmxa)
mlnap+gui_CMAS=$(foreach file, $(mlnap+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlnap+gui_ML4:.ml4=.ml) $(mlnap+gui_MLL:.mll=.ml) $(mlnap+gui_MLY:.mly=.ml) $(mlnap+gui_MLY:.mly=.mli) $(mlnap+gui_ZOG:.zog=.ml) 
 
mlnap+gui: $(mlnap+gui_OBJS) $(mlnap+gui_CMXS) $(mlnap+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlnap+gui_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(mlnap+gui_CMXAS) $(mlnap+gui_CMXS) 
 
mlnap+gui.byte: $(mlnap+gui_OBJS) $(mlnap+gui_CMOS)  $(mlnap+gui_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlnap+gui_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(mlnap+gui_CMAS) $(mlnap+gui_CMOS) 
 
mlnap+gui.static:  $(mlnap+gui_OBJS) $(mlnap+gui_CMXS)  $(mlnap+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlnap+gui_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(mlnap+gui_CMXAS) $(mlnap+gui_CMXS)


MLNET_ZOG := $(filter %.zog, $(MLNET_SRCS)) 
MLNET_MLL := $(filter %.mll, $(MLNET_SRCS)) 
MLNET_MLY := $(filter %.mly, $(MLNET_SRCS)) 
MLNET_ML4 := $(filter %.ml4, $(MLNET_SRCS)) 
MLNET_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MLNET_SRCS)) 
MLNET_C := $(filter %.c, $(MLNET_SRCS)) 
MLNET_CMOS=$(foreach file, $(MLNET_ML),   $(basename $(file)).cmo) 
MLNET_CMXS=$(foreach file, $(MLNET_ML),   $(basename $(file)).cmx) 
MLNET_OBJS=$(foreach file, $(MLNET_C),   $(basename $(file)).o)    

MLNET_CMXAS := $(foreach file, $(MLNET_CMXA),   build/$(basename $(file)).cmxa)
MLNET_CMAS=$(foreach file, $(MLNET_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLNET_ML4:.ml4=.ml) $(MLNET_MLL:.mll=.ml) $(MLNET_MLY:.mly=.ml) $(MLNET_MLY:.mly=.mli) $(MLNET_ZOG:.zog=.ml) 
 
mlnet: $(MLNET_OBJS) $(MLNET_CMXS) $(MLNET_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLNET_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(MLNET_CMXAS) $(MLNET_CMXS) 
 
mlnet.byte: $(MLNET_OBJS) $(MLNET_CMOS)  $(MLNET_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLNET_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(MLNET_CMAS) $(MLNET_CMOS) 
 
mlnet.static:  $(MLNET_OBJS) $(MLNET_CMXS)  $(MLNET_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLNET_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(MLNET_CMXAS) $(MLNET_CMXS)


mlnet+gui_ZOG := $(filter %.zog, $(mlnet+gui_SRCS)) 
mlnet+gui_MLL := $(filter %.mll, $(mlnet+gui_SRCS)) 
mlnet+gui_MLY := $(filter %.mly, $(mlnet+gui_SRCS)) 
mlnet+gui_ML4 := $(filter %.ml4, $(mlnet+gui_SRCS)) 
mlnet+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlnet+gui_SRCS)) 
mlnet+gui_C := $(filter %.c, $(mlnet+gui_SRCS)) 
mlnet+gui_CMOS=$(foreach file, $(mlnet+gui_ML),   $(basename $(file)).cmo) 
mlnet+gui_CMXS=$(foreach file, $(mlnet+gui_ML),   $(basename $(file)).cmx) 
mlnet+gui_OBJS=$(foreach file, $(mlnet+gui_C),   $(basename $(file)).o)    

mlnet+gui_CMXAS := $(foreach file, $(mlnet+gui_CMXA),   build/$(basename $(file)).cmxa)
mlnet+gui_CMAS=$(foreach file, $(mlnet+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlnet+gui_ML4:.ml4=.ml) $(mlnet+gui_MLL:.mll=.ml) $(mlnet+gui_MLY:.mly=.ml) $(mlnet+gui_MLY:.mly=.mli) $(mlnet+gui_ZOG:.zog=.ml) 
 
mlnet+gui: $(mlnet+gui_OBJS) $(mlnet+gui_CMXS) $(mlnet+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlnet+gui_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(mlnet+gui_CMXAS) $(mlnet+gui_CMXS) 
 
mlnet+gui.byte: $(mlnet+gui_OBJS) $(mlnet+gui_CMOS)  $(mlnet+gui_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlnet+gui_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(mlnet+gui_CMAS) $(mlnet+gui_CMOS) 
 
mlnet+gui.static:  $(mlnet+gui_OBJS) $(mlnet+gui_CMXS)  $(mlnet+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlnet+gui_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(mlnet+gui_CMXAS) $(mlnet+gui_CMXS)


mlgnut_ZOG := $(filter %.zog, $(mlgnut_SRCS)) 
mlgnut_MLL := $(filter %.mll, $(mlgnut_SRCS)) 
mlgnut_MLY := $(filter %.mly, $(mlgnut_SRCS)) 
mlgnut_ML4 := $(filter %.ml4, $(mlgnut_SRCS)) 
mlgnut_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlgnut_SRCS)) 
mlgnut_C := $(filter %.c, $(mlgnut_SRCS)) 
mlgnut_CMOS=$(foreach file, $(mlgnut_ML),   $(basename $(file)).cmo) 
mlgnut_CMXS=$(foreach file, $(mlgnut_ML),   $(basename $(file)).cmx) 
mlgnut_OBJS=$(foreach file, $(mlgnut_C),   $(basename $(file)).o)    

mlgnut_CMXAS := $(foreach file, $(mlgnut_CMXA),   build/$(basename $(file)).cmxa)
mlgnut_CMAS=$(foreach file, $(mlgnut_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlgnut_ML4:.ml4=.ml) $(mlgnut_MLL:.mll=.ml) $(mlgnut_MLY:.mly=.ml) $(mlgnut_MLY:.mly=.mli) $(mlgnut_ZOG:.zog=.ml) 
 
mlgnut: $(mlgnut_OBJS) $(mlgnut_CMXS) $(mlgnut_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlgnut_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(mlgnut_CMXAS) $(mlgnut_CMXS) 
 
mlgnut.byte: $(mlgnut_OBJS) $(mlgnut_CMOS)  $(mlgnut_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlgnut_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(mlgnut_CMAS) $(mlgnut_CMOS) 
 
mlgnut.static:  $(mlgnut_OBJS) $(mlgnut_CMXS)  $(mlgnut_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlgnut_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(mlgnut_CMXAS) $(mlgnut_CMXS)


mlbt_ZOG := $(filter %.zog, $(mlbt_SRCS)) 
mlbt_MLL := $(filter %.mll, $(mlbt_SRCS)) 
mlbt_MLY := $(filter %.mly, $(mlbt_SRCS)) 
mlbt_ML4 := $(filter %.ml4, $(mlbt_SRCS)) 
mlbt_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlbt_SRCS)) 
mlbt_C := $(filter %.c, $(mlbt_SRCS)) 
mlbt_CMOS=$(foreach file, $(mlbt_ML),   $(basename $(file)).cmo) 
mlbt_CMXS=$(foreach file, $(mlbt_ML),   $(basename $(file)).cmx) 
mlbt_OBJS=$(foreach file, $(mlbt_C),   $(basename $(file)).o)    

mlbt_CMXAS := $(foreach file, $(mlbt_CMXA),   build/$(basename $(file)).cmxa)
mlbt_CMAS=$(foreach file, $(mlbt_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlbt_ML4:.ml4=.ml) $(mlbt_MLL:.mll=.ml) $(mlbt_MLY:.mly=.ml) $(mlbt_MLY:.mly=.mli) $(mlbt_ZOG:.zog=.ml) 
 
mlbt: $(mlbt_OBJS) $(mlbt_CMXS) $(mlbt_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlbt_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(mlbt_CMXAS) $(mlbt_CMXS) 
 
mlbt.byte: $(mlbt_OBJS) $(mlbt_CMOS)  $(mlbt_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlbt_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(mlbt_CMAS) $(mlbt_CMOS) 
 
mlbt.static:  $(mlbt_OBJS) $(mlbt_CMXS)  $(mlbt_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlbt_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(mlbt_CMXAS) $(mlbt_CMXS)


mlgnut+gui_ZOG := $(filter %.zog, $(mlgnut+gui_SRCS)) 
mlgnut+gui_MLL := $(filter %.mll, $(mlgnut+gui_SRCS)) 
mlgnut+gui_MLY := $(filter %.mly, $(mlgnut+gui_SRCS)) 
mlgnut+gui_ML4 := $(filter %.ml4, $(mlgnut+gui_SRCS)) 
mlgnut+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlgnut+gui_SRCS)) 
mlgnut+gui_C := $(filter %.c, $(mlgnut+gui_SRCS)) 
mlgnut+gui_CMOS=$(foreach file, $(mlgnut+gui_ML),   $(basename $(file)).cmo) 
mlgnut+gui_CMXS=$(foreach file, $(mlgnut+gui_ML),   $(basename $(file)).cmx) 
mlgnut+gui_OBJS=$(foreach file, $(mlgnut+gui_C),   $(basename $(file)).o)    

mlgnut+gui_CMXAS := $(foreach file, $(mlgnut+gui_CMXA),   build/$(basename $(file)).cmxa)
mlgnut+gui_CMAS=$(foreach file, $(mlgnut+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlgnut+gui_ML4:.ml4=.ml) $(mlgnut+gui_MLL:.mll=.ml) $(mlgnut+gui_MLY:.mly=.ml) $(mlgnut+gui_MLY:.mly=.mli) $(mlgnut+gui_ZOG:.zog=.ml) 
 
mlgnut+gui: $(mlgnut+gui_OBJS) $(mlgnut+gui_CMXS) $(mlgnut+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlgnut+gui_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(mlgnut+gui_CMXAS) $(mlgnut+gui_CMXS) 
 
mlgnut+gui.byte: $(mlgnut+gui_OBJS) $(mlgnut+gui_CMOS)  $(mlgnut+gui_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlgnut+gui_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(mlgnut+gui_CMAS) $(mlgnut+gui_CMOS) 
 
mlgnut+gui.static:  $(mlgnut+gui_OBJS) $(mlgnut+gui_CMXS)  $(mlgnut+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlgnut+gui_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(mlgnut+gui_CMXAS) $(mlgnut+gui_CMXS)


mlbt+gui_ZOG := $(filter %.zog, $(mlbt+gui_SRCS)) 
mlbt+gui_MLL := $(filter %.mll, $(mlbt+gui_SRCS)) 
mlbt+gui_MLY := $(filter %.mly, $(mlbt+gui_SRCS)) 
mlbt+gui_ML4 := $(filter %.ml4, $(mlbt+gui_SRCS)) 
mlbt+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlbt+gui_SRCS)) 
mlbt+gui_C := $(filter %.c, $(mlbt+gui_SRCS)) 
mlbt+gui_CMOS=$(foreach file, $(mlbt+gui_ML),   $(basename $(file)).cmo) 
mlbt+gui_CMXS=$(foreach file, $(mlbt+gui_ML),   $(basename $(file)).cmx) 
mlbt+gui_OBJS=$(foreach file, $(mlbt+gui_C),   $(basename $(file)).o)    

mlbt+gui_CMXAS := $(foreach file, $(mlbt+gui_CMXA),   build/$(basename $(file)).cmxa)
mlbt+gui_CMAS=$(foreach file, $(mlbt+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlbt+gui_ML4:.ml4=.ml) $(mlbt+gui_MLL:.mll=.ml) $(mlbt+gui_MLY:.mly=.ml) $(mlbt+gui_MLY:.mly=.mli) $(mlbt+gui_ZOG:.zog=.ml) 
 
mlbt+gui: $(mlbt+gui_OBJS) $(mlbt+gui_CMXS) $(mlbt+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlbt+gui_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(mlbt+gui_CMXAS) $(mlbt+gui_CMXS) 
 
mlbt+gui.byte: $(mlbt+gui_OBJS) $(mlbt+gui_CMOS)  $(mlbt+gui_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlbt+gui_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(mlbt+gui_CMAS) $(mlbt+gui_CMOS) 
 
mlbt+gui.static:  $(mlbt+gui_OBJS) $(mlbt+gui_CMXS)  $(mlbt+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlbt+gui_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(mlbt+gui_CMXAS) $(mlbt+gui_CMXS)


mlslsk_ZOG := $(filter %.zog, $(mlslsk_SRCS)) 
mlslsk_MLL := $(filter %.mll, $(mlslsk_SRCS)) 
mlslsk_MLY := $(filter %.mly, $(mlslsk_SRCS)) 
mlslsk_ML4 := $(filter %.ml4, $(mlslsk_SRCS)) 
mlslsk_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlslsk_SRCS)) 
mlslsk_C := $(filter %.c, $(mlslsk_SRCS)) 
mlslsk_CMOS=$(foreach file, $(mlslsk_ML),   $(basename $(file)).cmo) 
mlslsk_CMXS=$(foreach file, $(mlslsk_ML),   $(basename $(file)).cmx) 
mlslsk_OBJS=$(foreach file, $(mlslsk_C),   $(basename $(file)).o)    

mlslsk_CMXAS := $(foreach file, $(mlslsk_CMXA),   build/$(basename $(file)).cmxa)
mlslsk_CMAS=$(foreach file, $(mlslsk_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlslsk_ML4:.ml4=.ml) $(mlslsk_MLL:.mll=.ml) $(mlslsk_MLY:.mly=.ml) $(mlslsk_MLY:.mly=.mli) $(mlslsk_ZOG:.zog=.ml) 
 
mlslsk: $(mlslsk_OBJS) $(mlslsk_CMXS) $(mlslsk_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlslsk_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(mlslsk_CMXAS) $(mlslsk_CMXS) 
 
mlslsk.byte: $(mlslsk_OBJS) $(mlslsk_CMOS)  $(mlslsk_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlslsk_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(mlslsk_CMAS) $(mlslsk_CMOS) 
 
mlslsk.static:  $(mlslsk_OBJS) $(mlslsk_CMXS)  $(mlslsk_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlslsk_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(mlslsk_CMXAS) $(mlslsk_CMXS)


mlslsk+gui_ZOG := $(filter %.zog, $(mlslsk+gui_SRCS)) 
mlslsk+gui_MLL := $(filter %.mll, $(mlslsk+gui_SRCS)) 
mlslsk+gui_MLY := $(filter %.mly, $(mlslsk+gui_SRCS)) 
mlslsk+gui_ML4 := $(filter %.ml4, $(mlslsk+gui_SRCS)) 
mlslsk+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(mlslsk+gui_SRCS)) 
mlslsk+gui_C := $(filter %.c, $(mlslsk+gui_SRCS)) 
mlslsk+gui_CMOS=$(foreach file, $(mlslsk+gui_ML),   $(basename $(file)).cmo) 
mlslsk+gui_CMXS=$(foreach file, $(mlslsk+gui_ML),   $(basename $(file)).cmx) 
mlslsk+gui_OBJS=$(foreach file, $(mlslsk+gui_C),   $(basename $(file)).o)    

mlslsk+gui_CMXAS := $(foreach file, $(mlslsk+gui_CMXA),   build/$(basename $(file)).cmxa)
mlslsk+gui_CMAS=$(foreach file, $(mlslsk+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlslsk+gui_ML4:.ml4=.ml) $(mlslsk+gui_MLL:.mll=.ml) $(mlslsk+gui_MLY:.mly=.ml) $(mlslsk+gui_MLY:.mly=.mli) $(mlslsk+gui_ZOG:.zog=.ml) 
 
mlslsk+gui: $(mlslsk+gui_OBJS) $(mlslsk+gui_CMXS) $(mlslsk+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(mlslsk+gui_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(mlslsk+gui_CMXAS) $(mlslsk+gui_CMXS) 
 
mlslsk+gui.byte: $(mlslsk+gui_OBJS) $(mlslsk+gui_CMOS)  $(mlslsk+gui_CMAS)
	$(OCAMLC) -linkall -o $@  $(mlslsk+gui_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(mlslsk+gui_CMAS) $(mlslsk+gui_CMOS) 
 
mlslsk+gui.static:  $(mlslsk+gui_OBJS) $(mlslsk+gui_CMXS)  $(mlslsk+gui_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlslsk+gui_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(mlslsk+gui_CMXAS) $(mlslsk+gui_CMXS)


MLDONKEY_IM_ZOG := $(filter %.zog, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_MLL := $(filter %.mll, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_MLY := $(filter %.mly, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_ML4 := $(filter %.ml4, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_C := $(filter %.c, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_CMOS=$(foreach file, $(MLDONKEY_IM_ML),   $(basename $(file)).cmo) 
MLDONKEY_IM_CMXS=$(foreach file, $(MLDONKEY_IM_ML),   $(basename $(file)).cmx) 
MLDONKEY_IM_OBJS=$(foreach file, $(MLDONKEY_IM_C),   $(basename $(file)).o)    

MLDONKEY_IM_CMXAS := $(foreach file, $(MLDONKEY_IM_CMXA),   build/$(basename $(file)).cmxa)
MLDONKEY_IM_CMAS=$(foreach file, $(MLDONKEY_IM_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLDONKEY_IM_ML4:.ml4=.ml) $(MLDONKEY_IM_MLL:.mll=.ml) $(MLDONKEY_IM_MLY:.mly=.ml) $(MLDONKEY_IM_MLY:.mly=.mli) $(MLDONKEY_IM_ZOG:.zog=.ml) 
 
mlim: $(MLDONKEY_IM_OBJS) $(MLDONKEY_IM_CMXS) $(MLDONKEY_IM_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLDONKEY_IM_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLDONKEY_IM_CMXAS) $(MLDONKEY_IM_CMXS) 
 
mlim.byte: $(MLDONKEY_IM_OBJS) $(MLDONKEY_IM_CMOS)  $(MLDONKEY_IM_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLDONKEY_IM_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLDONKEY_IM_CMAS) $(MLDONKEY_IM_CMOS) 
 
mlim.static:  $(MLDONKEY_IM_OBJS) $(MLDONKEY_IM_CMXS)  $(MLDONKEY_IM_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEY_IM_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(MLDONKEY_IM_CMXAS) $(MLDONKEY_IM_CMXS)


STARTER_ZOG := $(filter %.zog, $(STARTER_SRCS)) 
STARTER_MLL := $(filter %.mll, $(STARTER_SRCS)) 
STARTER_MLY := $(filter %.mly, $(STARTER_SRCS)) 
STARTER_ML4 := $(filter %.ml4, $(STARTER_SRCS)) 
STARTER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(STARTER_SRCS)) 
STARTER_C := $(filter %.c, $(STARTER_SRCS)) 
STARTER_CMOS=$(foreach file, $(STARTER_ML),   $(basename $(file)).cmo) 
STARTER_CMXS=$(foreach file, $(STARTER_ML),   $(basename $(file)).cmx) 
STARTER_OBJS=$(foreach file, $(STARTER_C),   $(basename $(file)).o)    

STARTER_CMXAS := $(foreach file, $(STARTER_CMXA),   build/$(basename $(file)).cmxa)
STARTER_CMAS=$(foreach file, $(STARTER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(STARTER_ML4:.ml4=.ml) $(STARTER_MLL:.mll=.ml) $(STARTER_MLY:.mly=.ml) $(STARTER_MLY:.mly=.mli) $(STARTER_ZOG:.zog=.ml) 
 
mlguistarter: $(STARTER_OBJS) $(STARTER_CMXS) $(STARTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(STARTER_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(STARTER_CMXAS) $(STARTER_CMXS) 
 
mlguistarter.byte: $(STARTER_OBJS) $(STARTER_CMOS)  $(STARTER_CMAS)
	$(OCAMLC) -linkall -o $@  $(STARTER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(STARTER_CMAS) $(STARTER_CMOS) 
 
mlguistarter.static:  $(STARTER_OBJS) $(STARTER_CMXS)  $(STARTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(STARTER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(STARTER_CMXAS) $(STARTER_CMXS)


MLCHAT_ZOG := $(filter %.zog, $(MLCHAT_SRCS)) 
MLCHAT_MLL := $(filter %.mll, $(MLCHAT_SRCS)) 
MLCHAT_MLY := $(filter %.mly, $(MLCHAT_SRCS)) 
MLCHAT_ML4 := $(filter %.ml4, $(MLCHAT_SRCS)) 
MLCHAT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MLCHAT_SRCS)) 
MLCHAT_C := $(filter %.c, $(MLCHAT_SRCS)) 
MLCHAT_CMOS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmo) 
MLCHAT_CMXS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmx) 
MLCHAT_OBJS=$(foreach file, $(MLCHAT_C),   $(basename $(file)).o)    

MLCHAT_CMXAS := $(foreach file, $(MLCHAT_CMXA),   build/$(basename $(file)).cmxa)
MLCHAT_CMAS=$(foreach file, $(MLCHAT_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLCHAT_ML4:.ml4=.ml) $(MLCHAT_MLL:.mll=.ml) $(MLCHAT_MLY:.mly=.ml) $(MLCHAT_MLY:.mly=.mli) $(MLCHAT_ZOG:.zog=.ml) 
 
mlchat: $(MLCHAT_OBJS) $(MLCHAT_CMXS) $(MLCHAT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLCHAT_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLCHAT_CMXAS) $(MLCHAT_CMXS) 
 
mlchat.byte: $(MLCHAT_OBJS) $(MLCHAT_CMOS)  $(MLCHAT_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLCHAT_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLCHAT_CMAS) $(MLCHAT_CMOS) 
 
mlchat.static:  $(MLCHAT_OBJS) $(MLCHAT_CMXS)  $(MLCHAT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLCHAT_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(MLCHAT_CMXAS) $(MLCHAT_CMXS)


OBSERVER_ZOG := $(filter %.zog, $(OBSERVER_SRCS)) 
OBSERVER_MLL := $(filter %.mll, $(OBSERVER_SRCS)) 
OBSERVER_MLY := $(filter %.mly, $(OBSERVER_SRCS)) 
OBSERVER_ML4 := $(filter %.ml4, $(OBSERVER_SRCS)) 
OBSERVER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(OBSERVER_SRCS)) 
OBSERVER_C := $(filter %.c, $(OBSERVER_SRCS)) 
OBSERVER_CMOS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmo) 
OBSERVER_CMXS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmx) 
OBSERVER_OBJS=$(foreach file, $(OBSERVER_C),   $(basename $(file)).o)    

OBSERVER_CMXAS := $(foreach file, $(OBSERVER_CMXA),   build/$(basename $(file)).cmxa)
OBSERVER_CMAS=$(foreach file, $(OBSERVER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(OBSERVER_ML4:.ml4=.ml) $(OBSERVER_MLL:.mll=.ml) $(OBSERVER_MLY:.mly=.ml) $(OBSERVER_MLY:.mly=.mli) $(OBSERVER_ZOG:.zog=.ml) 
 
observer: $(OBSERVER_OBJS) $(OBSERVER_CMXS) $(OBSERVER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(OBSERVER_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(OBSERVER_CMXAS) $(OBSERVER_CMXS) 
 
observer.byte: $(OBSERVER_OBJS) $(OBSERVER_CMOS)  $(OBSERVER_CMAS)
	$(OCAMLC) -linkall -o $@  $(OBSERVER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(OBSERVER_CMAS) $(OBSERVER_CMOS) 
 
observer.static:  $(OBSERVER_OBJS) $(OBSERVER_CMXS)  $(OBSERVER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(OBSERVER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(OBSERVER_CMXAS) $(OBSERVER_CMXS)


ED2K_HASH_ZOG := $(filter %.zog, $(ED2K_HASH_SRCS)) 
ED2K_HASH_MLL := $(filter %.mll, $(ED2K_HASH_SRCS)) 
ED2K_HASH_MLY := $(filter %.mly, $(ED2K_HASH_SRCS)) 
ED2K_HASH_ML4 := $(filter %.ml4, $(ED2K_HASH_SRCS)) 
ED2K_HASH_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(ED2K_HASH_SRCS)) 
ED2K_HASH_C := $(filter %.c, $(ED2K_HASH_SRCS)) 
ED2K_HASH_CMOS=$(foreach file, $(ED2K_HASH_ML),   $(basename $(file)).cmo) 
ED2K_HASH_CMXS=$(foreach file, $(ED2K_HASH_ML),   $(basename $(file)).cmx) 
ED2K_HASH_OBJS=$(foreach file, $(ED2K_HASH_C),   $(basename $(file)).o)    

ED2K_HASH_CMXAS := $(foreach file, $(ED2K_HASH_CMXA),   build/$(basename $(file)).cmxa)
ED2K_HASH_CMAS=$(foreach file, $(ED2K_HASH_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(ED2K_HASH_ML4:.ml4=.ml) $(ED2K_HASH_MLL:.mll=.ml) $(ED2K_HASH_MLY:.mly=.ml) $(ED2K_HASH_MLY:.mly=.mli) $(ED2K_HASH_ZOG:.zog=.ml) 
 
ed2k_hash: $(ED2K_HASH_OBJS) $(ED2K_HASH_CMXS) $(ED2K_HASH_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(ED2K_HASH_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(ED2K_HASH_CMXAS) $(ED2K_HASH_CMXS) 
 
ed2k_hash.byte: $(ED2K_HASH_OBJS) $(ED2K_HASH_CMOS)  $(ED2K_HASH_CMAS)
	$(OCAMLC) -linkall -o $@  $(ED2K_HASH_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(ED2K_HASH_CMAS) $(ED2K_HASH_CMOS) 
 
ed2k_hash.static:  $(ED2K_HASH_OBJS) $(ED2K_HASH_CMXS)  $(ED2K_HASH_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(ED2K_HASH_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(ED2K_HASH_CMXAS) $(ED2K_HASH_CMXS)


MAKE_TORRENT_ZOG := $(filter %.zog, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_MLL := $(filter %.mll, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_MLY := $(filter %.mly, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_ML4 := $(filter %.ml4, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_C := $(filter %.c, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_CMOS=$(foreach file, $(MAKE_TORRENT_ML),   $(basename $(file)).cmo) 
MAKE_TORRENT_CMXS=$(foreach file, $(MAKE_TORRENT_ML),   $(basename $(file)).cmx) 
MAKE_TORRENT_OBJS=$(foreach file, $(MAKE_TORRENT_C),   $(basename $(file)).o)    

MAKE_TORRENT_CMXAS := $(foreach file, $(MAKE_TORRENT_CMXA),   build/$(basename $(file)).cmxa)
MAKE_TORRENT_CMAS=$(foreach file, $(MAKE_TORRENT_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MAKE_TORRENT_ML4:.ml4=.ml) $(MAKE_TORRENT_MLL:.mll=.ml) $(MAKE_TORRENT_MLY:.mly=.ml) $(MAKE_TORRENT_MLY:.mly=.mli) $(MAKE_TORRENT_ZOG:.zog=.ml) 
 
make_torrent: $(MAKE_TORRENT_OBJS) $(MAKE_TORRENT_CMXS) $(MAKE_TORRENT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MAKE_TORRENT_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(MAKE_TORRENT_CMXAS) $(MAKE_TORRENT_CMXS) 
 
make_torrent.byte: $(MAKE_TORRENT_OBJS) $(MAKE_TORRENT_CMOS)  $(MAKE_TORRENT_CMAS)
	$(OCAMLC) -linkall -o $@  $(MAKE_TORRENT_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(MAKE_TORRENT_CMAS) $(MAKE_TORRENT_CMOS) 
 
make_torrent.static:  $(MAKE_TORRENT_OBJS) $(MAKE_TORRENT_CMXS)  $(MAKE_TORRENT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MAKE_TORRENT_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(MAKE_TORRENT_CMXAS) $(MAKE_TORRENT_CMXS)


GET_RANGE_ZOG := $(filter %.zog, $(GET_RANGE_SRCS)) 
GET_RANGE_MLL := $(filter %.mll, $(GET_RANGE_SRCS)) 
GET_RANGE_MLY := $(filter %.mly, $(GET_RANGE_SRCS)) 
GET_RANGE_ML4 := $(filter %.ml4, $(GET_RANGE_SRCS)) 
GET_RANGE_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(GET_RANGE_SRCS)) 
GET_RANGE_C := $(filter %.c, $(GET_RANGE_SRCS)) 
GET_RANGE_CMOS=$(foreach file, $(GET_RANGE_ML),   $(basename $(file)).cmo) 
GET_RANGE_CMXS=$(foreach file, $(GET_RANGE_ML),   $(basename $(file)).cmx) 
GET_RANGE_OBJS=$(foreach file, $(GET_RANGE_C),   $(basename $(file)).o)    

GET_RANGE_CMXAS := $(foreach file, $(GET_RANGE_CMXA),   build/$(basename $(file)).cmxa)
GET_RANGE_CMAS=$(foreach file, $(GET_RANGE_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(GET_RANGE_ML4:.ml4=.ml) $(GET_RANGE_MLL:.mll=.ml) $(GET_RANGE_MLY:.mly=.ml) $(GET_RANGE_MLY:.mly=.mli) $(GET_RANGE_ZOG:.zog=.ml) 
 
get_range: $(GET_RANGE_OBJS) $(GET_RANGE_CMXS) $(GET_RANGE_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(GET_RANGE_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(GET_RANGE_CMXAS) $(GET_RANGE_CMXS) 
 
get_range.byte: $(GET_RANGE_OBJS) $(GET_RANGE_CMOS)  $(GET_RANGE_CMAS)
	$(OCAMLC) -linkall -o $@  $(GET_RANGE_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(GET_RANGE_CMAS) $(GET_RANGE_CMOS) 
 
get_range.static:  $(GET_RANGE_OBJS) $(GET_RANGE_CMXS)  $(GET_RANGE_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(GET_RANGE_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(GET_RANGE_CMXAS) $(GET_RANGE_CMXS)


COPYSOURCES_ZOG := $(filter %.zog, $(COPYSOURCES_SRCS)) 
COPYSOURCES_MLL := $(filter %.mll, $(COPYSOURCES_SRCS)) 
COPYSOURCES_MLY := $(filter %.mly, $(COPYSOURCES_SRCS)) 
COPYSOURCES_ML4 := $(filter %.ml4, $(COPYSOURCES_SRCS)) 
COPYSOURCES_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(COPYSOURCES_SRCS)) 
COPYSOURCES_C := $(filter %.c, $(COPYSOURCES_SRCS)) 
COPYSOURCES_CMOS=$(foreach file, $(COPYSOURCES_ML),   $(basename $(file)).cmo) 
COPYSOURCES_CMXS=$(foreach file, $(COPYSOURCES_ML),   $(basename $(file)).cmx) 
COPYSOURCES_OBJS=$(foreach file, $(COPYSOURCES_C),   $(basename $(file)).o)    

COPYSOURCES_CMXAS := $(foreach file, $(COPYSOURCES_CMXA),   build/$(basename $(file)).cmxa)
COPYSOURCES_CMAS=$(foreach file, $(COPYSOURCES_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(COPYSOURCES_ML4:.ml4=.ml) $(COPYSOURCES_MLL:.mll=.ml) $(COPYSOURCES_MLY:.mly=.ml) $(COPYSOURCES_MLY:.mly=.mli) $(COPYSOURCES_ZOG:.zog=.ml) 
 
copysources: $(COPYSOURCES_OBJS) $(COPYSOURCES_CMXS) $(COPYSOURCES_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(COPYSOURCES_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(COPYSOURCES_CMXAS) $(COPYSOURCES_CMXS) 
 
copysources.byte: $(COPYSOURCES_OBJS) $(COPYSOURCES_CMOS)  $(COPYSOURCES_CMAS)
	$(OCAMLC) -linkall -o $@  $(COPYSOURCES_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(COPYSOURCES_CMAS) $(COPYSOURCES_CMOS) 
 
copysources.static:  $(COPYSOURCES_OBJS) $(COPYSOURCES_CMXS)  $(COPYSOURCES_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(COPYSOURCES_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(COPYSOURCES_CMXAS) $(COPYSOURCES_CMXS)


USE_TAGS_ZOG := $(filter %.zog, $(USE_TAGS_SRCS)) 
USE_TAGS_MLL := $(filter %.mll, $(USE_TAGS_SRCS)) 
USE_TAGS_MLY := $(filter %.mly, $(USE_TAGS_SRCS)) 
USE_TAGS_ML4 := $(filter %.ml4, $(USE_TAGS_SRCS)) 
USE_TAGS_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(USE_TAGS_SRCS)) 
USE_TAGS_C := $(filter %.c, $(USE_TAGS_SRCS)) 
USE_TAGS_CMOS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmo) 
USE_TAGS_CMXS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmx) 
USE_TAGS_OBJS=$(foreach file, $(USE_TAGS_C),   $(basename $(file)).o)    

USE_TAGS_CMXAS := $(foreach file, $(USE_TAGS_CMXA),   build/$(basename $(file)).cmxa)
USE_TAGS_CMAS=$(foreach file, $(USE_TAGS_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(USE_TAGS_ML4:.ml4=.ml) $(USE_TAGS_MLL:.mll=.ml) $(USE_TAGS_MLY:.mly=.ml) $(USE_TAGS_MLY:.mly=.mli) $(USE_TAGS_ZOG:.zog=.ml) 
 
use_tags: $(USE_TAGS_OBJS) $(USE_TAGS_CMXS) $(USE_TAGS_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(USE_TAGS_OBJS) $(LIBS_opt) $(LIBS_flags) $(NO_LIBS_opt) $(NO_LIBS_flags) -I build $(USE_TAGS_CMXAS) $(USE_TAGS_CMXS) 
 
use_tags.byte: $(USE_TAGS_OBJS) $(USE_TAGS_CMOS)  $(USE_TAGS_CMAS)
	$(OCAMLC) -linkall -o $@  $(USE_TAGS_OBJS) $(LIBS_byte) $(LIBS_flags)  $(NO_LIBS_byte) $(NO_LIBS_flags) -I build $(USE_TAGS_CMAS) $(USE_TAGS_CMOS) 
 
use_tags.static:  $(USE_TAGS_OBJS) $(USE_TAGS_CMXS)  $(USE_TAGS_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(USE_TAGS_OBJS) $(LIBS_opt) $(LIBS_flags)  $(NO_LIBS_flags)  $(NO_STATIC_LIBS_opt) -I build $(USE_TAGS_CMXAS) $(USE_TAGS_CMXS)


HASH_FILES_ZOG := $(filter %.zog, $(HASH_FILES_SRCS)) 
HASH_FILES_MLL := $(filter %.mll, $(HASH_FILES_SRCS)) 
HASH_FILES_MLY := $(filter %.mly, $(HASH_FILES_SRCS)) 
HASH_FILES_ML4 := $(filter %.ml4, $(HASH_FILES_SRCS)) 
HASH_FILES_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(HASH_FILES_SRCS)) 
HASH_FILES_C := $(filter %.c, $(HASH_FILES_SRCS)) 
HASH_FILES_CMOS=$(foreach file, $(HASH_FILES_ML),   $(basename $(file)).cmo) 
HASH_FILES_CMXS=$(foreach file, $(HASH_FILES_ML),   $(basename $(file)).cmx) 
HASH_FILES_OBJS=$(foreach file, $(HASH_FILES_C),   $(basename $(file)).o)    

HASH_FILES_CMXAS := $(foreach file, $(HASH_FILES_CMXA),   build/$(basename $(file)).cmxa)
HASH_FILES_CMAS=$(foreach file, $(HASH_FILES_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(HASH_FILES_ML4:.ml4=.ml) $(HASH_FILES_MLL:.mll=.ml) $(HASH_FILES_MLY:.mly=.ml) $(HASH_FILES_MLY:.mly=.mli) $(HASH_FILES_ZOG:.zog=.ml) 
 
hash_files: $(HASH_FILES_OBJS) $(HASH_FILES_CMXS) $(HASH_FILES_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(HASH_FILES_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(HASH_FILES_CMXAS) $(HASH_FILES_CMXS) 
 
hash_files.byte: $(HASH_FILES_OBJS) $(HASH_FILES_CMOS)  $(HASH_FILES_CMAS)
	$(OCAMLC) -linkall -o $@  $(HASH_FILES_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(HASH_FILES_CMAS) $(HASH_FILES_CMOS) 
 
hash_files.static:  $(HASH_FILES_OBJS) $(HASH_FILES_CMXS)  $(HASH_FILES_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(HASH_FILES_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(HASH_FILES_CMXAS) $(HASH_FILES_CMXS)


INSTALLER_ZOG := $(filter %.zog, $(INSTALLER_SRCS)) 
INSTALLER_MLL := $(filter %.mll, $(INSTALLER_SRCS)) 
INSTALLER_MLY := $(filter %.mly, $(INSTALLER_SRCS)) 
INSTALLER_ML4 := $(filter %.ml4, $(INSTALLER_SRCS)) 
INSTALLER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(INSTALLER_SRCS)) 
INSTALLER_C := $(filter %.c, $(INSTALLER_SRCS)) 
INSTALLER_CMOS=$(foreach file, $(INSTALLER_ML),   $(basename $(file)).cmo) 
INSTALLER_CMXS=$(foreach file, $(INSTALLER_ML),   $(basename $(file)).cmx) 
INSTALLER_OBJS=$(foreach file, $(INSTALLER_C),   $(basename $(file)).o)    

INSTALLER_CMXAS := $(foreach file, $(INSTALLER_CMXA),   build/$(basename $(file)).cmxa)
INSTALLER_CMAS=$(foreach file, $(INSTALLER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(INSTALLER_ML4:.ml4=.ml) $(INSTALLER_MLL:.mll=.ml) $(INSTALLER_MLY:.mly=.ml) $(INSTALLER_MLY:.mly=.mli) $(INSTALLER_ZOG:.zog=.ml) 
 
mldonkey_installer: $(INSTALLER_OBJS) $(INSTALLER_CMXS) $(INSTALLER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(INSTALLER_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(INSTALLER_CMXAS) $(INSTALLER_CMXS) 
 
mldonkey_installer.byte: $(INSTALLER_OBJS) $(INSTALLER_CMOS)  $(INSTALLER_CMAS)
	$(OCAMLC) -linkall -o $@  $(INSTALLER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(INSTALLER_CMAS) $(INSTALLER_CMOS) 
 
mldonkey_installer.static:  $(INSTALLER_OBJS) $(INSTALLER_CMXS)  $(INSTALLER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(INSTALLER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(INSTALLER_CMXAS) $(INSTALLER_CMXS)


MLPIC_ZOG := $(filter %.zog, $(MLPIC_SRCS)) 
MLPIC_MLL := $(filter %.mll, $(MLPIC_SRCS)) 
MLPIC_MLY := $(filter %.mly, $(MLPIC_SRCS)) 
MLPIC_ML4 := $(filter %.ml4, $(MLPIC_SRCS)) 
MLPIC_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(MLPIC_SRCS)) 
MLPIC_C := $(filter %.c, $(MLPIC_SRCS)) 
MLPIC_CMOS=$(foreach file, $(MLPIC_ML),   $(basename $(file)).cmo) 
MLPIC_CMXS=$(foreach file, $(MLPIC_ML),   $(basename $(file)).cmx) 
MLPIC_OBJS=$(foreach file, $(MLPIC_C),   $(basename $(file)).o)    

MLPIC_CMXAS := $(foreach file, $(MLPIC_CMXA),   build/$(basename $(file)).cmxa)
MLPIC_CMAS=$(foreach file, $(MLPIC_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLPIC_ML4:.ml4=.ml) $(MLPIC_MLL:.mll=.ml) $(MLPIC_MLY:.mly=.ml) $(MLPIC_MLY:.mly=.mli) $(MLPIC_ZOG:.zog=.ml) 
 
mlpic: $(MLPIC_OBJS) $(MLPIC_CMXS) $(MLPIC_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLPIC_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(MLPIC_CMXAS) $(MLPIC_CMXS) 
 
mlpic.byte: $(MLPIC_OBJS) $(MLPIC_CMOS)  $(MLPIC_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLPIC_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(MLPIC_CMAS) $(MLPIC_CMOS) 
 
mlpic.static:  $(MLPIC_OBJS) $(MLPIC_CMXS)  $(MLPIC_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLPIC_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(MLPIC_CMXAS) $(MLPIC_CMXS)


SPIDER_ZOG := $(filter %.zog, $(SPIDER_SRCS)) 
SPIDER_MLL := $(filter %.mll, $(SPIDER_SRCS)) 
SPIDER_MLY := $(filter %.mly, $(SPIDER_SRCS)) 
SPIDER_ML4 := $(filter %.ml4, $(SPIDER_SRCS)) 
SPIDER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(SPIDER_SRCS)) 
SPIDER_C := $(filter %.c, $(SPIDER_SRCS)) 
SPIDER_CMOS=$(foreach file, $(SPIDER_ML),   $(basename $(file)).cmo) 
SPIDER_CMXS=$(foreach file, $(SPIDER_ML),   $(basename $(file)).cmx) 
SPIDER_OBJS=$(foreach file, $(SPIDER_C),   $(basename $(file)).o)    

SPIDER_CMXAS := $(foreach file, $(SPIDER_CMXA),   build/$(basename $(file)).cmxa)
SPIDER_CMAS=$(foreach file, $(SPIDER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(SPIDER_ML4:.ml4=.ml) $(SPIDER_MLL:.mll=.ml) $(SPIDER_MLY:.mly=.ml) $(SPIDER_MLY:.mly=.mli) $(SPIDER_ZOG:.zog=.ml) 
 
mlspider: $(SPIDER_OBJS) $(SPIDER_CMXS) $(SPIDER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(SPIDER_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(SPIDER_CMXAS) $(SPIDER_CMXS) 
 
mlspider.byte: $(SPIDER_OBJS) $(SPIDER_CMOS)  $(SPIDER_CMAS)
	$(OCAMLC) -linkall -o $@  $(SPIDER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(SPIDER_CMAS) $(SPIDER_CMOS) 
 
mlspider.static:  $(SPIDER_OBJS) $(SPIDER_CMXS)  $(SPIDER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(SPIDER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(SPIDER_CMXAS) $(SPIDER_CMXS)




#######################################################################

##                      Other rules

#######################################################################


TOP_ZOG := $(filter %.zog, $(TOP_SRCS)) 
TOP_MLL := $(filter %.mll, $(TOP_SRCS)) 
TOP_MLY := $(filter %.mly, $(TOP_SRCS)) 
TOP_ML4 := $(filter %.ml4, $(TOP_SRCS)) 
TOP_ML := $(filter %.ml %.mll %.zog %.mly %.ml4, $(TOP_SRCS)) 
TOP_C := $(filter %.c, $(TOP_SRCS)) 
TOP_CMOS=$(foreach file, $(TOP_ML),   $(basename $(file)).cmo) 
TOP_CMXS=$(foreach file, $(TOP_ML),   $(basename $(file)).cmx) 
TOP_OBJS=$(foreach file, $(TOP_C),   $(basename $(file)).o)    

TOP_CMXAS :=$(foreach file, $(TOP_CMXA),   build/$(basename $(file)).cmxa)    
TOP_CMAS=$(foreach file, $(TOP_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(TOP_ML4:.ml4=.ml) $(TOP_MLL:.mll=.ml) $(TOP_MLY:.mly=.ml) $(TOP_MLY:.mly=.mli) $(TOP_ZOG:.zog=.ml) 
 
mldonkeytop: $(TOP_OBJS) $(TOP_CMOS) $(TOP_CMAS)
	ocamlmktop -linkall $(PLUGIN_FLAG) -o $@  $(TOP_OBJS) $(LIBS_byte) $(LIBS_flags) $(_LIBS_byte) $(_LIBS_flags) -I build $(TOP_CMAS) $(TOP_CMOS) 
 


#######################################################################

##                      Other rules

#######################################################################


opt:  $(TMPSOURCES) $(TARGETS)

byte:  $(TMPSOURCES) $(foreach target, $(TARGETS), $(target).byte)

static: $(PATCHED_OCAMLOPT) $(foreach target, $(TARGETS), $(target).static)

kde_applet: $(APPLET_OBJS)
	cd src/applets/kde; ./configure --prefix /usr; make
	@echo; echo
	@echo "      Go in src/applets/kde, su root, and call 'make install'"
	@echo; echo

gnome_applet: $(APPLET_OBJS)
	cd src/applets/gnome; make
	@echo; echo
	@echo "      Go in src/applets/gnome, su root, and call 'make install'"
	@echo; echo

APPLET_SRCS=\
  src/applets/api/endianess.c \
  src/applets/api/gui_protocol.c \
  src/applets/api/main.c

APPLET_OBJS += $(foreach file, $(APPLET_SRCS), $(basename $(file)).o)

text_applet: $(APPLET_OBJS)
	$(CC) -o text_applet $(APPLET_OBJS)


$(LIB)/md4_cc.o: $(LIB)/md4.c
	$(OCAMLC) -ccopt "$(CFLAGS) -O6 -o $(LIB)/md4_cc.o" -ccopt "" -c $(LIB)/md4.c

$(CDK)/heap_c.o: $(CDK)/heap_c.c
	$(OCAMLC) -ccopt "$(CFLAGS) $(MORECFLAGS) -o $(CDK)/heap_c.o" -ccopt "" -c $(CDK)/heap_c.c

$(LIB)/md4_as.o: $(LIB)/md4_$(MD4ARCH).s
	as -o $(LIB)/md4_as.o $(LIB)/md4_$(MD4ARCH).s

$(LIB)/md4_comp.o: $(LIB)/md4_$(MD4COMP).o
	cp -f $(LIB)/md4_$(MD4COMP).o $(LIB)/md4_comp.o


zogml:
	(for i in $(GUI)/gui*_base.zog ; do \
		$(CAMLP4) pa_o.cmo pa_zog.cma pr_o.cmo -impl $$i > $(GUI)/`basename $$i zog`ml ;\
	done)

#######################################################################

#                      Other rules

#######################################################################


clean: 
	rm -f *.cm? donkey_* *.byte *.cm?? $(TARGETS) *~ *.o core *.static *.a
	rm -f build/*.a build/*.cma build/*.cmxa
	rm -f *_plugin
	rm -f mldonkey mlgui
	(for i in $(SUBDIRS); do \
		rm -f  $$i/*.cm? $$i/*.o ; \
	done)

releaseclean: clean
	rm -f config/config.cache config/config.log config/config.status
	rm -f config/config.h config/Makefile.config
	rm -f tools/zoggy/*.cm?
	rm -f $(TMPSOURCES)
	rm -rf patches/build
	rm -f .depend
	rm -f config/Makefile.config
	rm -f config/Makefile.config.i386
	rm -f config/Makefile.config.i486
	rm -f config/Makefile.config.i586
	rm -f config/Makefile.config.i686
	rm -f config/mldonkey.rc
	rm -f config/config.h
	rm -f packages/rpm/*.spec
	rm -f packages/windows/mlnet.nsi
	rm -f src/utils/lib/autoconf.ml
	rm -f src/utils/lib/autoconf.ml.new
	rm -f src/utils/lib/gAutoconf.ml
	rm -f src/utils/lib/gAutoconf.ml.new
	rm -f icons/tux/*.ml_icons
	rm -f icons/tux/*.ml
	rm -f icons/kde/*.ml_icons
	rm -f icons/kde/*.ml
	rm -f icons/mldonkey/*.ml_icons
	rm -f icons/mldonkey/*.ml

distclean: releaseclean
	rm -rf patches/local

maintainerclean: distclean
	rm -f $(GUI)/gui.ml $(GUI)/gui_zog.ml 

LOCAL=patches/build

PA_ZOG_FILES=tools/zoggy/zog_types.ml tools/zoggy/zog_messages.ml tools/zoggy/zog_misc.ml tools/zoggy/pa_zog.ml

pa_zog.cma: $(PA_ZOG_FILES)
	$(OCAMLC) -I tools/zoggy -I +camlp4 -pp "$(CAMLP4) pa_o.cmo pr_dump.cmo" -a -o pa_zog.cma  $(PA_ZOG_FILES)

$(TMPSOURCES): pa_zog.cma

depend:  pa_zog.cma $(LIB)/http_lexer.ml $(TMPSOURCES) $(TMPFILES)
	$(OCAMLDEP) $(OCAMLDEP_OPTIONS) $(patsubst -I +lablgtk,,$(INCLUDES)) *.ml *.mli > .depend
	(for i in $(SUBDIRS); do \
		$(OCAMLDEP) $(OCAMLDEP_OPTIONS) $(patsubst -I +lablgtk,,$(INCLUDES)) $$i/*.ml $$i/*.mli  >> .depend; \
	done)

$(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/Makefile: patches/ocamlopt-$(REQUIRED_OCAML).tar.gz
	rm -rf $(LOCAL)/ocamlopt-$(REQUIRED_OCAML)
	mkdir -p $(LOCAL)
	cd $(LOCAL); \
	gzip -cd ../ocamlopt-$(REQUIRED_OCAML).tar.gz | tar xf -; \
	touch ocamlopt-$(REQUIRED_OCAML)/Makefile

$(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/ocamlopt: $(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/Makefile
	cd $(LOCAL)/ocamlopt-$(REQUIRED_OCAML); $(MAKE)

utils: ed2k_hash make_torrent copysources get_range
utils.byte: ed2k_hash.byte make_torrent.byte copysources.byte get_range.byte
utils.static: ed2k_hash.static make_torrent.static copysources.static get_range.static

#######################################################################

#                      Building binary distribs

#######################################################################

DISDIR=mldonkey-distrib
distrib/Readme.txt: $(GUI)/gui_messages.ml
	grep -A 1000 help_text $(GUI)/gui_messages.ml | grep -v '"' > distrib/Readme.txt


debug:
	rm -f $(CDK)/heap_c.o
	MORECFLAGS="-I patches/ocaml-3.06/ -DHEAP_DUMP" $(MAKE) $(CDK)/heap_c.o
	$(MAKE)

RELEASE_TARGETS=mlnet 

ifeq ("$(COMPILE_GUI)" , "yes")
RELEASE_TARGETS += mlgui mlnet+gui mlguistarter mlchat
endif

release.shared: opt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	for i in $(RELEASE_TARGETS); do \
	   cp -f $$i $(DISDIR)/$$i && strip  $(DISDIR)/$$i; \
	done
	mv $(DISDIR) $(DISDIR)-$(CURRENT_VERSION)
	tar cf $(DISDIR).tar $(DISDIR)-$(CURRENT_VERSION)
	mv $(DISDIR).tar mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar
	$(COMPRESS) mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar

upload.shared: release.shared
	scp mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar.$(COMPRESS_EXT) lachesis:devel/mldonkey-release/

upload.shared.rcp: release.shared
	rcp mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar.$(COMPRESS_EXT) lachesis:devel/mldonkey-release/

release.static: static opt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	for i in $(RELEASE_TARGETS); do \
	   cp $$i.static $(DISDIR)/$$i && strip  $(DISDIR)/$$i; \
	done
	mv $(DISDIR) $(DISDIR)-$(CURRENT_VERSION)
	tar cf $(DISDIR).tar $(DISDIR)-$(CURRENT_VERSION)
	mv $(DISDIR).tar mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar
	$(COMPRESS) mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar

release.mlnet.static: mlnet.static opt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	cp mlnet.static $(DISDIR)/mlnet && strip $(DISDIR)/mlnet
	mv $(DISDIR) $(DISDIR)-$(CURRENT_VERSION)
	tar cf $(DISDIR).tar $(DISDIR)-$(CURRENT_VERSION)
	mv $(DISDIR).tar mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar
	$(COMPRESS) mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar

upload.static: release.static
	scp mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar.$(COMPRESS_EXT) lachesis:devel/mldonkey-release/

upload.static.rcp: release.static
	rcp mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar.$(COMPRESS_EXT) lachesis:devel/mldonkey-release/


release.sources: 
	rm -rf **/CVS
	rm -f config/Makefile.config
	cd ..; tar zcf mldonkey-$(CURRENT_VERSION).sources.tar.gz mldonkey

upload.sources: release.sources
	scp ../mldonkey-$(CURRENT_VERSION).sources.tar.gz lachesis:devel/mldonkey-release/

distrib: $(DISDIR)

$(DISDIR):  static distrib/Readme.txt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	rm -rf $(DISDIR)/CVS
	for i in $(RELEASE_TARGETS); do \
	   cp $$i.static $(DISDIR)/$$i && strip  $(DISDIR)/$$i; \
	done
	tar cf $(DISDIR).tar $(DISDIR)
	mv $(DISDIR).tar mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar

macosx:  opt distrib/Readme.txt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	rm -rf $(DISDIR)/CVS
	for i in $(RELEASE_TARGETS); do \
	   cp $$i $(DISDIR)/$$i && strip  $(DISDIR)/$$i; \
	done
	tar cf $(DISDIR).tar $(DISDIR)
	mv $(DISDIR).tar mldonkey-$(CURRENT_VERSION).shared.ppc-MacOS-X.tar
	gzip mldonkey-$(CURRENT_VERSION).shared.ppc-MacOS-X.tar

SHADIR=mldonkey-shared

shared: $(SHADIR)

$(SHADIR):  static distrib/Readme.txt
	rm -rf mldonkey-*
	cp -R distrib $(SHADIR)
	rm -rf $(SHADIR)/CVS
	for i in $(RELEASE_TARGETS); do \
	   cp $$i.static $(SHADIR)/$$i && strip  $(SHADIR)/$$i; \
	done
	tar cf $(SHADIR).tar $(SHADIR)
	mv $(SHADIR).tar mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar

auto-release:
## i386
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i386 config/Makefile.config
	rm -f mlnet mlnet.static mlnet+gui mlnet+gui.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i686
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i686 config/Makefile.config
	rm -f  mlnet+gui mlnet+gui.static mlnet mlnet.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i586
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i586 config/Makefile.config
	rm -f  mlnet+gui mlnet+gui.static mlnet mlnet.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i486
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i486 config/Makefile.config
	rm -f  mlnet+gui mlnet+gui.static mlnet mlnet.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i486-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i486-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/

buildrpm: 
	cp -f config/Makefile.config.i386 config/Makefile.config
	$(MAKE) clean
	$(MAKE) opt
	rm -rf ../mldonkey-rpm rpm/mldonkey
	rm -f rpm/mldonkey.sources.tar.bz2
	cp -dpR . ../mldonkey-rpm
	mv ../mldonkey-rpm rpm/mldonkey
	cd rpm/mldonkey; rm -rf **/*.cm? **/*.o 
	cd rpm; tar jcf mldonkey.sources.tar.bz2 mldonkey
	rm -rf rpm/mldonkey



sourcedist: copysources
	./copysources
	cp packages/rpm/mldonkey.spec /tmp/mldonkey/
	cp packages/rpm/mldonkey.init /tmp/mldonkey/distrib/
	cp packages/rpm/mldonkey.sysconfig /tmp/mldonkey/distrib/
	cd /tmp; tar jcf /tmp/mldonkey.sources.tar.bz2 mldonkey
	cp /tmp/mldonkey.sources.tar.bz2 .

rpm: sourcedist
	$(RPMBUILD) -ta mldonkey.sources.tar.bz2


#######################################################################

##              Specific rules

#######################################################################


-include .depend

.SUFFIXES: .mli .ml .cmx .cmo .o .c .cmi .mll .mly .zog .plugindep .xpm .ml .cc .ml_icons .ml4 .mlii

.mli.cmi :
	$(OCAMLC) $(OFLAGS) $(INCLUDES) -c $<

.ml.mlii :
	rm -f $*.mli
	$(OCAMLC) -i $(OFLAGS) $(INCLUDES) -c $< > $*.mlii
	mv $*.mlii $*.mli

.ml.cmi :
	$(OCAMLC) $(OFLAGS) $(INCLUDES) -c $<

.xpm.ml_icons :
	echo "let t = [|" > $@
	grep '"' $< | sed 's/",$$/";/' | sed 's/"};$$/"/' >> $@
	echo "|]" >> $@
	echo "let mini = [|" >> $@
	grep '"' $*_mini.xpm | sed 's/",$$/";/' | sed 's/"};$$/"/' >> $@
	echo "|]" >> $@
	cp -f $@ $*_xpm.ml

.ml.cmx :
	$(OCAMLOPT) $(PLUGIN_FLAG) $(OFLAGS) $(INCLUDES) -c $<

.ml.cmo :
	$(OCAMLC) $(OFLAGS) $(INCLUDES) -c $<

.mll.ml :
	$(OCAMLLEX) $<

.mly.ml :
	$(OCAMLYACC) $<

.mly.mli:
	$(OCAMLYACC) $<

.zog.ml:
	$(CAMLP4) pa_o.cmo ./pa_zog.cma pr_o.cmo -impl $< > $@

.ml4.ml:
	$(CAMLP4) pa_o.cmo pa_op.cmo pr_o.cmo -impl $< > $@

.c.o :
	$(OCAMLC) -ccopt "-I $(OCAML_SRC)/byterun -o $*.o" -ccopt "$(CFLAGS)" -c $<

.cc.o :
	$(CXX) $(CXX_FLAGS) -o $*.o $(CFLAGS) -c $<

.cmo.byte:
	$(OCAMLC) -o $*.byte $(LIBS) $<

.cmx.opt:
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $*.opt $(OPTLIBS) $<


.plugindep:
	echo toto

src/utils/lib/sha1old_c.o: src/utils/lib/sha1old_c.h \
  src/utils/lib/sha1_c.h src/utils/lib/os_stubs.h


src/utils/lib/sha1new_c.o: src/utils/lib/sha1new_c.h \
  src/utils/lib/sha1_c.h src/utils/lib/os_stubs.h

src/daemon/common/commonHasher_c.o: src/utils/lib/sha1_c.h
src/utils/lib/stubs_c.o: src/utils/lib/sha1_c.h