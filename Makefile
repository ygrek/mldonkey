
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

DEVFLAGS= -warn-error Am 
# -dtypes

NO_LIBS_byte=
NO_LIBS_opt=
NO_STATIC_LIBS_opt=


LIBS_byte=-custom unix.cma str.cma
LIBS_opt= unix.cmxa str.cmxa

BIGARRAY_LIBS_opt=bigarray.cmxa
BIGARRAY_LIBS_byte=bigarray.cma

CRYPT_LIBS_opt=-cclib -lcrypt
CRYPT_LIBS_byte=-cclib -lcrypt


#######################################################################

##              General options

#######################################################################

ifeq ("$(USE_GTK2)", "yes")
  ICONS_CHOICE=icons/rsvg
  SRC_GUI=src/gtk2/gui
  GUI=GTK2GUI
  IM_GUI=src/gtk2/im
  CHAT_GUI=src/gtk2/chat
else
  CONFIGWIN=src/gtk/configwin
  GPATTERN=src/gtk/gpattern
  OKEY=src/gtk/okey
  IM_GUI=src/im
  CHAT_GUI=src/gtk/chat
  ifeq ("$(BUILD_NEWGUI)", "yes")
    SRC_PROGRESS=src/gtk/progress
    ICONS_CHOICE=icons/tux
    GUI=NEWGUI
    SRC_GUI=src/gtk/newgui
  else
    SRC_GUI=src/gtk/gui
    SRC_GUI2=src/gtk/gui2
    ICONS_CHOICE=icons/kde
    GUI=OLDGUI
  endif
endif

CDK=src/utils/cdk
LIB=src/utils/lib
NET=src/utils/net
RSS=src/utils/ocamlrss
XML=src/utils/xml-light

CHAT=src/daemon/chat
COMMON=src/daemon/common
DRIVER=src/daemon/driver
MP3=src/utils/mp3tagui

ifeq ("$(DEVEL)", "yes")

DONKEY_DIR=donkey_devel

else

DONKEY_DIR=donkey

endif

SRC_AUDIOGALAXY=src/networks/audio_galaxy
SRC_DONKEY=src/networks/$(DONKEY_DIR)
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
SRC_ARES=src/networks/ares

IM=src/im

SUBDIRS=$(CDK) $(CHAT) $(CHAT_GUI) $(LIB) $(RSS) $(XML) $(NET) tools \
   $(COMMON) $(DRIVER) $(MP3) src/config/$(OS_FILES) $(EXTRA_DIRS)

INCLUDES += $(foreach file, $(SUBDIRS), -I $(file))

CFLAGS:=$(CFLAGS) $(CONFIG_INCLUDES)

# use_tags$(EXE) 
TARGETS= mlnet$(EXE) 

ifeq ("$(DEVEL)", "yes")

TARGETS += mldonkey_installer$(EXE)

endif


#######################################################################

##              Objects files for "mldonkey"

#######################################################################

ifeq ("$(OS_FILES2)", "mingw")
  LIBS_flags += -ccopt "$(GTKCFLAGS) $(GTKLLIBS)" -cclib "$(GTKLFLAGS) -lws2_32 -lgdi32 -luser32 -ladvapi32 -lwsock32 -limm32 -lshell32 -lole32 resfile.o"
endif

ifeq ("$(OS_FILES2)", "cygwin")
  LIBS_flags += -ccopt "$(GTKCFLAGS) $(GTKLLIBS)" -cclib "$(GTKLFLAGS) -lws2_32 -lgdi32 -luser32 -ladvapi32 -lwsock32 -limm32 -lshell32 -lole32 resfile.o"
endif

ifeq ("$(ZLIB)" , "yes")
  LIBS_flags += -cclib -lz
#  LIBS_byte += -cclib -lz
  CDK_SRCS +=  $(CDK)/zlib.ml $(CDK)/zlibstubs.c
endif


ifeq ("$(SUPERNODES)", "yes")
  FASTTRACK_SUPERNODE=$(SRC_FASTTRACK)/fasttrackSupernode.ml
else

endif

ifeq ("$(ICONV)" , "yes")
  LIBS_flags += -cclib -liconv
#  LIBS_byte += -cclib -liconv
endif

ifeq ("$(BROOSNET)", "yes")
  BROOSNET = \
       $(SRC_BITTORRENT)/bTBrooseTypes.ml \
       $(SRC_BITTORRENT)/bTProtoBroosnet.ml \
       $(SRC_BITTORRENT)/bTBroosnet.ml
endif

CDK_SRCS += $(LIB)/autoconf.ml


CDK_SRCS+= $(LIB)/fifo.ml $(CDK)/arg2.ml $(CDK)/printf2.ml \
   $(CDK)/heap.ml $(CDK)/dprintf.ml \
  $(CDK)/printexc2.ml $(CDK)/genlex2.ml $(CDK)/sysenv.ml \
  $(CDK)/netbase.ml $(CDK)/filepath.ml $(CDK)/string2.ml \
  $(CDK)/filename2.ml $(CDK)/list2.ml $(CDK)/hashtbl2.ml \
  $(CDK)/file.ml $(CDK)/unix2.ml $(CDK)/weak2.ml \
  $(CDK)/heap_c.c $(CDK)/array2.ml $(CDK)/sort2.ml \
  $(LIB)/longarray.ml \
  \
  $(XML)/xml_types.ml \
  $(XML)/xml_parser.mly $(XML)/xml_lexer.mll $(XML)/xml_dtd.ml \
  $(XML)/xmlParser.ml $(XML)/xml.ml \
  \
  $(RSS)/rss_messages.ml $(RSS)/rss_date.ml \
  $(RSS)/rss_types.ml  $(RSS)/rss_io.ml  $(RSS)/rss.ml
  
#  $(CDK)/xmllex.mll $(CDK)/xmlyacc.mly $(CDK)/xml.ml \
#  $(LIB)/htmlgen.ml

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
  $(LIB)/int64ops.ml $(LIB)/options.ml4 $(LIB)/numset.ml  \
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
  $(NET)/ip.ml $(NET)/ip_set.ml $(NET)/mailer.ml $(NET)/base64.ml  \
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
        $(CHAT)/chat_config.ml

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
  $(COMMON)/commonIndexing.ml \
  $(COMMON)/commonHasher_c.c

COMMON_CLIENT_SRCS= \
  $(COMMON)/commonUser.ml \
  $(COMMON)/commonServer.ml \
  $(COMMON)/commonClient.ml \
  $(COMMON)/commonFile.ml \
  $(COMMON)/commonNetwork.ml \
  $(COMMON)/commonResult.ml \
  $(COMMON)/commonComplexOptions.ml \
  $(COMMON)/commonShared.ml \
  $(COMMON)/commonRoom.ml \
  $(COMMON)/commonSearch.ml \
  $(COMMON)/commonMultimedia.ml \
  $(COMMON)/commonWeb.ml \
  $(COMMON)/commonInteractive.ml \
  $(COMMON)/commonChunks.ml \
  $(COMMON)/commonSwarming.ml \
  $(COMMON)/commonSwarming1.ml \
  $(COMMON)/commonSwarming2.ml \
  $(COMMON)/commonDownloads.ml \
  $(COMMON)/commonUploads.ml \
  $(COMMON)/commonSources.ml

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
  $(SRC_DONKEY)/donkeyPandora.ml  \
  \
  $(SRC_DONKEY)/donkeyGlobals.ml \
  $(SRC_DONKEY)/donkeyProtoCom.ml  \
 \
  $(SRC_DONKEY)/donkeyComplexOptions.ml \
  $(SRC_DONKEY)/donkeySupernode.ml \
  $(SRC_DONKEY)/donkeyShare.ml \
  $(SRC_DONKEY)/donkeyReliability.ml \
  $(SRC_DONKEY)/donkeyThieves.ml \
  $(SRC_DONKEY)/donkeyChunks.ml \
  $(SRC_DONKEY)/donkeyNeighbours.ml \
  $(SRC_DONKEY)/donkeyStats.ml \
  $(SRC_DONKEY)/donkeyOneFile.ml \
  \
  $(SRC_DONKEY)/donkeyOvernet.ml \
  $(SRC_DONKEY)/donkeyProtoKademlia.ml \
  $(SRC_DONKEY)/donkeyClient.ml \
  $(SRC_DONKEY)/donkeyProtoOvernet.ml \
  $(SRC_DONKEY)/donkeyUdp.ml \
  $(SRC_DONKEY)/donkeyFiles.ml  \
  $(SRC_DONKEY)/donkeyServers.ml \
  $(SRC_DONKEY)/donkeySearch.ml \
  $(SRC_DONKEY)/donkeyInteractive.ml \
  $(SRC_DONKEY)/donkeyMain.ml

#  $(SRC_DONKEY)/donkeySourcesMisc.ml \
#  $(SRC_DONKEY)/donkeySources.ml  \


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
  $(SRC_GNUTELLA)/gnutellaNetwork.ml \
  $(SRC_GNUTELLA)/gnutellaTypes.ml \
  $(SRC_GNUTELLA)/gnutellaOptions.ml \
  $(SRC_GNUTELLA)/gnutellaGlobals.ml \
  $(SRC_GNUTELLA)/gnutellaComplexOptions.ml \
  $(SRC_GNUTELLA)/gnutellaFunctions.ml \
  $(SRC_GNUTELLA)/gnutellaProtocol.ml \
  $(SRC_GNUTELLA)/gnutellaProto.ml \
  $(SRC_GNUTELLA)/gnutellaClients.ml \
  $(SRC_GNUTELLA)/gnutellaPandora.ml \
  $(SRC_GNUTELLA)/gnutellaHandler.ml \
  $(SRC_GNUTELLA)/gnutellaRedirector.ml \
  $(SRC_GNUTELLA)/gnutellaServers.ml \
  $(SRC_GNUTELLA)/gnutellaSupernode.ml \
  $(SRC_GNUTELLA)/gnutellaInteractive.ml \
  $(SRC_GNUTELLA)/gnutellaMain.ml

# The only files specific to Gnutella2 are:
#  $(SRC_GNUTELLA2)/g2Network.ml
#  $(SRC_GNUTELLA2)/g2Proto.ml 
#  $(SRC_GNUTELLA2)/g2Handler.ml 
#  $(SRC_GNUTELLA2)/g2Redirector.ml 

GNUTELLA2_SRCS= \
  $(SRC_GNUTELLA2)/g2Network.ml \
  $(SRC_GNUTELLA2)/g2Types.mlt \
  $(SRC_GNUTELLA2)/g2Options.mlt \
  $(SRC_GNUTELLA2)/g2Globals.mlt \
  $(SRC_GNUTELLA2)/g2ComplexOptions.mlt \
  $(SRC_GNUTELLA2)/g2Functions.mlt \
  $(SRC_GNUTELLA2)/g2Protocol.mlt \
  $(SRC_GNUTELLA2)/g2Proto.ml \
  $(SRC_GNUTELLA2)/g2Clients.mlt \
  $(SRC_GNUTELLA2)/g2Pandora.ml \
  $(SRC_GNUTELLA2)/g2Handler.ml \
  $(SRC_GNUTELLA2)/g2Redirector.ml \
  $(SRC_GNUTELLA2)/g2Servers.mlt \
  $(SRC_GNUTELLA2)/g2Supernode.ml \
  $(SRC_GNUTELLA2)/g2Interactive.mlt \
  $(SRC_GNUTELLA2)/g2Main.mlt


# The only files specific to Fasttrack are:
#  $(SRC_FASTTRACK)/fasttrackNetwork.ml 
#  $(SRC_FASTTRACK)/fasttrackGlobals.ml 
#  $(SRC_FASTTRACK)/fasttrackProtocol.ml 
#  $(SRC_FASTTRACK)/fasttrackProto.ml 
#  $(SRC_FASTTRACK)/fasttrackHandler.ml 
#  $(SRC_FASTTRACK)/fasttrackServers.ml 
#  $(SRC_FASTTRACK)/fasttrackPandora.ml 


FASTTRACK_SRCS= \
  $(SRC_FASTTRACK)/enc_type_1.c \
  $(SRC_FASTTRACK)/enc_type_2.c \
  $(SRC_FASTTRACK)/enc_type_20.c \
  $(SRC_FASTTRACK)/enc_type_80.c \
  $(SRC_FASTTRACK)/fst_crypt.c \
  $(SRC_FASTTRACK)/fst_crypt_ml.c \
  $(SRC_FASTTRACK)/fasttrackNetwork.ml \
  $(SRC_FASTTRACK)/fasttrackTypes.mlt \
  $(SRC_FASTTRACK)/fasttrackOptions.mlt \
  $(SRC_FASTTRACK)/fasttrackGlobals.ml \
  $(SRC_FASTTRACK)/fasttrackComplexOptions.mlt \
  $(SRC_FASTTRACK)/fasttrackFunctions.mlt \
  $(SRC_FASTTRACK)/fasttrackProtocol.ml \
  $(SRC_FASTTRACK)/fasttrackProto.ml \
  $(SRC_FASTTRACK)/fasttrackClients.mlt \
  $(SRC_FASTTRACK)/fasttrackHandler.ml \
  $(SRC_FASTTRACK)/fasttrackServers.ml \
  $(FASTTRACK_SUPERNODE) \
  $(SRC_FASTTRACK)/fasttrackPandora.ml \
  $(SRC_FASTTRACK)/fasttrackInteractive.mlt \
  $(SRC_FASTTRACK)/fasttrackMain.mlt

BITTORRENT_SRCS= \
  $(SRC_BITTORRENT)/bencode.ml \
  $(SRC_BITTORRENT)/bTRate.ml \
  $(SRC_BITTORRENT)/bTTypes.ml \
  $(SRC_BITTORRENT)/bTOptions.ml \
  $(SRC_BITTORRENT)/bTProtocol.ml \
  $(SRC_BITTORRENT)/bTTorrent.ml \
  $(SRC_BITTORRENT)/bTGlobals.ml \
  $(SRC_BITTORRENT)/bTComplexOptions.ml \
  $(SRC_BITTORRENT)/bTTracker.ml \
  $(SRC_BITTORRENT)/bTChooser.ml \
  $(SRC_BITTORRENT)/bTClients.ml \
  $(SRC_BITTORRENT)/bTInteractive.ml \
  $(BROOSNET) \
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

ARES_SRCS= \
  $(SRC_ARES)/aresTypes.ml \
  $(SRC_ARES)/aresOptions.ml \
  $(SRC_ARES)/aresGlobals.ml \
  $(SRC_ARES)/aresComplexOptions.ml \
  $(SRC_ARES)/aresProtocol.ml \
  $(SRC_ARES)/aresProto.ml \
  $(SRC_ARES)/aresClients.ml \
  $(SRC_ARES)/aresHandler.ml \
  $(SRC_ARES)/aresServers.ml \
  $(SRC_ARES)/aresPandora.ml \
  $(SRC_ARES)/aresInteractive.ml \
  $(SRC_ARES)/aresMain.ml

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


OBSERVER_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(COMMON_CLIENT_SRCS) $(DONKEY_SRCS) \
  tools/observer.ml

ED2K_HASH_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  tools/ed2k_hash.ml

OCAMLPP_SRCS = \
  tools/ocamlpp.ml4

COPYSOURCES_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) tools/copysources.ml

SUBCONV_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) tools/subconv.ml

MLSPLIT_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) tools/mlsplit.ml

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
  $(DRIVER)/driverLink.ml \
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

#              Sources for other development tools

#######################################################################


CONTESTER_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  src/contest/pass.c src/contest/contester.ml

SAFEEXEC_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  src/contest/pass.c src/contest/safeexec.ml

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

ifeq ("$(TMK)" , "yes")
TMK_SRCS= \
  src/spider/tmk/ml_curses.c \
  src/spider/tmk/curses.mlcpp \
  \
  src/spider/tmk/tmkStruct.ml \
  src/spider/tmk/tmkArea.ml \
  src/spider/tmk/tmkStyle.ml \
  src/spider/tmk/tmkStyle_p.mly \
  src/spider/tmk/tmkStyle_l.mll \
  src/spider/tmk/tmkSignal.ml \
  src/spider/tmk/tmkTerminal.ml \
  src/spider/tmk/tmkMain.ml \
  src/spider/tmk/tmkWidget.ml \
  src/spider/tmk/tmkContainer.ml \
  src/spider/tmk/tmkPacking.ml \
  src/spider/tmk/tmkMisc.ml \
  src/spider/tmk/tmkButton.ml \
  src/spider/tmk/tmkList.ml \
  src/spider/tmk/tmkEntry.ml \
  src/spider/tmk/tmkFrame.ml \

DISASM_SRCS= \
  $(TMK_SRCS) \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  src/spider/disasm/ansi.c \
  src/spider/disasm/asmserv.c \
  src/spider/disasm/assembl.c \
  src/spider/disasm/disasm.c \
  src/spider/disasm/mldisasm_c.c \
  src/spider/disasm/mldisasm.ml \
  src/spider/disasm/aSMmain.ml \
  

endif


SPIDER_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) $(CHAT_SRCS) $(COMMON_SRCS) $(COMMON_CLIENT_SRCS) \
  \
  src/spider/ecrawler/spiderTypes.ml \
  src/spider/ecrawler/spiderOptions.ml \
  $(SRC_DONKEY)/donkeyTypes.ml \
  $(SRC_DONKEY)/donkeyMftp.ml \
  $(SRC_DONKEY)/donkeyImport.ml \
  $(SRC_DONKEY)/donkeyOpenProtocol.ml \
  $(SRC_DONKEY)/donkeyProtoClient.ml \
  $(SRC_DONKEY)/donkeyProtoServer.ml  \
  $(SRC_DONKEY)/donkeyProtoUdp.ml  \
  \
  src/spider/ecrawler/spiderGlobals.ml \
  src/spider/ecrawler/spiderArgs.ml \
  src/spider/ecrawler/spiderProtoCom.ml  \
 \
  src/spider/ecrawler/spiderComplexOptions.ml \
  src/spider/ecrawler/spiderSupernode.ml \
  src/spider/ecrawler/spiderIndexer.ml \
  src/spider/ecrawler/spiderShare.ml \
  src/spider/ecrawler/spiderReliability.ml \
  src/spider/ecrawler/spiderOneFile.ml \
  src/spider/ecrawler/spiderClient.ml \
  \
  src/spider/ecrawler/spiderTrap.ml \
  src/spider/ecrawler/spiderOvernet.ml \
  src/spider/ecrawler/spiderServers.ml \
  src/spider/ecrawler/spiderSearch.ml \
  src/spider/ecrawler/spiderInteractive.ml \
  \
  src/spider/ecrawler/spiderMain.ml \
  src/daemon/common/commonMain.ml

TESTRSS_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) tools/testrss.ml

CLUSTER_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  src/cluster/clusterMain.ml

ANALYSER1_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) $(CHAT_SRCS) $(COMMON_SRCS) $(COMMON_CLIENT_SRCS) \
  \
  src/spider/ecrawler/spiderTypes.ml \
  src/spider/ecrawler/spiderOptions.ml \
  $(SRC_DONKEY)/donkeyTypes.ml \
  $(SRC_DONKEY)/donkeyMftp.ml \
  $(SRC_DONKEY)/donkeyImport.ml \
  $(SRC_DONKEY)/donkeyOpenProtocol.ml \
  $(SRC_DONKEY)/donkeyProtoClient.ml \
  $(SRC_DONKEY)/donkeyProtoServer.ml  \
  $(SRC_DONKEY)/donkeyProtoUdp.ml  \
  \
  src/spider/ecrawler/spiderGlobals.ml \
  src/spider/ecrawler/spiderArgs.ml \
  src/spider/ecrawler/spiderProtoCom.ml  \
 \
  src/spider/ecrawler/spiderComplexOptions.ml \
  src/spider/ecrawler/spiderSupernode.ml \
  src/spider/ecrawler/spiderIndexer.ml \
  src/spider/ecrawler/spiderShare.ml \
  src/spider/ecrawler/spiderReliability.ml \
  src/spider/ecrawler/spiderOneFile.ml \
  \
  src/spider/analyser1/spiderDependencies.ml \
  src/spider/analyser1/spiderUtils.ml \
  src/spider/analyser1/spiderFiles.ml \
  src/spider/analyser1/spiderGenTables.ml \
  src/spider/analyser1/spiderTables.ml \
  src/spider/analyser1/spiderChunks.ml \
  src/spider/analyser1/spiderCommands.ml \
  src/spider/analyser1/spiderClustering.ml \
  src/spider/analyser1/spiderCorruption.ml \
  src/spider/analyser1/spiderRoutes.ml \
  src/spider/analyser1/spiderStats.ml \
  \
  src/spider/analyser1/analyser1Main.ml \
  src/daemon/common/commonMain.ml

DP500_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  \
  $(COMMON)/commonTypes.ml \
  $(COMMON)/commonOptions.ml \
  $(COMMON)/commonMessages.ml \
  $(COMMON)/commonGlobals.ml \
  \
  src/daemon/driver/driverLink.ml \
  tools/dp500.ml \
  \
  src/daemon/common/commonMain.ml

BTVIEW_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  \
  $(COMMON)/commonTypes.ml \
  $(COMMON)/commonOptions.ml \
  $(COMMON)/commonMessages.ml \
  $(COMMON)/commonGlobals.ml \
  $(COMMON)/commonWeb.ml \
  $(COMMON)/commonHasher.ml \
  $(COMMON)/commonSwarming.ml \
  $(COMMON)/commonHasher_c.c \
  \
  $(SRC_BITTORRENT)/bencode.ml \
  $(SRC_BITTORRENT)/bTProtocol.ml \
  src/btview/btviewTypes.ml \
  src/btview/btviewGlobals.ml \
  src/btview/btviewOptions.ml \
  src/btview/btviewClients.ml \
  src/btview/btviewInteractive.ml \
  \
  src/btview/btviewMain.ml \
  src/daemon/common/commonMain.ml


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


#######################################################################

#              Objects files for "mlgui"

#######################################################################

uninstall::
	rm -f $(BINDIR)/mlnet
	rm -f $(BINDIR)/mlgui

install:: opt 
	mkdir -p $(prefix)/bin
	if test -f mlnet; then \
             rm -f $(prefix)/bin/mlnet; cp -f mlnet $(prefix)/bin/mlnet; \
             for link in mlslsk mldonkey mlgnut mldc mlbt; do \
               rm -f $(prefix)/bin/$$link; ln -s mlnet $(prefix)/bin/$$link; \
             done; \
         fi
	if test -f mlgui; then \
             rm -f $(prefix)/bin/mlgui; cp -f mlgui $(prefix)/bin/mlgui; \
             rm -f $(prefix)/bin/mldonkey_gui; ln -s mlgui $(prefix)/bin/mldonkey_gui; \
         fi
	if test -f mlnet+gui; then \
             rm -f $(prefix)/bin/mlnet+gui; cp -f mlnet+gui $(prefix)/bin/mlnet+gui; \
             for link in mlslsk+gui mldonkey+gui mlgnut+gui mldc+gui mlbt+gui; do \
               rm -f $(prefix)/bin/$$link; ln -s mlnet+gui $(prefix)/bin/$$link; \
             done; \
         fi
	if test -f mlim; then \
             rm -f $(prefix)/bin/mlim; cp -f mlim $(prefix)/bin/mlim; \
         fi

ifeq ("$(COMPILE_GUI)" , "yes")
  ifeq ("$(USE_GTK2)", "yes")
    SUBDIRS += $(SRC_GUI) $(SRC_GUI)/win32 $(ICONS_CHOICE) +lablgtk2
    GTK_LIBS_byte=-I +lablgtk2 $(LABLGL_CMA) lablgtk.cma gtkInit.cmo lablrsvg.cma
    GTK_LIBS_opt=-I +lablgtk2 $(LABLGL_CMXA) lablgtk.cmxa gtkInit.cmx lablrsvg.cmxa
    GTK_STATIC_LIBS_opt=-I +lablgtk2 lablgtk.cmxa gtkInit.cmx lablrsvg.cmxa
  else
    SUBDIRS += $(SRC_GUI) $(SRC_GUI2) $(CONFIGWIN) $(OKEY) $(GPATTERN) $(ICONS_CHOICE) +lablgtk $(SRC_PROGRESS)
    GTK_LIBS_byte=-I +lablgtk $(LABLGL_CMA) lablgtk.cma
    GTK_LIBS_opt=-I +lablgtk  $(LABLGL_CMXA) lablgtk.cmxa
    GTK_STATIC_LIBS_opt=-I +lablgtk lablgtk.cmxa
  endif

SVG_CONVERTER_SRCS = \
  $(CDK_SRCS) tools/svg_converter.ml

CURSES_LIBS_byte=-cclib -lncurses
CURSES_LIBS_opt=-cclib -lncurses


CONFIGWIN_SRCS= \
  $(CONFIGWIN)/configwin_types.ml \
  $(CONFIGWIN)/configwin_messages.ml \
  $(CONFIGWIN)/configwin_ihm.ml \
  $(CONFIGWIN)/configwin.ml

MP3TAGUI_SRCS=  $(MP3)/mp3_messages.ml $(MP3)/mp3_ui.ml

GPATTERN_SRCS=  $(LIB)/gAutoconf.ml $(GPATTERN)/gpattern.ml

OKEY_SRCS= $(OKEY)/okey.ml

GTK2GUI_ICONS= \
  $(ICONS_CHOICE)/splash_screen.svg \
  $(ICONS_CHOICE)/menu_networks.svg \
  $(ICONS_CHOICE)/menu_servers.svg \
  $(ICONS_CHOICE)/menu_downloads.svg \
  $(ICONS_CHOICE)/menu_friends.svg \
  $(ICONS_CHOICE)/menu_searches.svg \
  $(ICONS_CHOICE)/menu_rooms.svg \
  $(ICONS_CHOICE)/menu_uploads.svg \
  $(ICONS_CHOICE)/menu_console.svg \
  $(ICONS_CHOICE)/menu_graph.svg \
  $(ICONS_CHOICE)/menu_im.svg \
  $(ICONS_CHOICE)/menu_settings.svg \
  $(ICONS_CHOICE)/menu_quit.svg \
  $(ICONS_CHOICE)/menu_help.svg \
  $(ICONS_CHOICE)/menu_core.svg \
  $(ICONS_CHOICE)/menu_core_reconnect.svg \
  $(ICONS_CHOICE)/menu_core_connectto.svg \
  $(ICONS_CHOICE)/menu_core_scanports.svg \
  $(ICONS_CHOICE)/menu_core_disconnect.svg \
  $(ICONS_CHOICE)/menu_core_kill.svg \
  $(ICONS_CHOICE)/menu_search_album.svg \
  $(ICONS_CHOICE)/menu_search_movie.svg \
  $(ICONS_CHOICE)/menu_search_mp3.svg \
  $(ICONS_CHOICE)/menu_search_complex.svg \
  $(ICONS_CHOICE)/menu_search_freedb.svg \
  $(ICONS_CHOICE)/menu_search_imdb.svg \
  $(ICONS_CHOICE)/menu_mlchat.svg \
  $(ICONS_CHOICE)/menu_interfaces.svg \
  $(ICONS_CHOICE)/menu_tools.svg \
  $(ICONS_CHOICE)/menu_others.svg \
  $(ICONS_CHOICE)/net_bittorrent.svg \
  $(ICONS_CHOICE)/net_dc.svg \
  $(ICONS_CHOICE)/net_ed2k.svg \
  $(ICONS_CHOICE)/net_fasttrack.svg \
  $(ICONS_CHOICE)/net_filetp.svg \
  $(ICONS_CHOICE)/net_gnutella1.svg \
  $(ICONS_CHOICE)/net_gnutella2.svg \
  $(ICONS_CHOICE)/net_napster.svg \
  $(ICONS_CHOICE)/net_soulseek.svg \
  $(ICONS_CHOICE)/net_multinet.svg \
  $(ICONS_CHOICE)/net_globalshare.svg \
  $(ICONS_CHOICE)/net_supernode.svg \
  $(ICONS_CHOICE)/stock_shared_directory.svg \
  $(ICONS_CHOICE)/stock_directory.svg \
  $(ICONS_CHOICE)/stock_directory_open.svg \
  $(ICONS_CHOICE)/stock_color.svg \
  $(ICONS_CHOICE)/stock_font.svg \
  $(ICONS_CHOICE)/stock_password.svg \
  $(ICONS_CHOICE)/stock_download_directory.svg \
  $(ICONS_CHOICE)/stock_pending_slots.svg \
  $(ICONS_CHOICE)/stock_close.svg \
  $(ICONS_CHOICE)/stock_close_overlay.svg \
  $(ICONS_CHOICE)/stock_stop.svg \
  $(ICONS_CHOICE)/stock_ok.svg \
  $(ICONS_CHOICE)/stock_all_servers.svg \
  $(ICONS_CHOICE)/stock_add_server.svg \
  $(ICONS_CHOICE)/stock_subscribe_search.svg \
  $(ICONS_CHOICE)/stock_submit_search.svg \
  $(ICONS_CHOICE)/stock_extend_search.svg \
  $(ICONS_CHOICE)/stock_info.svg \
  $(ICONS_CHOICE)/stock_local_search.svg \
  $(ICONS_CHOICE)/stock_warning.svg \
  $(ICONS_CHOICE)/type_source_contact.svg \
  $(ICONS_CHOICE)/type_source_friend.svg \
  $(ICONS_CHOICE)/type_source_normal.svg \
  $(ICONS_CHOICE)/state_server_conh.svg \
  $(ICONS_CHOICE)/state_server_conl.svg \
  $(ICONS_CHOICE)/state_server_init.svg \
  $(ICONS_CHOICE)/state_server_notcon.svg \
  $(ICONS_CHOICE)/state_server_unknown.svg \
  $(ICONS_CHOICE)/state_source_fileslisted.svg \
  $(ICONS_CHOICE)/state_down.svg \
  $(ICONS_CHOICE)/state_up.svg \
  $(ICONS_CHOICE)/state_updown.svg \
  $(ICONS_CHOICE)/state_notupdown.svg \
  $(ICONS_CHOICE)/mime_unknown.svg \
  $(ICONS_CHOICE)/mime_images.svg \
  $(ICONS_CHOICE)/mime_binary.svg \
  $(ICONS_CHOICE)/mime_cdimage.svg \
  $(ICONS_CHOICE)/mime_debian.svg \
  $(ICONS_CHOICE)/mime_html.svg \
  $(ICONS_CHOICE)/mime_java.svg \
  $(ICONS_CHOICE)/mime_pdf.svg \
  $(ICONS_CHOICE)/mime_postscript.svg \
  $(ICONS_CHOICE)/mime_real.svg \
  $(ICONS_CHOICE)/mime_recycled.svg \
  $(ICONS_CHOICE)/mime_rpm.svg \
  $(ICONS_CHOICE)/mime_shellscript.svg \
  $(ICONS_CHOICE)/mime_soffice.svg \
  $(ICONS_CHOICE)/mime_sound.svg \
  $(ICONS_CHOICE)/mime_source.svg \
  $(ICONS_CHOICE)/mime_spreadsheet.svg \
  $(ICONS_CHOICE)/mime_tex.svg \
  $(ICONS_CHOICE)/mime_text.svg \
  $(ICONS_CHOICE)/mime_tgz.svg \
  $(ICONS_CHOICE)/mime_video.svg \
  $(ICONS_CHOICE)/mime_wordprocessing.svg \
  $(ICONS_CHOICE)/emoticon_storm.svg \
  $(ICONS_CHOICE)/emoticon_airplane.svg \
  $(ICONS_CHOICE)/emoticon_angel.svg \
  $(ICONS_CHOICE)/emoticon_arrogant.svg \
  $(ICONS_CHOICE)/emoticon_asl.svg \
  $(ICONS_CHOICE)/emoticon_bad.svg \
  $(ICONS_CHOICE)/emoticon_baringteeth.svg \
  $(ICONS_CHOICE)/emoticon_bat.svg \
  $(ICONS_CHOICE)/emoticon_beer.svg \
  $(ICONS_CHOICE)/emoticon_bowl.svg \
  $(ICONS_CHOICE)/emoticon_boy.svg \
  $(ICONS_CHOICE)/emoticon_cake.svg \
  $(ICONS_CHOICE)/emoticon_cat.svg \
  $(ICONS_CHOICE)/emoticon_cigaret.svg \
  $(ICONS_CHOICE)/emoticon_clock.svg \
  $(ICONS_CHOICE)/emoticon_confused.svg \
  $(ICONS_CHOICE)/emoticon_cry.svg \
  $(ICONS_CHOICE)/emoticon_cup.svg \
  $(ICONS_CHOICE)/emoticon_devil.svg \
  $(ICONS_CHOICE)/emoticon_dog.svg \
  $(ICONS_CHOICE)/emoticon_dude_hug.svg \
  $(ICONS_CHOICE)/emoticon_dunno.svg \
  $(ICONS_CHOICE)/emoticon_embarrassed.svg \
  $(ICONS_CHOICE)/emoticon_envelope.svg \
  $(ICONS_CHOICE)/emoticon_eyeroll.svg \
  $(ICONS_CHOICE)/emoticon_film.svg \
  $(ICONS_CHOICE)/emoticon_girl.svg \
  $(ICONS_CHOICE)/emoticon_girl_hug.svg \
  $(ICONS_CHOICE)/emoticon_ip.svg \
  $(ICONS_CHOICE)/emoticon_kiss.svg \
  $(ICONS_CHOICE)/emoticon_lightning.svg \
  $(ICONS_CHOICE)/emoticon_love.svg \
  $(ICONS_CHOICE)/emoticon_megasmile.svg \
  $(ICONS_CHOICE)/emoticon_moon.svg \
  $(ICONS_CHOICE)/emoticon_nerd.svg \
  $(ICONS_CHOICE)/emoticon_omg.svg \
  $(ICONS_CHOICE)/emoticon_party.svg \
  $(ICONS_CHOICE)/emoticon_pizza.svg \
  $(ICONS_CHOICE)/emoticon_plate.svg \
  $(ICONS_CHOICE)/emoticon_present.svg \
  $(ICONS_CHOICE)/emoticon_rainbow.svg \
  $(ICONS_CHOICE)/emoticon_sad.svg \
  $(ICONS_CHOICE)/emoticon_sarcastic.svg \
  $(ICONS_CHOICE)/emoticon_secret.svg \
  $(ICONS_CHOICE)/emoticon_shade.svg \
  $(ICONS_CHOICE)/emoticon_sick.svg \
  $(ICONS_CHOICE)/emoticon_sleepy.svg \
  $(ICONS_CHOICE)/emoticon_sorry.svg \
  $(ICONS_CHOICE)/emoticon_sshh.svg \
  $(ICONS_CHOICE)/emoticon_sun.svg \
  $(ICONS_CHOICE)/emoticon_teeth.svg \
  $(ICONS_CHOICE)/emoticon_thumbs_down.svg \
  $(ICONS_CHOICE)/emoticon_thumbs_up.svg \
  $(ICONS_CHOICE)/emoticon_tongue.svg \
  $(ICONS_CHOICE)/emoticon_ugly.svg \
  $(ICONS_CHOICE)/emoticon_ulove.svg \
  $(ICONS_CHOICE)/emoticon_wink.svg

NEWGUI_ICONS= \
  $(ICONS_CHOICE)/extend_search.xpm \
  $(ICONS_CHOICE)/local_search.xpm \
  $(ICONS_CHOICE)/trash.xpm \
  $(ICONS_CHOICE)/subscribe_search.xpm \
  $(ICONS_CHOICE)/submit_search.xpm \
  $(ICONS_CHOICE)/close_search.xpm \
  $(ICONS_CHOICE)/stop_search.xpm \
  $(ICONS_CHOICE)/nbk_networks_on.xpm \
  $(ICONS_CHOICE)/nbk_networks_menu.xpm \
  $(ICONS_CHOICE)/nbk_servers_on.xpm \
  $(ICONS_CHOICE)/nbk_servers_menu.xpm \
  $(ICONS_CHOICE)/nbk_downloads_on.xpm \
  $(ICONS_CHOICE)/nbk_downloads_menu.xpm \
  $(ICONS_CHOICE)/nbk_friends_on.xpm \
  $(ICONS_CHOICE)/nbk_friends_menu.xpm \
  $(ICONS_CHOICE)/nbk_search_on.xpm \
  $(ICONS_CHOICE)/nbk_search_menu.xpm \
  $(ICONS_CHOICE)/nbk_rooms_on.xpm \
  $(ICONS_CHOICE)/nbk_rooms_menu.xpm \
  $(ICONS_CHOICE)/nbk_uploads_on.xpm \
  $(ICONS_CHOICE)/nbk_uploads_menu.xpm \
  $(ICONS_CHOICE)/nbk_console_on.xpm \
  $(ICONS_CHOICE)/nbk_console_menu.xpm \
  $(ICONS_CHOICE)/nbk_graphs_on.xpm \
  $(ICONS_CHOICE)/nbk_graphs_menu.xpm \
  $(ICONS_CHOICE)/about.xpm \
  $(ICONS_CHOICE)/im.xpm \
  $(ICONS_CHOICE)/settings.xpm \
  $(ICONS_CHOICE)/exit.xpm \
  $(ICONS_CHOICE)/gui.xpm \
  $(ICONS_CHOICE)/kill_core.xpm \
  $(ICONS_CHOICE)/splash_screen.xpm \
  $(ICONS_CHOICE)/album_search.xpm \
  $(ICONS_CHOICE)/movie_search.xpm \
  $(ICONS_CHOICE)/mp3_search.xpm \
  $(ICONS_CHOICE)/complex_search.xpm \
  $(ICONS_CHOICE)/sharereactor_search.xpm \
  $(ICONS_CHOICE)/jigle_search.xpm \
  $(ICONS_CHOICE)/freedb_search.xpm \
  $(ICONS_CHOICE)/imdb_search.xpm \
  $(ICONS_CHOICE)/bt.xpm \
  $(ICONS_CHOICE)/dc.xpm \
  $(ICONS_CHOICE)/ed2k.xpm \
  $(ICONS_CHOICE)/fasttrack.xpm \
  $(ICONS_CHOICE)/gnutella.xpm \
  $(ICONS_CHOICE)/napster.xpm \
  $(ICONS_CHOICE)/slsk.xpm \
  $(ICONS_CHOICE)/unknown.xpm \
  $(ICONS_CHOICE)/downloading.xpm \
  $(ICONS_CHOICE)/connect_y.xpm \
  $(ICONS_CHOICE)/connect_m.xpm \
  $(ICONS_CHOICE)/connect_n.xpm \
  $(ICONS_CHOICE)/removedhost.xpm \
  $(ICONS_CHOICE)/blacklistedhost.xpm \
  $(ICONS_CHOICE)/files_listed.xpm \
  $(ICONS_CHOICE)/server_c_high.xpm \
  $(ICONS_CHOICE)/server_c_low.xpm \
  $(ICONS_CHOICE)/server_ci.xpm \
  $(ICONS_CHOICE)/server_nc.xpm \
  $(ICONS_CHOICE)/toggle_display_all_servers.xpm \
  $(ICONS_CHOICE)/view_pending_slots.xpm \
  $(ICONS_CHOICE)/add_server.xpm \
  $(ICONS_CHOICE)/add_shared_directory.xpm \
  $(ICONS_CHOICE)/download_directory.xpm \
  $(ICONS_CHOICE)/friend_user.xpm \
  $(ICONS_CHOICE)/contact_user.xpm \
  $(ICONS_CHOICE)/normal_user.xpm \
  $(ICONS_CHOICE)/priority_0.xpm \
  $(ICONS_CHOICE)/priority_1.xpm \
  $(ICONS_CHOICE)/priority_2.xpm \
  $(ICONS_CHOICE)/mimetype_binary.xpm \
  $(ICONS_CHOICE)/mimetype_cdimage.xpm \
  $(ICONS_CHOICE)/mimetype_debian.xpm \
  $(ICONS_CHOICE)/mimetype_html.xpm \
  $(ICONS_CHOICE)/mimetype_images.xpm \
  $(ICONS_CHOICE)/mimetype_java.xpm \
  $(ICONS_CHOICE)/mimetype_pdf.xpm \
  $(ICONS_CHOICE)/mimetype_postscript.xpm \
  $(ICONS_CHOICE)/mimetype_real.xpm \
  $(ICONS_CHOICE)/mimetype_recycled.xpm \
  $(ICONS_CHOICE)/mimetype_rpm.xpm \
  $(ICONS_CHOICE)/mimetype_shellscript.xpm \
  $(ICONS_CHOICE)/mimetype_soffice.xpm \
  $(ICONS_CHOICE)/mimetype_sound.xpm \
  $(ICONS_CHOICE)/mimetype_source.xpm \
  $(ICONS_CHOICE)/mimetype_spreadsheet.xpm \
  $(ICONS_CHOICE)/mimetype_tex.xpm \
  $(ICONS_CHOICE)/mimetype_text.xpm \
  $(ICONS_CHOICE)/mimetype_tgz.xpm \
  $(ICONS_CHOICE)/mimetype_video.xpm \
  $(ICONS_CHOICE)/mimetype_wordprocessing.xpm \
  $(ICONS_CHOICE)/mimetype_unknown.xpm \
  $(ICONS_CHOICE)/tree_closed.xpm \
  $(ICONS_CHOICE)/tree_opened.xpm \
  $(ICONS_CHOICE)/bt_net_on.xpm \
  $(ICONS_CHOICE)/dc_net_on.xpm \
  $(ICONS_CHOICE)/ed2k_net_on.xpm \
  $(ICONS_CHOICE)/ftt_net_on.xpm \
  $(ICONS_CHOICE)/gnut_net_on.xpm \
  $(ICONS_CHOICE)/nap_net_on.xpm \
  $(ICONS_CHOICE)/slsk_net_on.xpm \
  $(ICONS_CHOICE)/mld_tux_on.xpm

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
  $(ICONS_CHOICE)/add_to_friends.xpm \
  $(ICONS_CHOICE)/cancel.xpm $(ICONS_CHOICE)/connect_more.xpm \
  $(ICONS_CHOICE)/connect.xpm $(ICONS_CHOICE)/disconnect.xpm \
  $(ICONS_CHOICE)/download.xpm \
  $(ICONS_CHOICE)/edit_mp3.xpm $(ICONS_CHOICE)/extend_search.xpm \
  $(ICONS_CHOICE)/get_format.xpm \
  $(ICONS_CHOICE)/local_search.xpm $(ICONS_CHOICE)/preview.xpm \
  $(ICONS_CHOICE)/refres.xpm \
  $(ICONS_CHOICE)/save_all.xpm $(ICONS_CHOICE)/save_as.xpm \
  $(ICONS_CHOICE)/save.xpm \
  $(ICONS_CHOICE)/trash.xpm $(ICONS_CHOICE)/verify_chunks.xpm \
  $(ICONS_CHOICE)/view_users.xpm \
  $(ICONS_CHOICE)/pause_resume.xpm \
  $(ICONS_CHOICE)/remove_all_friends.xpm 

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

ifeq ("$(USE_GTK2)", "yes")
  ALL_ICONS=$(foreach file, $(ICONS),   $(basename $(file)).ml_icons)
  ALL_ICONS_SRCS=$(foreach file, $(ICONS),   $(basename $(file))_svg.ml)
else
  ALL_ICONS=$(foreach file, $(ICONS),   $(basename $(file)).ml_icons)
  ALL_ICONS_SRCS=$(foreach file, $(ICONS),   $(basename $(file))_xpm.ml)
endif

$(ALL_ICONS_SRCS): $(ALL_ICONS)

ifeq ("$(USE_GTK2)", "yes")
  GUI_BASE_SRCS= \
    $(SRC_GUI)/guiUtf8.ml      $(SRC_GUI)/guiMessages.ml \
    $(SRC_GUI)/guiColumns.ml   $(SRC_GUI)/guiOptions.ml \
    $(SRC_GUI)/guiArt.ml       $(SRC_GUI)/guiTools.ml \
    $(SRC_GUI)/guiTypes2.ml    $(SRC_GUI)/guiTemplates.ml \
    $(SRC_GUI)/configWindow.ml
else
  GUI_BASE_SRCS= \
    $(SRC_GUI)/gui_messages.ml   $(SRC_GUI)/gui_global.ml \
    $(SRC_GUI)/gui_columns.ml    $(SRC_GUI)/gui_keys.ml \
    $(SRC_GUI)/gui_options.ml
endif

GTK2GUI_SRCS=  \
  $(SRC_GUI)/guiGlobal.ml \
  $(SRC_GUI)/guiMisc.ml \
  $(SRC_GUI)/guiCom.ml \
  $(SRC_GUI)/guiStatusBar.ml \
  $(SRC_GUI)/guiUsers.ml \
  $(SRC_GUI)/guiResults.ml \
  $(SRC_GUI)/guiInfoWindow.ml \
  $(SRC_GUI)/guiDownloads.ml \
  $(SRC_GUI)/guiServers.ml \
  $(SRC_GUI)/guiQueries.ml \
  $(SRC_GUI)/guiRooms.ml \
  $(SRC_GUI)/guiConsole.ml \
  $(SRC_GUI)/guiFriends.ml \
  $(SRC_GUI)/guiUploads.ml \
  $(SRC_GUI)/guiNetworks.ml \
  $(IM_GUI_CORE) \
  $(SRC_GUI)/guiConfig.ml \
  $(SRC_GUI)/guiWindow.ml

ifeq ("$(OS_FILES2)", "mingw")
  GTK2GUI_SRCS += $(SRC_GUI)/win32/systraystubs.c $(SRC_GUI)/win32/systray.ml
endif

GTK2GUI_SRCS += $(SRC_GUI)/guiMain.ml

NEWGUI_SRCS=  \
  $(SRC_PROGRESS)/gui_progress.ml \
  $(SRC_GUI)/gui_misc.ml \
  $(SRC_GUI)/gui_com.ml \
  $(SRC_GUI)/gui_types.ml \
  $(SRC_GUI)/gui_graph_base.ml $(SRC_GUI)/gui_graph.ml \
  $(SRC_GUI)/gui_console_base.ml $(SRC_GUI)/gui_console.ml \
  $(SRC_GUI)/gui_users_base.ml $(SRC_GUI)/gui_users.ml \
  $(SRC_GUI)/gui_results_base.ml $(SRC_GUI)/gui_results.ml \
  $(SRC_GUI)/gui_rooms_base.ml $(SRC_GUI)/gui_rooms.ml \
  $(SRC_GUI)/gui_friends_base.ml $(SRC_GUI)/gui_friends.ml \
  $(SRC_GUI)/gui_cdget_base.ml $(SRC_GUI)/gui_cdget.ml \
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

ifeq ("$(USE_GTK2)", "yes")
  MLDONKEY_IM_SRCS= $(IM_GUI_CORE) $(IM_GUI)/guiImMain.ml  $(MAIN_SRCS)
else
  MLDONKEY_IM_SRCS= $(IM_GUI_CORE) $(IM_GUI)/gui_im_main.ml  $(MAIN_SRCS)
endif

ifeq ("$(USE_GTK2)", "yes")
  STARTER_CMXA=cdk.cmxa gmisc.cmxa common.cmxa icons.cmxa guibase.cmxa
  STARTER_SRCS= $(SRC_GUI)/guiStarter.ml
else
  STARTER_CMXA=cdk.cmxa
  STARTER_SRCS= $(SRC_GUI)/gui_starter.ml
endif

INSTALLER_CMXA= cdk.cmxa gmisc.cmxa common.cmxa icons.cmxa guibase.cmxa

ifeq ("$(USE_GTK2)", "yes")
  INSTALLER_SRCS= \
    $(SRC_GUI)/gui_installer_base.ml  $(SRC_GUI)/gui_installer.ml
else
  INSTALLER_SRCS= \
    $(SRC_GUI)/gui_installer_base.zog $(SRC_GUI)/gui_installer.ml
endif

MLPROGRESS_CMXA= cdk.cmxa gmisc.cmxa common.cmxa icons.cmxa guibase.cmxa

MLPROGRESS_SRCS = \
  $(PROGRESS_SRCS) $(MAIN_SRCS)


#######################################################################

#              Objects files for "mlchat"

#######################################################################

CHAT_EXE_SRCS= \
  $(CHAT)/chat_data.ml \
  $(CHAT)/chat_args.ml

ifeq ("$(USE_GTK2)", "yes")
  CHAT_EXE_SRCS += $(CHAT_GUI)/chat_art.ml $(CHAT_GUI)/chat_configwin.ml 
endif

CHAT_EXE_SRCS += \
  $(CHAT)/chat_icons.ml \
  $(CHAT_GUI)/chat_gui_base.ml \
  $(CHAT_GUI)/chat_gui.ml \
  $(CHAT_GUI)/chat_app.ml \
  $(CHAT)/mlchat.ml \
  $(CHAT_GUI)/chat_main.ml

ifeq ("$(USE_GTK2)", "yes")
  MLCHAT_CMXA= cdk.cmxa gmisc.cmxa icons.cmxa
else
  MLCHAT_CMXA= cdk.cmxa gmisc.cmxa
endif

MLCHAT_SRCS=  $(CHAT_SRCS) $(CHAT_EXE_SRCS)


TARGETS += mlgui$(EXE) mlchat$(EXE) mlguistarter$(EXE)
ifeq ("$(BUILD_NEWGUI)", "yes")
  TARGETS += mlprogress$(EXE)
endif

TARGETS +=  mlnet+gui$(EXE)


#### IM stuff is now automatically included in the GUI

SUBDIRS += $(IM) $(IM)/yahoo $(IM)/irc $(IM_GUI)

IM_CORE += $(IM)/imTypes.ml $(IM)/imEvent.ml \
   $(IM)/imProtocol.ml $(IM)/imIdentity.ml $(IM)/imAccount.ml \
   $(IM)/imChat.ml $(IM)/imRoom.ml \
   $(IM)/imOptions.ml

IM_CORE +=  $(IM)/irc/irc.ml

ifeq ("$(USE_GTK2)", "yes")
  IM_GUI_CORE += $(IM_GUI)/guiImAccounts.ml $(IM_GUI)/guiImChat.ml \
                 $(IM_GUI)/guiImRooms.ml    $(IM_GUI)/guiIm.ml
else
  IM_GUI_CORE += $(IM_GUI)/gui_im_base.ml $(IM_GUI)/gui_im.ml
endif

TARGETS += mlim

#ifeq ("$(DEVEL)", "yes")
#  SUBDIRS += $(IM)/msn
#
#  IM_CORE +=    $(IM)/yahoo/yahoo.ml   $(IM)/msn/msn.ml
#endif

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
DIRECT_CONNECT_MLT := $(filter %.mlt, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_MLP := $(filter %.mlcpp, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_C := $(filter %.c, $(DIRECT_CONNECT_SRCS)) 
DIRECT_CONNECT_CMOS=$(foreach file, $(DIRECT_CONNECT_ML),   $(basename $(file)).cmo) 
DIRECT_CONNECT_CMXS=$(foreach file, $(DIRECT_CONNECT_ML),   $(basename $(file)).cmx) 
DIRECT_CONNECT_OBJS=$(foreach file, $(DIRECT_CONNECT_C),   $(basename $(file)).o)    

TMPSOURCES += $(DIRECT_CONNECT_ML4:.ml4=.ml) $(DIRECT_CONNECT_MLT:.mlt=.ml) $(DIRECT_CONNECT_MLP:.mlcpp=.ml) $(DIRECT_CONNECT_MLL:.mll=.ml) $(DIRECT_CONNECT_MLY:.mly=.ml) $(DIRECT_CONNECT_MLY:.mly=.mli)  $(DIRECT_CONNECT_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(DIRECT_CONNECT_ZOG:.zog=.ml) 
MLTSOURCES +=  $(DIRECT_CONNECT_MLT:.mlt=.ml)
MLPSOURCES +=  $(DIRECT_CONNECT_MLP:.mlcpp=.ml)

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
OPENNAP_MLT := $(filter %.mlt, $(OPENNAP_SRCS)) 
OPENNAP_MLP := $(filter %.mlcpp, $(OPENNAP_SRCS)) 
OPENNAP_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(OPENNAP_SRCS)) 
OPENNAP_C := $(filter %.c, $(OPENNAP_SRCS)) 
OPENNAP_CMOS=$(foreach file, $(OPENNAP_ML),   $(basename $(file)).cmo) 
OPENNAP_CMXS=$(foreach file, $(OPENNAP_ML),   $(basename $(file)).cmx) 
OPENNAP_OBJS=$(foreach file, $(OPENNAP_C),   $(basename $(file)).o)    

TMPSOURCES += $(OPENNAP_ML4:.ml4=.ml) $(OPENNAP_MLT:.mlt=.ml) $(OPENNAP_MLP:.mlcpp=.ml) $(OPENNAP_MLL:.mll=.ml) $(OPENNAP_MLY:.mly=.ml) $(OPENNAP_MLY:.mly=.mli)  $(OPENNAP_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(OPENNAP_ZOG:.zog=.ml) 
MLTSOURCES +=  $(OPENNAP_MLT:.mlt=.ml)
MLPSOURCES +=  $(OPENNAP_MLP:.mlcpp=.ml)

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
GNUTELLA_MLT := $(filter %.mlt, $(GNUTELLA_SRCS)) 
GNUTELLA_MLP := $(filter %.mlcpp, $(GNUTELLA_SRCS)) 
GNUTELLA_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(GNUTELLA_SRCS)) 
GNUTELLA_C := $(filter %.c, $(GNUTELLA_SRCS)) 
GNUTELLA_CMOS=$(foreach file, $(GNUTELLA_ML),   $(basename $(file)).cmo) 
GNUTELLA_CMXS=$(foreach file, $(GNUTELLA_ML),   $(basename $(file)).cmx) 
GNUTELLA_OBJS=$(foreach file, $(GNUTELLA_C),   $(basename $(file)).o)    

TMPSOURCES += $(GNUTELLA_ML4:.ml4=.ml) $(GNUTELLA_MLT:.mlt=.ml) $(GNUTELLA_MLP:.mlcpp=.ml) $(GNUTELLA_MLL:.mll=.ml) $(GNUTELLA_MLY:.mly=.ml) $(GNUTELLA_MLY:.mly=.mli)  $(GNUTELLA_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(GNUTELLA_ZOG:.zog=.ml) 
MLTSOURCES +=  $(GNUTELLA_MLT:.mlt=.ml)
MLPSOURCES +=  $(GNUTELLA_MLP:.mlcpp=.ml)

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
GNUTELLA2_MLT := $(filter %.mlt, $(GNUTELLA2_SRCS)) 
GNUTELLA2_MLP := $(filter %.mlcpp, $(GNUTELLA2_SRCS)) 
GNUTELLA2_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(GNUTELLA2_SRCS)) 
GNUTELLA2_C := $(filter %.c, $(GNUTELLA2_SRCS)) 
GNUTELLA2_CMOS=$(foreach file, $(GNUTELLA2_ML),   $(basename $(file)).cmo) 
GNUTELLA2_CMXS=$(foreach file, $(GNUTELLA2_ML),   $(basename $(file)).cmx) 
GNUTELLA2_OBJS=$(foreach file, $(GNUTELLA2_C),   $(basename $(file)).o)    

TMPSOURCES += $(GNUTELLA2_ML4:.ml4=.ml) $(GNUTELLA2_MLT:.mlt=.ml) $(GNUTELLA2_MLP:.mlcpp=.ml) $(GNUTELLA2_MLL:.mll=.ml) $(GNUTELLA2_MLY:.mly=.ml) $(GNUTELLA2_MLY:.mly=.mli)  $(GNUTELLA2_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(GNUTELLA2_ZOG:.zog=.ml) 
MLTSOURCES +=  $(GNUTELLA2_MLT:.mlt=.ml)
MLPSOURCES +=  $(GNUTELLA2_MLP:.mlcpp=.ml)

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
FASTTRACK_MLT := $(filter %.mlt, $(FASTTRACK_SRCS)) 
FASTTRACK_MLP := $(filter %.mlcpp, $(FASTTRACK_SRCS)) 
FASTTRACK_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(FASTTRACK_SRCS)) 
FASTTRACK_C := $(filter %.c, $(FASTTRACK_SRCS)) 
FASTTRACK_CMOS=$(foreach file, $(FASTTRACK_ML),   $(basename $(file)).cmo) 
FASTTRACK_CMXS=$(foreach file, $(FASTTRACK_ML),   $(basename $(file)).cmx) 
FASTTRACK_OBJS=$(foreach file, $(FASTTRACK_C),   $(basename $(file)).o)    

TMPSOURCES += $(FASTTRACK_ML4:.ml4=.ml) $(FASTTRACK_MLT:.mlt=.ml) $(FASTTRACK_MLP:.mlcpp=.ml) $(FASTTRACK_MLL:.mll=.ml) $(FASTTRACK_MLY:.mly=.ml) $(FASTTRACK_MLY:.mly=.mli)  $(FASTTRACK_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(FASTTRACK_ZOG:.zog=.ml) 
MLTSOURCES +=  $(FASTTRACK_MLT:.mlt=.ml)
MLPSOURCES +=  $(FASTTRACK_MLP:.mlcpp=.ml)

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
FILETP_MLT := $(filter %.mlt, $(FILETP_SRCS)) 
FILETP_MLP := $(filter %.mlcpp, $(FILETP_SRCS)) 
FILETP_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(FILETP_SRCS)) 
FILETP_C := $(filter %.c, $(FILETP_SRCS)) 
FILETP_CMOS=$(foreach file, $(FILETP_ML),   $(basename $(file)).cmo) 
FILETP_CMXS=$(foreach file, $(FILETP_ML),   $(basename $(file)).cmx) 
FILETP_OBJS=$(foreach file, $(FILETP_C),   $(basename $(file)).o)    

TMPSOURCES += $(FILETP_ML4:.ml4=.ml) $(FILETP_MLT:.mlt=.ml) $(FILETP_MLP:.mlcpp=.ml) $(FILETP_MLL:.mll=.ml) $(FILETP_MLY:.mly=.ml) $(FILETP_MLY:.mly=.mli)  $(FILETP_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(FILETP_ZOG:.zog=.ml) 
MLTSOURCES +=  $(FILETP_MLT:.mlt=.ml)
MLPSOURCES +=  $(FILETP_MLP:.mlcpp=.ml)

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
BITTORRENT_MLT := $(filter %.mlt, $(BITTORRENT_SRCS)) 
BITTORRENT_MLP := $(filter %.mlcpp, $(BITTORRENT_SRCS)) 
BITTORRENT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(BITTORRENT_SRCS)) 
BITTORRENT_C := $(filter %.c, $(BITTORRENT_SRCS)) 
BITTORRENT_CMOS=$(foreach file, $(BITTORRENT_ML),   $(basename $(file)).cmo) 
BITTORRENT_CMXS=$(foreach file, $(BITTORRENT_ML),   $(basename $(file)).cmx) 
BITTORRENT_OBJS=$(foreach file, $(BITTORRENT_C),   $(basename $(file)).o)    

TMPSOURCES += $(BITTORRENT_ML4:.ml4=.ml) $(BITTORRENT_MLT:.mlt=.ml) $(BITTORRENT_MLP:.mlcpp=.ml) $(BITTORRENT_MLL:.mll=.ml) $(BITTORRENT_MLY:.mly=.ml) $(BITTORRENT_MLY:.mly=.mli)  $(BITTORRENT_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(BITTORRENT_ZOG:.zog=.ml) 
MLTSOURCES +=  $(BITTORRENT_MLT:.mlt=.ml)
MLPSOURCES +=  $(BITTORRENT_MLP:.mlcpp=.ml)

build/mlbt.cmxa: $(BITTORRENT_OBJS) $(BITTORRENT_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(BITTORRENT_OBJS) $(LIBS_flags) $(_LIBS_flags) $(BITTORRENT_CMXS) 
 
build/mlbt.cma: $(BITTORRENT_OBJS) $(BITTORRENT_CMOS) 
	$(OCAMLC) -a -o $@  $(BITTORRENT_OBJS) $(LIBS_flags) $(_LIBS_flags) $(BITTORRENT_CMOS) 
 


mlbt+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlbt.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlbt+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(DONKEY)" , "yes")
SUBDIRS += src/networks/$(DONKEY_DIR)

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
DONKEY_MLT := $(filter %.mlt, $(DONKEY_SRCS)) 
DONKEY_MLP := $(filter %.mlcpp, $(DONKEY_SRCS)) 
DONKEY_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(DONKEY_SRCS)) 
DONKEY_C := $(filter %.c, $(DONKEY_SRCS)) 
DONKEY_CMOS=$(foreach file, $(DONKEY_ML),   $(basename $(file)).cmo) 
DONKEY_CMXS=$(foreach file, $(DONKEY_ML),   $(basename $(file)).cmx) 
DONKEY_OBJS=$(foreach file, $(DONKEY_C),   $(basename $(file)).o)    

TMPSOURCES += $(DONKEY_ML4:.ml4=.ml) $(DONKEY_MLT:.mlt=.ml) $(DONKEY_MLP:.mlcpp=.ml) $(DONKEY_MLL:.mll=.ml) $(DONKEY_MLY:.mly=.ml) $(DONKEY_MLY:.mly=.mli)  $(DONKEY_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(DONKEY_ZOG:.zog=.ml) 
MLTSOURCES +=  $(DONKEY_MLT:.mlt=.ml)
MLPSOURCES +=  $(DONKEY_MLP:.mlcpp=.ml)

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
CYMES_MLT := $(filter %.mlt, $(CYMES_SRCS)) 
CYMES_MLP := $(filter %.mlcpp, $(CYMES_SRCS)) 
CYMES_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(CYMES_SRCS)) 
CYMES_C := $(filter %.c, $(CYMES_SRCS)) 
CYMES_CMOS=$(foreach file, $(CYMES_ML),   $(basename $(file)).cmo) 
CYMES_CMXS=$(foreach file, $(CYMES_ML),   $(basename $(file)).cmx) 
CYMES_OBJS=$(foreach file, $(CYMES_C),   $(basename $(file)).o)    

TMPSOURCES += $(CYMES_ML4:.ml4=.ml) $(CYMES_MLT:.mlt=.ml) $(CYMES_MLP:.mlcpp=.ml) $(CYMES_MLL:.mll=.ml) $(CYMES_MLY:.mly=.ml) $(CYMES_MLY:.mly=.mli)  $(CYMES_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(CYMES_ZOG:.zog=.ml) 
MLTSOURCES +=  $(CYMES_MLT:.mlt=.ml)
MLPSOURCES +=  $(CYMES_MLP:.mlcpp=.ml)

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
SOULSEEK_MLT := $(filter %.mlt, $(SOULSEEK_SRCS)) 
SOULSEEK_MLP := $(filter %.mlcpp, $(SOULSEEK_SRCS)) 
SOULSEEK_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(SOULSEEK_SRCS)) 
SOULSEEK_C := $(filter %.c, $(SOULSEEK_SRCS)) 
SOULSEEK_CMOS=$(foreach file, $(SOULSEEK_ML),   $(basename $(file)).cmo) 
SOULSEEK_CMXS=$(foreach file, $(SOULSEEK_ML),   $(basename $(file)).cmx) 
SOULSEEK_OBJS=$(foreach file, $(SOULSEEK_C),   $(basename $(file)).o)    

TMPSOURCES += $(SOULSEEK_ML4:.ml4=.ml) $(SOULSEEK_MLT:.mlt=.ml) $(SOULSEEK_MLP:.mlcpp=.ml) $(SOULSEEK_MLL:.mll=.ml) $(SOULSEEK_MLY:.mly=.ml) $(SOULSEEK_MLY:.mly=.mli)  $(SOULSEEK_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(SOULSEEK_ZOG:.zog=.ml) 
MLTSOURCES +=  $(SOULSEEK_MLT:.mlt=.ml)
MLPSOURCES +=  $(SOULSEEK_MLP:.mlcpp=.ml)

build/mlslsk.cmxa: $(SOULSEEK_OBJS) $(SOULSEEK_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(SOULSEEK_OBJS) $(LIBS_flags) $(_LIBS_flags) $(SOULSEEK_CMXS) 
 
build/mlslsk.cma: $(SOULSEEK_OBJS) $(SOULSEEK_CMOS) 
	$(OCAMLC) -a -o $@  $(SOULSEEK_OBJS) $(LIBS_flags) $(_LIBS_flags) $(SOULSEEK_CMOS) 
 


mlslsk+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlslsk.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlslsk+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(ARES)" , "yes")
SUBDIRS += src/networks/ares

CORE_SRCS += $(ARES_SRCS)

## TARGETS += mlares$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

## BUNDLE_TARGETS += mlares+gui$(EXE)

endif
endif


mlares_CMXA= cdk.cmxa common.cmxa client.cmxa mlares.cmxa driver.cmxa
mlares_SRCS= $(MAIN_SRCS)


ARES_ZOG := $(filter %.zog, $(ARES_SRCS)) 
ARES_MLL := $(filter %.mll, $(ARES_SRCS)) 
ARES_MLY := $(filter %.mly, $(ARES_SRCS)) 
ARES_ML4 := $(filter %.ml4, $(ARES_SRCS)) 
ARES_MLT := $(filter %.mlt, $(ARES_SRCS)) 
ARES_MLP := $(filter %.mlcpp, $(ARES_SRCS)) 
ARES_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(ARES_SRCS)) 
ARES_C := $(filter %.c, $(ARES_SRCS)) 
ARES_CMOS=$(foreach file, $(ARES_ML),   $(basename $(file)).cmo) 
ARES_CMXS=$(foreach file, $(ARES_ML),   $(basename $(file)).cmx) 
ARES_OBJS=$(foreach file, $(ARES_C),   $(basename $(file)).o)    

TMPSOURCES += $(ARES_ML4:.ml4=.ml) $(ARES_MLT:.mlt=.ml) $(ARES_MLP:.mlcpp=.ml) $(ARES_MLL:.mll=.ml) $(ARES_MLY:.mly=.ml) $(ARES_MLY:.mly=.mli)  $(ARES_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(ARES_ZOG:.zog=.ml) 
MLTSOURCES +=  $(ARES_MLT:.mlt=.ml)
MLPSOURCES +=  $(ARES_MLP:.mlcpp=.ml)

build/mlares.cmxa: $(ARES_OBJS) $(ARES_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(ARES_OBJS) $(LIBS_flags) $(_LIBS_flags) $(ARES_CMXS) 
 
build/mlares.cma: $(ARES_OBJS) $(ARES_CMOS) 
	$(OCAMLC) -a -o $@  $(ARES_OBJS) $(LIBS_flags) $(_LIBS_flags) $(ARES_CMOS) 
 


mlares+gui_CMXA=cdk.cmxa \
   common.cmxa client.cmxa mlares.cmxa driver.cmxa \
   gmisc.cmxa icons.cmxa guibase.cmxa gui.cmxa
mlares+gui_SRCS= $(MAIN_SRCS)



libcdk_SRCS=  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS)
libcommon_SRCS= $(CHAT_SRCS) $(COMMON_SRCS)
libclient_SRCS= $(COMMON_CLIENT_SRCS)
ifeq ("$(USE_GTK2)", "yes")
  libgmisc_SRCS=
else
  libgmisc_SRCS= $(CONFIGWIN_SRCS) $(MP3TAGUI_SRCS) $(OKEY_SRCS) $(GPATTERN_SRCS)
endif
libguibase_SRCS= $(IM_CORE) $(GUI_BASE_SRCS)
libgui_SRCS=   $(GUI_SRCS)
libgui3_SRCS=   $(GUI3_SRCS)
libicons_SRCS= $(ALL_ICONS_SRCS)


libicons_ZOG := $(filter %.zog, $(libicons_SRCS)) 
libicons_MLL := $(filter %.mll, $(libicons_SRCS)) 
libicons_MLY := $(filter %.mly, $(libicons_SRCS)) 
libicons_ML4 := $(filter %.ml4, $(libicons_SRCS)) 
libicons_MLT := $(filter %.mlt, $(libicons_SRCS)) 
libicons_MLP := $(filter %.mlcpp, $(libicons_SRCS)) 
libicons_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(libicons_SRCS)) 
libicons_C := $(filter %.c, $(libicons_SRCS)) 
libicons_CMOS=$(foreach file, $(libicons_ML),   $(basename $(file)).cmo) 
libicons_CMXS=$(foreach file, $(libicons_ML),   $(basename $(file)).cmx) 
libicons_OBJS=$(foreach file, $(libicons_C),   $(basename $(file)).o)    

TMPSOURCES += $(libicons_ML4:.ml4=.ml) $(libicons_MLT:.mlt=.ml) $(libicons_MLP:.mlcpp=.ml) $(libicons_MLL:.mll=.ml) $(libicons_MLY:.mly=.ml) $(libicons_MLY:.mly=.mli)  $(libicons_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(libicons_ZOG:.zog=.ml) 
MLTSOURCES +=  $(libicons_MLT:.mlt=.ml)
MLPSOURCES +=  $(libicons_MLP:.mlcpp=.ml)

build/icons.cmxa: $(libicons_OBJS) $(libicons_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libicons_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libicons_CMXS) 
 
build/icons.cma: $(libicons_OBJS) $(libicons_CMOS) 
	$(OCAMLC) -a -o $@  $(libicons_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libicons_CMOS) 
 


libcdk_ZOG := $(filter %.zog, $(libcdk_SRCS)) 
libcdk_MLL := $(filter %.mll, $(libcdk_SRCS)) 
libcdk_MLY := $(filter %.mly, $(libcdk_SRCS)) 
libcdk_ML4 := $(filter %.ml4, $(libcdk_SRCS)) 
libcdk_MLT := $(filter %.mlt, $(libcdk_SRCS)) 
libcdk_MLP := $(filter %.mlcpp, $(libcdk_SRCS)) 
libcdk_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(libcdk_SRCS)) 
libcdk_C := $(filter %.c, $(libcdk_SRCS)) 
libcdk_CMOS=$(foreach file, $(libcdk_ML),   $(basename $(file)).cmo) 
libcdk_CMXS=$(foreach file, $(libcdk_ML),   $(basename $(file)).cmx) 
libcdk_OBJS=$(foreach file, $(libcdk_C),   $(basename $(file)).o)    

TMPSOURCES += $(libcdk_ML4:.ml4=.ml) $(libcdk_MLT:.mlt=.ml) $(libcdk_MLP:.mlcpp=.ml) $(libcdk_MLL:.mll=.ml) $(libcdk_MLY:.mly=.ml) $(libcdk_MLY:.mly=.mli)  $(libcdk_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(libcdk_ZOG:.zog=.ml) 
MLTSOURCES +=  $(libcdk_MLT:.mlt=.ml)
MLPSOURCES +=  $(libcdk_MLP:.mlcpp=.ml)

build/cdk.cmxa: $(libcdk_OBJS) $(libcdk_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libcdk_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcdk_CMXS) 
 
build/cdk.cma: $(libcdk_OBJS) $(libcdk_CMOS) 
	$(OCAMLC) -a -o $@  $(libcdk_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcdk_CMOS) 
 


libcommon_ZOG := $(filter %.zog, $(libcommon_SRCS)) 
libcommon_MLL := $(filter %.mll, $(libcommon_SRCS)) 
libcommon_MLY := $(filter %.mly, $(libcommon_SRCS)) 
libcommon_ML4 := $(filter %.ml4, $(libcommon_SRCS)) 
libcommon_MLT := $(filter %.mlt, $(libcommon_SRCS)) 
libcommon_MLP := $(filter %.mlcpp, $(libcommon_SRCS)) 
libcommon_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(libcommon_SRCS)) 
libcommon_C := $(filter %.c, $(libcommon_SRCS)) 
libcommon_CMOS=$(foreach file, $(libcommon_ML),   $(basename $(file)).cmo) 
libcommon_CMXS=$(foreach file, $(libcommon_ML),   $(basename $(file)).cmx) 
libcommon_OBJS=$(foreach file, $(libcommon_C),   $(basename $(file)).o)    

TMPSOURCES += $(libcommon_ML4:.ml4=.ml) $(libcommon_MLT:.mlt=.ml) $(libcommon_MLP:.mlcpp=.ml) $(libcommon_MLL:.mll=.ml) $(libcommon_MLY:.mly=.ml) $(libcommon_MLY:.mly=.mli)  $(libcommon_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(libcommon_ZOG:.zog=.ml) 
MLTSOURCES +=  $(libcommon_MLT:.mlt=.ml)
MLPSOURCES +=  $(libcommon_MLP:.mlcpp=.ml)

build/common.cmxa: $(libcommon_OBJS) $(libcommon_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libcommon_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcommon_CMXS) 
 
build/common.cma: $(libcommon_OBJS) $(libcommon_CMOS) 
	$(OCAMLC) -a -o $@  $(libcommon_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libcommon_CMOS) 
 


libclient_ZOG := $(filter %.zog, $(libclient_SRCS)) 
libclient_MLL := $(filter %.mll, $(libclient_SRCS)) 
libclient_MLY := $(filter %.mly, $(libclient_SRCS)) 
libclient_ML4 := $(filter %.ml4, $(libclient_SRCS)) 
libclient_MLT := $(filter %.mlt, $(libclient_SRCS)) 
libclient_MLP := $(filter %.mlcpp, $(libclient_SRCS)) 
libclient_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(libclient_SRCS)) 
libclient_C := $(filter %.c, $(libclient_SRCS)) 
libclient_CMOS=$(foreach file, $(libclient_ML),   $(basename $(file)).cmo) 
libclient_CMXS=$(foreach file, $(libclient_ML),   $(basename $(file)).cmx) 
libclient_OBJS=$(foreach file, $(libclient_C),   $(basename $(file)).o)    

TMPSOURCES += $(libclient_ML4:.ml4=.ml) $(libclient_MLT:.mlt=.ml) $(libclient_MLP:.mlcpp=.ml) $(libclient_MLL:.mll=.ml) $(libclient_MLY:.mly=.ml) $(libclient_MLY:.mly=.mli)  $(libclient_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(libclient_ZOG:.zog=.ml) 
MLTSOURCES +=  $(libclient_MLT:.mlt=.ml)
MLPSOURCES +=  $(libclient_MLP:.mlcpp=.ml)

build/client.cmxa: $(libclient_OBJS) $(libclient_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libclient_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libclient_CMXS) 
 
build/client.cma: $(libclient_OBJS) $(libclient_CMOS) 
	$(OCAMLC) -a -o $@  $(libclient_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libclient_CMOS) 
 


DRIVER_ZOG := $(filter %.zog, $(DRIVER_SRCS)) 
DRIVER_MLL := $(filter %.mll, $(DRIVER_SRCS)) 
DRIVER_MLY := $(filter %.mly, $(DRIVER_SRCS)) 
DRIVER_ML4 := $(filter %.ml4, $(DRIVER_SRCS)) 
DRIVER_MLT := $(filter %.mlt, $(DRIVER_SRCS)) 
DRIVER_MLP := $(filter %.mlcpp, $(DRIVER_SRCS)) 
DRIVER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(DRIVER_SRCS)) 
DRIVER_C := $(filter %.c, $(DRIVER_SRCS)) 
DRIVER_CMOS=$(foreach file, $(DRIVER_ML),   $(basename $(file)).cmo) 
DRIVER_CMXS=$(foreach file, $(DRIVER_ML),   $(basename $(file)).cmx) 
DRIVER_OBJS=$(foreach file, $(DRIVER_C),   $(basename $(file)).o)    

TMPSOURCES += $(DRIVER_ML4:.ml4=.ml) $(DRIVER_MLT:.mlt=.ml) $(DRIVER_MLP:.mlcpp=.ml) $(DRIVER_MLL:.mll=.ml) $(DRIVER_MLY:.mly=.ml) $(DRIVER_MLY:.mly=.mli)  $(DRIVER_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(DRIVER_ZOG:.zog=.ml) 
MLTSOURCES +=  $(DRIVER_MLT:.mlt=.ml)
MLPSOURCES +=  $(DRIVER_MLP:.mlcpp=.ml)

build/driver.cmxa: $(DRIVER_OBJS) $(DRIVER_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(DRIVER_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DRIVER_CMXS) 
 
build/driver.cma: $(DRIVER_OBJS) $(DRIVER_CMOS) 
	$(OCAMLC) -a -o $@  $(DRIVER_OBJS) $(LIBS_flags) $(_LIBS_flags) $(DRIVER_CMOS) 
 


CORE_ZOG := $(filter %.zog, $(CORE_SRCS)) 
CORE_MLL := $(filter %.mll, $(CORE_SRCS)) 
CORE_MLY := $(filter %.mly, $(CORE_SRCS)) 
CORE_ML4 := $(filter %.ml4, $(CORE_SRCS)) 
CORE_MLT := $(filter %.mlt, $(CORE_SRCS)) 
CORE_MLP := $(filter %.mlcpp, $(CORE_SRCS)) 
CORE_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(CORE_SRCS)) 
CORE_C := $(filter %.c, $(CORE_SRCS)) 
CORE_CMOS=$(foreach file, $(CORE_ML),   $(basename $(file)).cmo) 
CORE_CMXS=$(foreach file, $(CORE_ML),   $(basename $(file)).cmx) 
CORE_OBJS=$(foreach file, $(CORE_C),   $(basename $(file)).o)    

TMPSOURCES += $(CORE_ML4:.ml4=.ml) $(CORE_MLT:.mlt=.ml) $(CORE_MLP:.mlcpp=.ml) $(CORE_MLL:.mll=.ml) $(CORE_MLY:.mly=.ml) $(CORE_MLY:.mly=.mli)  $(CORE_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(CORE_ZOG:.zog=.ml) 
MLTSOURCES +=  $(CORE_MLT:.mlt=.ml)
MLPSOURCES +=  $(CORE_MLP:.mlcpp=.ml)

build/core.cmxa: $(CORE_OBJS) $(CORE_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(CORE_OBJS) $(LIBS_flags) $(_LIBS_flags) $(CORE_CMXS) 
 
build/core.cma: $(CORE_OBJS) $(CORE_CMOS) 
	$(OCAMLC) -a -o $@  $(CORE_OBJS) $(LIBS_flags) $(_LIBS_flags) $(CORE_CMOS) 
 


libgmisc_ZOG := $(filter %.zog, $(libgmisc_SRCS)) 
libgmisc_MLL := $(filter %.mll, $(libgmisc_SRCS)) 
libgmisc_MLY := $(filter %.mly, $(libgmisc_SRCS)) 
libgmisc_ML4 := $(filter %.ml4, $(libgmisc_SRCS)) 
libgmisc_MLT := $(filter %.mlt, $(libgmisc_SRCS)) 
libgmisc_MLP := $(filter %.mlcpp, $(libgmisc_SRCS)) 
libgmisc_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(libgmisc_SRCS)) 
libgmisc_C := $(filter %.c, $(libgmisc_SRCS)) 
libgmisc_CMOS=$(foreach file, $(libgmisc_ML),   $(basename $(file)).cmo) 
libgmisc_CMXS=$(foreach file, $(libgmisc_ML),   $(basename $(file)).cmx) 
libgmisc_OBJS=$(foreach file, $(libgmisc_C),   $(basename $(file)).o)    

TMPSOURCES += $(libgmisc_ML4:.ml4=.ml) $(libgmisc_MLT:.mlt=.ml) $(libgmisc_MLP:.mlcpp=.ml) $(libgmisc_MLL:.mll=.ml) $(libgmisc_MLY:.mly=.ml) $(libgmisc_MLY:.mly=.mli)  $(libgmisc_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(libgmisc_ZOG:.zog=.ml) 
MLTSOURCES +=  $(libgmisc_MLT:.mlt=.ml)
MLPSOURCES +=  $(libgmisc_MLP:.mlcpp=.ml)

build/gmisc.cmxa: $(libgmisc_OBJS) $(libgmisc_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libgmisc_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgmisc_CMXS) 
 
build/gmisc.cma: $(libgmisc_OBJS) $(libgmisc_CMOS) 
	$(OCAMLC) -a -o $@  $(libgmisc_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgmisc_CMOS) 
 


libgui_ZOG := $(filter %.zog, $(libgui_SRCS)) 
libgui_MLL := $(filter %.mll, $(libgui_SRCS)) 
libgui_MLY := $(filter %.mly, $(libgui_SRCS)) 
libgui_ML4 := $(filter %.ml4, $(libgui_SRCS)) 
libgui_MLT := $(filter %.mlt, $(libgui_SRCS)) 
libgui_MLP := $(filter %.mlcpp, $(libgui_SRCS)) 
libgui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(libgui_SRCS)) 
libgui_C := $(filter %.c, $(libgui_SRCS)) 
libgui_CMOS=$(foreach file, $(libgui_ML),   $(basename $(file)).cmo) 
libgui_CMXS=$(foreach file, $(libgui_ML),   $(basename $(file)).cmx) 
libgui_OBJS=$(foreach file, $(libgui_C),   $(basename $(file)).o)    

TMPSOURCES += $(libgui_ML4:.ml4=.ml) $(libgui_MLT:.mlt=.ml) $(libgui_MLP:.mlcpp=.ml) $(libgui_MLL:.mll=.ml) $(libgui_MLY:.mly=.ml) $(libgui_MLY:.mly=.mli)  $(libgui_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(libgui_ZOG:.zog=.ml) 
MLTSOURCES +=  $(libgui_MLT:.mlt=.ml)
MLPSOURCES +=  $(libgui_MLP:.mlcpp=.ml)

build/gui.cmxa: $(libgui_OBJS) $(libgui_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -a -o $@  $(libgui_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgui_CMXS) 
 
build/gui.cma: $(libgui_OBJS) $(libgui_CMOS) 
	$(OCAMLC) -a -o $@  $(libgui_OBJS) $(LIBS_flags) $(_LIBS_flags) $(libgui_CMOS) 
 


libguibase_ZOG := $(filter %.zog, $(libguibase_SRCS)) 
libguibase_MLL := $(filter %.mll, $(libguibase_SRCS)) 
libguibase_MLY := $(filter %.mly, $(libguibase_SRCS)) 
libguibase_ML4 := $(filter %.ml4, $(libguibase_SRCS)) 
libguibase_MLT := $(filter %.mlt, $(libguibase_SRCS)) 
libguibase_MLP := $(filter %.mlcpp, $(libguibase_SRCS)) 
libguibase_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(libguibase_SRCS)) 
libguibase_C := $(filter %.c, $(libguibase_SRCS)) 
libguibase_CMOS=$(foreach file, $(libguibase_ML),   $(basename $(file)).cmo) 
libguibase_CMXS=$(foreach file, $(libguibase_ML),   $(basename $(file)).cmx) 
libguibase_OBJS=$(foreach file, $(libguibase_C),   $(basename $(file)).o)    

TMPSOURCES += $(libguibase_ML4:.ml4=.ml) $(libguibase_MLT:.mlt=.ml) $(libguibase_MLP:.mlcpp=.ml) $(libguibase_MLL:.mll=.ml) $(libguibase_MLY:.mly=.ml) $(libguibase_MLY:.mly=.mli)  $(libguibase_ZOG:.zog=.ml) 
 
ZOGSOURCES +=  $(libguibase_ZOG:.zog=.ml) 
MLTSOURCES +=  $(libguibase_MLT:.mlt=.ml)
MLPSOURCES +=  $(libguibase_MLP:.mlcpp=.ml)

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
mldonkey_MLT := $(filter %.mlt, $(mldonkey_SRCS)) 
mldonkey_MLP := $(filter %.mlcpp, $(mldonkey_SRCS)) 
mldonkey_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mldonkey_SRCS)) 
mldonkey_C := $(filter %.c, $(mldonkey_SRCS)) 
mldonkey_CMOS=$(foreach file, $(mldonkey_ML),   $(basename $(file)).cmo) 
mldonkey_CMXS=$(foreach file, $(mldonkey_ML),   $(basename $(file)).cmx) 
mldonkey_OBJS=$(foreach file, $(mldonkey_C),   $(basename $(file)).o)    

mldonkey_CMXAS := $(foreach file, $(mldonkey_CMXA),   build/$(basename $(file)).cmxa)
mldonkey_CMAS=$(foreach file, $(mldonkey_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldonkey_ML4:.ml4=.ml) $(mldonkey_MLT:.mlt=.ml) $(mldonkey_MLP:.mlcpp=.ml) $(mldonkey_MLL:.mll=.ml) $(mldonkey_MLY:.mly=.ml) $(mldonkey_MLY:.mly=.mli) $(mldonkey_ZOG:.zog=.ml) 
 
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
mldonkey+gui_MLT := $(filter %.mlt, $(mldonkey+gui_SRCS)) 
mldonkey+gui_MLP := $(filter %.mlcpp, $(mldonkey+gui_SRCS)) 
mldonkey+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mldonkey+gui_SRCS)) 
mldonkey+gui_C := $(filter %.c, $(mldonkey+gui_SRCS)) 
mldonkey+gui_CMOS=$(foreach file, $(mldonkey+gui_ML),   $(basename $(file)).cmo) 
mldonkey+gui_CMXS=$(foreach file, $(mldonkey+gui_ML),   $(basename $(file)).cmx) 
mldonkey+gui_OBJS=$(foreach file, $(mldonkey+gui_C),   $(basename $(file)).o)    

mldonkey+gui_CMXAS := $(foreach file, $(mldonkey+gui_CMXA),   build/$(basename $(file)).cmxa)
mldonkey+gui_CMAS=$(foreach file, $(mldonkey+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldonkey+gui_ML4:.ml4=.ml) $(mldonkey+gui_MLT:.mlt=.ml) $(mldonkey+gui_MLP:.mlcpp=.ml) $(mldonkey+gui_MLL:.mll=.ml) $(mldonkey+gui_MLY:.mly=.ml) $(mldonkey+gui_MLY:.mly=.mli) $(mldonkey+gui_ZOG:.zog=.ml) 
 
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
MLPROGRESS_MLT := $(filter %.mlt, $(MLPROGRESS_SRCS)) 
MLPROGRESS_MLP := $(filter %.mlcpp, $(MLPROGRESS_SRCS)) 
MLPROGRESS_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLPROGRESS_SRCS)) 
MLPROGRESS_C := $(filter %.c, $(MLPROGRESS_SRCS)) 
MLPROGRESS_CMOS=$(foreach file, $(MLPROGRESS_ML),   $(basename $(file)).cmo) 
MLPROGRESS_CMXS=$(foreach file, $(MLPROGRESS_ML),   $(basename $(file)).cmx) 
MLPROGRESS_OBJS=$(foreach file, $(MLPROGRESS_C),   $(basename $(file)).o)    

MLPROGRESS_CMXAS := $(foreach file, $(MLPROGRESS_CMXA),   build/$(basename $(file)).cmxa)
MLPROGRESS_CMAS=$(foreach file, $(MLPROGRESS_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLPROGRESS_ML4:.ml4=.ml) $(MLPROGRESS_MLT:.mlt=.ml) $(MLPROGRESS_MLP:.mlcpp=.ml) $(MLPROGRESS_MLL:.mll=.ml) $(MLPROGRESS_MLY:.mly=.ml) $(MLPROGRESS_MLY:.mly=.mli) $(MLPROGRESS_ZOG:.zog=.ml) 
 
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
MLDONKEYGUI_MLT := $(filter %.mlt, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_MLP := $(filter %.mlcpp, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_C := $(filter %.c, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_CMOS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmo) 
MLDONKEYGUI_CMXS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmx) 
MLDONKEYGUI_OBJS=$(foreach file, $(MLDONKEYGUI_C),   $(basename $(file)).o)    

MLDONKEYGUI_CMXAS := $(foreach file, $(MLDONKEYGUI_CMXA),   build/$(basename $(file)).cmxa)
MLDONKEYGUI_CMAS=$(foreach file, $(MLDONKEYGUI_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLDONKEYGUI_ML4:.ml4=.ml) $(MLDONKEYGUI_MLT:.mlt=.ml) $(MLDONKEYGUI_MLP:.mlcpp=.ml) $(MLDONKEYGUI_MLL:.mll=.ml) $(MLDONKEYGUI_MLY:.mly=.ml) $(MLDONKEYGUI_MLY:.mly=.mli) $(MLDONKEYGUI_ZOG:.zog=.ml) 
 
mlgui: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS) $(MLDONKEYGUI_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI_CMXAS) $(MLDONKEYGUI_CMXS) 
 
mlgui.byte: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMOS)  $(MLDONKEYGUI_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI_CMAS) $(MLDONKEYGUI_CMOS) 
 
mlgui.static:  $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS)  $(MLDONKEYGUI_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEYGUI_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(MLDONKEYGUI_CMXAS) $(MLDONKEYGUI_CMXS)


ifneq ("$(BUILD_NEWGUI)", "yes")
 ifneq ("$(USE_GTK2)", "yes")
   
MLDONKEYGUI2_ZOG := $(filter %.zog, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLL := $(filter %.mll, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLY := $(filter %.mly, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_ML4 := $(filter %.ml4, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLT := $(filter %.mlt, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLP := $(filter %.mlcpp, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_C := $(filter %.c, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_CMOS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmo) 
MLDONKEYGUI2_CMXS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmx) 
MLDONKEYGUI2_OBJS=$(foreach file, $(MLDONKEYGUI2_C),   $(basename $(file)).o)    

MLDONKEYGUI2_CMXAS := $(foreach file, $(MLDONKEYGUI2_CMXA),   build/$(basename $(file)).cmxa)
MLDONKEYGUI2_CMAS=$(foreach file, $(MLDONKEYGUI2_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLDONKEYGUI2_ML4:.ml4=.ml) $(MLDONKEYGUI2_MLT:.mlt=.ml) $(MLDONKEYGUI2_MLP:.mlcpp=.ml) $(MLDONKEYGUI2_MLL:.mll=.ml) $(MLDONKEYGUI2_MLY:.mly=.ml) $(MLDONKEYGUI2_MLY:.mly=.mli) $(MLDONKEYGUI2_ZOG:.zog=.ml) 
 
mlgui2: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS) $(MLDONKEYGUI2_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI2_CMXAS) $(MLDONKEYGUI2_CMXS) 
 
mlgui2.byte: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMOS)  $(MLDONKEYGUI2_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI2_CMAS) $(MLDONKEYGUI2_CMOS) 
 
mlgui2.static:  $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS)  $(MLDONKEYGUI2_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEYGUI2_OBJS) $(LIBS_opt) $(LIBS_flags)  $(GTK_LIBS_flags)  $(GTK_STATIC_LIBS_opt) -I build $(MLDONKEYGUI2_CMXAS) $(MLDONKEYGUI2_CMXS)

 endif
endif


mldc_ZOG := $(filter %.zog, $(mldc_SRCS)) 
mldc_MLL := $(filter %.mll, $(mldc_SRCS)) 
mldc_MLY := $(filter %.mly, $(mldc_SRCS)) 
mldc_ML4 := $(filter %.ml4, $(mldc_SRCS)) 
mldc_MLT := $(filter %.mlt, $(mldc_SRCS)) 
mldc_MLP := $(filter %.mlcpp, $(mldc_SRCS)) 
mldc_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mldc_SRCS)) 
mldc_C := $(filter %.c, $(mldc_SRCS)) 
mldc_CMOS=$(foreach file, $(mldc_ML),   $(basename $(file)).cmo) 
mldc_CMXS=$(foreach file, $(mldc_ML),   $(basename $(file)).cmx) 
mldc_OBJS=$(foreach file, $(mldc_C),   $(basename $(file)).o)    

mldc_CMXAS := $(foreach file, $(mldc_CMXA),   build/$(basename $(file)).cmxa)
mldc_CMAS=$(foreach file, $(mldc_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldc_ML4:.ml4=.ml) $(mldc_MLT:.mlt=.ml) $(mldc_MLP:.mlcpp=.ml) $(mldc_MLL:.mll=.ml) $(mldc_MLY:.mly=.ml) $(mldc_MLY:.mly=.mli) $(mldc_ZOG:.zog=.ml) 
 
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
mldc+gui_MLT := $(filter %.mlt, $(mldc+gui_SRCS)) 
mldc+gui_MLP := $(filter %.mlcpp, $(mldc+gui_SRCS)) 
mldc+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mldc+gui_SRCS)) 
mldc+gui_C := $(filter %.c, $(mldc+gui_SRCS)) 
mldc+gui_CMOS=$(foreach file, $(mldc+gui_ML),   $(basename $(file)).cmo) 
mldc+gui_CMXS=$(foreach file, $(mldc+gui_ML),   $(basename $(file)).cmx) 
mldc+gui_OBJS=$(foreach file, $(mldc+gui_C),   $(basename $(file)).o)    

mldc+gui_CMXAS := $(foreach file, $(mldc+gui_CMXA),   build/$(basename $(file)).cmxa)
mldc+gui_CMAS=$(foreach file, $(mldc+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mldc+gui_ML4:.ml4=.ml) $(mldc+gui_MLT:.mlt=.ml) $(mldc+gui_MLP:.mlcpp=.ml) $(mldc+gui_MLL:.mll=.ml) $(mldc+gui_MLY:.mly=.ml) $(mldc+gui_MLY:.mly=.mli) $(mldc+gui_ZOG:.zog=.ml) 
 
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
mlnap_MLT := $(filter %.mlt, $(mlnap_SRCS)) 
mlnap_MLP := $(filter %.mlcpp, $(mlnap_SRCS)) 
mlnap_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlnap_SRCS)) 
mlnap_C := $(filter %.c, $(mlnap_SRCS)) 
mlnap_CMOS=$(foreach file, $(mlnap_ML),   $(basename $(file)).cmo) 
mlnap_CMXS=$(foreach file, $(mlnap_ML),   $(basename $(file)).cmx) 
mlnap_OBJS=$(foreach file, $(mlnap_C),   $(basename $(file)).o)    

mlnap_CMXAS := $(foreach file, $(mlnap_CMXA),   build/$(basename $(file)).cmxa)
mlnap_CMAS=$(foreach file, $(mlnap_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlnap_ML4:.ml4=.ml) $(mlnap_MLT:.mlt=.ml) $(mlnap_MLP:.mlcpp=.ml) $(mlnap_MLL:.mll=.ml) $(mlnap_MLY:.mly=.ml) $(mlnap_MLY:.mly=.mli) $(mlnap_ZOG:.zog=.ml) 
 
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
mlnap+gui_MLT := $(filter %.mlt, $(mlnap+gui_SRCS)) 
mlnap+gui_MLP := $(filter %.mlcpp, $(mlnap+gui_SRCS)) 
mlnap+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlnap+gui_SRCS)) 
mlnap+gui_C := $(filter %.c, $(mlnap+gui_SRCS)) 
mlnap+gui_CMOS=$(foreach file, $(mlnap+gui_ML),   $(basename $(file)).cmo) 
mlnap+gui_CMXS=$(foreach file, $(mlnap+gui_ML),   $(basename $(file)).cmx) 
mlnap+gui_OBJS=$(foreach file, $(mlnap+gui_C),   $(basename $(file)).o)    

mlnap+gui_CMXAS := $(foreach file, $(mlnap+gui_CMXA),   build/$(basename $(file)).cmxa)
mlnap+gui_CMAS=$(foreach file, $(mlnap+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlnap+gui_ML4:.ml4=.ml) $(mlnap+gui_MLT:.mlt=.ml) $(mlnap+gui_MLP:.mlcpp=.ml) $(mlnap+gui_MLL:.mll=.ml) $(mlnap+gui_MLY:.mly=.ml) $(mlnap+gui_MLY:.mly=.mli) $(mlnap+gui_ZOG:.zog=.ml) 
 
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
MLNET_MLT := $(filter %.mlt, $(MLNET_SRCS)) 
MLNET_MLP := $(filter %.mlcpp, $(MLNET_SRCS)) 
MLNET_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLNET_SRCS)) 
MLNET_C := $(filter %.c, $(MLNET_SRCS)) 
MLNET_CMOS=$(foreach file, $(MLNET_ML),   $(basename $(file)).cmo) 
MLNET_CMXS=$(foreach file, $(MLNET_ML),   $(basename $(file)).cmx) 
MLNET_OBJS=$(foreach file, $(MLNET_C),   $(basename $(file)).o)    

MLNET_CMXAS := $(foreach file, $(MLNET_CMXA),   build/$(basename $(file)).cmxa)
MLNET_CMAS=$(foreach file, $(MLNET_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLNET_ML4:.ml4=.ml) $(MLNET_MLT:.mlt=.ml) $(MLNET_MLP:.mlcpp=.ml) $(MLNET_MLL:.mll=.ml) $(MLNET_MLY:.mly=.ml) $(MLNET_MLY:.mly=.mli) $(MLNET_ZOG:.zog=.ml) 
 
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
mlnet+gui_MLT := $(filter %.mlt, $(mlnet+gui_SRCS)) 
mlnet+gui_MLP := $(filter %.mlcpp, $(mlnet+gui_SRCS)) 
mlnet+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlnet+gui_SRCS)) 
mlnet+gui_C := $(filter %.c, $(mlnet+gui_SRCS)) 
mlnet+gui_CMOS=$(foreach file, $(mlnet+gui_ML),   $(basename $(file)).cmo) 
mlnet+gui_CMXS=$(foreach file, $(mlnet+gui_ML),   $(basename $(file)).cmx) 
mlnet+gui_OBJS=$(foreach file, $(mlnet+gui_C),   $(basename $(file)).o)    

mlnet+gui_CMXAS := $(foreach file, $(mlnet+gui_CMXA),   build/$(basename $(file)).cmxa)
mlnet+gui_CMAS=$(foreach file, $(mlnet+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlnet+gui_ML4:.ml4=.ml) $(mlnet+gui_MLT:.mlt=.ml) $(mlnet+gui_MLP:.mlcpp=.ml) $(mlnet+gui_MLL:.mll=.ml) $(mlnet+gui_MLY:.mly=.ml) $(mlnet+gui_MLY:.mly=.mli) $(mlnet+gui_ZOG:.zog=.ml) 
 
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
mlgnut_MLT := $(filter %.mlt, $(mlgnut_SRCS)) 
mlgnut_MLP := $(filter %.mlcpp, $(mlgnut_SRCS)) 
mlgnut_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlgnut_SRCS)) 
mlgnut_C := $(filter %.c, $(mlgnut_SRCS)) 
mlgnut_CMOS=$(foreach file, $(mlgnut_ML),   $(basename $(file)).cmo) 
mlgnut_CMXS=$(foreach file, $(mlgnut_ML),   $(basename $(file)).cmx) 
mlgnut_OBJS=$(foreach file, $(mlgnut_C),   $(basename $(file)).o)    

mlgnut_CMXAS := $(foreach file, $(mlgnut_CMXA),   build/$(basename $(file)).cmxa)
mlgnut_CMAS=$(foreach file, $(mlgnut_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlgnut_ML4:.ml4=.ml) $(mlgnut_MLT:.mlt=.ml) $(mlgnut_MLP:.mlcpp=.ml) $(mlgnut_MLL:.mll=.ml) $(mlgnut_MLY:.mly=.ml) $(mlgnut_MLY:.mly=.mli) $(mlgnut_ZOG:.zog=.ml) 
 
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
mlbt_MLT := $(filter %.mlt, $(mlbt_SRCS)) 
mlbt_MLP := $(filter %.mlcpp, $(mlbt_SRCS)) 
mlbt_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlbt_SRCS)) 
mlbt_C := $(filter %.c, $(mlbt_SRCS)) 
mlbt_CMOS=$(foreach file, $(mlbt_ML),   $(basename $(file)).cmo) 
mlbt_CMXS=$(foreach file, $(mlbt_ML),   $(basename $(file)).cmx) 
mlbt_OBJS=$(foreach file, $(mlbt_C),   $(basename $(file)).o)    

mlbt_CMXAS := $(foreach file, $(mlbt_CMXA),   build/$(basename $(file)).cmxa)
mlbt_CMAS=$(foreach file, $(mlbt_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlbt_ML4:.ml4=.ml) $(mlbt_MLT:.mlt=.ml) $(mlbt_MLP:.mlcpp=.ml) $(mlbt_MLL:.mll=.ml) $(mlbt_MLY:.mly=.ml) $(mlbt_MLY:.mly=.mli) $(mlbt_ZOG:.zog=.ml) 
 
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
mlgnut+gui_MLT := $(filter %.mlt, $(mlgnut+gui_SRCS)) 
mlgnut+gui_MLP := $(filter %.mlcpp, $(mlgnut+gui_SRCS)) 
mlgnut+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlgnut+gui_SRCS)) 
mlgnut+gui_C := $(filter %.c, $(mlgnut+gui_SRCS)) 
mlgnut+gui_CMOS=$(foreach file, $(mlgnut+gui_ML),   $(basename $(file)).cmo) 
mlgnut+gui_CMXS=$(foreach file, $(mlgnut+gui_ML),   $(basename $(file)).cmx) 
mlgnut+gui_OBJS=$(foreach file, $(mlgnut+gui_C),   $(basename $(file)).o)    

mlgnut+gui_CMXAS := $(foreach file, $(mlgnut+gui_CMXA),   build/$(basename $(file)).cmxa)
mlgnut+gui_CMAS=$(foreach file, $(mlgnut+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlgnut+gui_ML4:.ml4=.ml) $(mlgnut+gui_MLT:.mlt=.ml) $(mlgnut+gui_MLP:.mlcpp=.ml) $(mlgnut+gui_MLL:.mll=.ml) $(mlgnut+gui_MLY:.mly=.ml) $(mlgnut+gui_MLY:.mly=.mli) $(mlgnut+gui_ZOG:.zog=.ml) 
 
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
mlbt+gui_MLT := $(filter %.mlt, $(mlbt+gui_SRCS)) 
mlbt+gui_MLP := $(filter %.mlcpp, $(mlbt+gui_SRCS)) 
mlbt+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlbt+gui_SRCS)) 
mlbt+gui_C := $(filter %.c, $(mlbt+gui_SRCS)) 
mlbt+gui_CMOS=$(foreach file, $(mlbt+gui_ML),   $(basename $(file)).cmo) 
mlbt+gui_CMXS=$(foreach file, $(mlbt+gui_ML),   $(basename $(file)).cmx) 
mlbt+gui_OBJS=$(foreach file, $(mlbt+gui_C),   $(basename $(file)).o)    

mlbt+gui_CMXAS := $(foreach file, $(mlbt+gui_CMXA),   build/$(basename $(file)).cmxa)
mlbt+gui_CMAS=$(foreach file, $(mlbt+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlbt+gui_ML4:.ml4=.ml) $(mlbt+gui_MLT:.mlt=.ml) $(mlbt+gui_MLP:.mlcpp=.ml) $(mlbt+gui_MLL:.mll=.ml) $(mlbt+gui_MLY:.mly=.ml) $(mlbt+gui_MLY:.mly=.mli) $(mlbt+gui_ZOG:.zog=.ml) 
 
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
mlslsk_MLT := $(filter %.mlt, $(mlslsk_SRCS)) 
mlslsk_MLP := $(filter %.mlcpp, $(mlslsk_SRCS)) 
mlslsk_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlslsk_SRCS)) 
mlslsk_C := $(filter %.c, $(mlslsk_SRCS)) 
mlslsk_CMOS=$(foreach file, $(mlslsk_ML),   $(basename $(file)).cmo) 
mlslsk_CMXS=$(foreach file, $(mlslsk_ML),   $(basename $(file)).cmx) 
mlslsk_OBJS=$(foreach file, $(mlslsk_C),   $(basename $(file)).o)    

mlslsk_CMXAS := $(foreach file, $(mlslsk_CMXA),   build/$(basename $(file)).cmxa)
mlslsk_CMAS=$(foreach file, $(mlslsk_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlslsk_ML4:.ml4=.ml) $(mlslsk_MLT:.mlt=.ml) $(mlslsk_MLP:.mlcpp=.ml) $(mlslsk_MLL:.mll=.ml) $(mlslsk_MLY:.mly=.ml) $(mlslsk_MLY:.mly=.mli) $(mlslsk_ZOG:.zog=.ml) 
 
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
mlslsk+gui_MLT := $(filter %.mlt, $(mlslsk+gui_SRCS)) 
mlslsk+gui_MLP := $(filter %.mlcpp, $(mlslsk+gui_SRCS)) 
mlslsk+gui_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(mlslsk+gui_SRCS)) 
mlslsk+gui_C := $(filter %.c, $(mlslsk+gui_SRCS)) 
mlslsk+gui_CMOS=$(foreach file, $(mlslsk+gui_ML),   $(basename $(file)).cmo) 
mlslsk+gui_CMXS=$(foreach file, $(mlslsk+gui_ML),   $(basename $(file)).cmx) 
mlslsk+gui_OBJS=$(foreach file, $(mlslsk+gui_C),   $(basename $(file)).o)    

mlslsk+gui_CMXAS := $(foreach file, $(mlslsk+gui_CMXA),   build/$(basename $(file)).cmxa)
mlslsk+gui_CMAS=$(foreach file, $(mlslsk+gui_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(mlslsk+gui_ML4:.ml4=.ml) $(mlslsk+gui_MLT:.mlt=.ml) $(mlslsk+gui_MLP:.mlcpp=.ml) $(mlslsk+gui_MLL:.mll=.ml) $(mlslsk+gui_MLY:.mly=.ml) $(mlslsk+gui_MLY:.mly=.mli) $(mlslsk+gui_ZOG:.zog=.ml) 
 
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
MLDONKEY_IM_MLT := $(filter %.mlt, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_MLP := $(filter %.mlcpp, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_C := $(filter %.c, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_CMOS=$(foreach file, $(MLDONKEY_IM_ML),   $(basename $(file)).cmo) 
MLDONKEY_IM_CMXS=$(foreach file, $(MLDONKEY_IM_ML),   $(basename $(file)).cmx) 
MLDONKEY_IM_OBJS=$(foreach file, $(MLDONKEY_IM_C),   $(basename $(file)).o)    

MLDONKEY_IM_CMXAS := $(foreach file, $(MLDONKEY_IM_CMXA),   build/$(basename $(file)).cmxa)
MLDONKEY_IM_CMAS=$(foreach file, $(MLDONKEY_IM_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLDONKEY_IM_ML4:.ml4=.ml) $(MLDONKEY_IM_MLT:.mlt=.ml) $(MLDONKEY_IM_MLP:.mlcpp=.ml) $(MLDONKEY_IM_MLL:.mll=.ml) $(MLDONKEY_IM_MLY:.mly=.ml) $(MLDONKEY_IM_MLY:.mly=.mli) $(MLDONKEY_IM_ZOG:.zog=.ml) 
 
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
STARTER_MLT := $(filter %.mlt, $(STARTER_SRCS)) 
STARTER_MLP := $(filter %.mlcpp, $(STARTER_SRCS)) 
STARTER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(STARTER_SRCS)) 
STARTER_C := $(filter %.c, $(STARTER_SRCS)) 
STARTER_CMOS=$(foreach file, $(STARTER_ML),   $(basename $(file)).cmo) 
STARTER_CMXS=$(foreach file, $(STARTER_ML),   $(basename $(file)).cmx) 
STARTER_OBJS=$(foreach file, $(STARTER_C),   $(basename $(file)).o)    

STARTER_CMXAS := $(foreach file, $(STARTER_CMXA),   build/$(basename $(file)).cmxa)
STARTER_CMAS=$(foreach file, $(STARTER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(STARTER_ML4:.ml4=.ml) $(STARTER_MLT:.mlt=.ml) $(STARTER_MLP:.mlcpp=.ml) $(STARTER_MLL:.mll=.ml) $(STARTER_MLY:.mly=.ml) $(STARTER_MLY:.mly=.mli) $(STARTER_ZOG:.zog=.ml) 
 
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
MLCHAT_MLT := $(filter %.mlt, $(MLCHAT_SRCS)) 
MLCHAT_MLP := $(filter %.mlcpp, $(MLCHAT_SRCS)) 
MLCHAT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLCHAT_SRCS)) 
MLCHAT_C := $(filter %.c, $(MLCHAT_SRCS)) 
MLCHAT_CMOS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmo) 
MLCHAT_CMXS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmx) 
MLCHAT_OBJS=$(foreach file, $(MLCHAT_C),   $(basename $(file)).o)    

MLCHAT_CMXAS := $(foreach file, $(MLCHAT_CMXA),   build/$(basename $(file)).cmxa)
MLCHAT_CMAS=$(foreach file, $(MLCHAT_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLCHAT_ML4:.ml4=.ml) $(MLCHAT_MLT:.mlt=.ml) $(MLCHAT_MLP:.mlcpp=.ml) $(MLCHAT_MLL:.mll=.ml) $(MLCHAT_MLY:.mly=.ml) $(MLCHAT_MLY:.mly=.mli) $(MLCHAT_ZOG:.zog=.ml) 
 
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
OBSERVER_MLT := $(filter %.mlt, $(OBSERVER_SRCS)) 
OBSERVER_MLP := $(filter %.mlcpp, $(OBSERVER_SRCS)) 
OBSERVER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(OBSERVER_SRCS)) 
OBSERVER_C := $(filter %.c, $(OBSERVER_SRCS)) 
OBSERVER_CMOS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmo) 
OBSERVER_CMXS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmx) 
OBSERVER_OBJS=$(foreach file, $(OBSERVER_C),   $(basename $(file)).o)    

OBSERVER_CMXAS := $(foreach file, $(OBSERVER_CMXA),   build/$(basename $(file)).cmxa)
OBSERVER_CMAS=$(foreach file, $(OBSERVER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(OBSERVER_ML4:.ml4=.ml) $(OBSERVER_MLT:.mlt=.ml) $(OBSERVER_MLP:.mlcpp=.ml) $(OBSERVER_MLL:.mll=.ml) $(OBSERVER_MLY:.mly=.ml) $(OBSERVER_MLY:.mly=.mli) $(OBSERVER_ZOG:.zog=.ml) 
 
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
ED2K_HASH_MLT := $(filter %.mlt, $(ED2K_HASH_SRCS)) 
ED2K_HASH_MLP := $(filter %.mlcpp, $(ED2K_HASH_SRCS)) 
ED2K_HASH_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(ED2K_HASH_SRCS)) 
ED2K_HASH_C := $(filter %.c, $(ED2K_HASH_SRCS)) 
ED2K_HASH_CMOS=$(foreach file, $(ED2K_HASH_ML),   $(basename $(file)).cmo) 
ED2K_HASH_CMXS=$(foreach file, $(ED2K_HASH_ML),   $(basename $(file)).cmx) 
ED2K_HASH_OBJS=$(foreach file, $(ED2K_HASH_C),   $(basename $(file)).o)    

ED2K_HASH_CMXAS := $(foreach file, $(ED2K_HASH_CMXA),   build/$(basename $(file)).cmxa)
ED2K_HASH_CMAS=$(foreach file, $(ED2K_HASH_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(ED2K_HASH_ML4:.ml4=.ml) $(ED2K_HASH_MLT:.mlt=.ml) $(ED2K_HASH_MLP:.mlcpp=.ml) $(ED2K_HASH_MLL:.mll=.ml) $(ED2K_HASH_MLY:.mly=.ml) $(ED2K_HASH_MLY:.mly=.mli) $(ED2K_HASH_ZOG:.zog=.ml) 
 
ed2k_hash: $(ED2K_HASH_OBJS) $(ED2K_HASH_CMXS) $(ED2K_HASH_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(ED2K_HASH_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(ED2K_HASH_CMXAS) $(ED2K_HASH_CMXS) 
 
ed2k_hash.byte: $(ED2K_HASH_OBJS) $(ED2K_HASH_CMOS)  $(ED2K_HASH_CMAS)
	$(OCAMLC) -linkall -o $@  $(ED2K_HASH_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(ED2K_HASH_CMAS) $(ED2K_HASH_CMOS) 
 
ed2k_hash.static:  $(ED2K_HASH_OBJS) $(ED2K_HASH_CMXS)  $(ED2K_HASH_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(ED2K_HASH_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(ED2K_HASH_CMXAS) $(ED2K_HASH_CMXS)


OCAMLPP_ZOG := $(filter %.zog, $(OCAMLPP_SRCS)) 
OCAMLPP_MLL := $(filter %.mll, $(OCAMLPP_SRCS)) 
OCAMLPP_MLY := $(filter %.mly, $(OCAMLPP_SRCS)) 
OCAMLPP_ML4 := $(filter %.ml4, $(OCAMLPP_SRCS)) 
OCAMLPP_MLT := $(filter %.mlt, $(OCAMLPP_SRCS)) 
OCAMLPP_MLP := $(filter %.mlcpp, $(OCAMLPP_SRCS)) 
OCAMLPP_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(OCAMLPP_SRCS)) 
OCAMLPP_C := $(filter %.c, $(OCAMLPP_SRCS)) 
OCAMLPP_CMOS=$(foreach file, $(OCAMLPP_ML),   $(basename $(file)).cmo) 
OCAMLPP_CMXS=$(foreach file, $(OCAMLPP_ML),   $(basename $(file)).cmx) 
OCAMLPP_OBJS=$(foreach file, $(OCAMLPP_C),   $(basename $(file)).o)    

OCAMLPP_CMXAS := $(foreach file, $(OCAMLPP_CMXA),   build/$(basename $(file)).cmxa)
OCAMLPP_CMAS=$(foreach file, $(OCAMLPP_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(OCAMLPP_ML4:.ml4=.ml) $(OCAMLPP_MLT:.mlt=.ml) $(OCAMLPP_MLP:.mlcpp=.ml) $(OCAMLPP_MLL:.mll=.ml) $(OCAMLPP_MLY:.mly=.ml) $(OCAMLPP_MLY:.mly=.mli) $(OCAMLPP_ZOG:.zog=.ml) 
 
ocamlpp: $(OCAMLPP_OBJS) $(OCAMLPP_CMXS) $(OCAMLPP_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(OCAMLPP_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(OCAMLPP_CMXAS) $(OCAMLPP_CMXS) 
 
ocamlpp.byte: $(OCAMLPP_OBJS) $(OCAMLPP_CMOS)  $(OCAMLPP_CMAS)
	$(OCAMLC) -linkall -o $@  $(OCAMLPP_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(OCAMLPP_CMAS) $(OCAMLPP_CMOS) 
 
ocamlpp.static:  $(OCAMLPP_OBJS) $(OCAMLPP_CMXS)  $(OCAMLPP_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(OCAMLPP_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(OCAMLPP_CMXAS) $(OCAMLPP_CMXS)


MAKE_TORRENT_ZOG := $(filter %.zog, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_MLL := $(filter %.mll, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_MLY := $(filter %.mly, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_ML4 := $(filter %.ml4, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_MLT := $(filter %.mlt, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_MLP := $(filter %.mlcpp, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_C := $(filter %.c, $(MAKE_TORRENT_SRCS)) 
MAKE_TORRENT_CMOS=$(foreach file, $(MAKE_TORRENT_ML),   $(basename $(file)).cmo) 
MAKE_TORRENT_CMXS=$(foreach file, $(MAKE_TORRENT_ML),   $(basename $(file)).cmx) 
MAKE_TORRENT_OBJS=$(foreach file, $(MAKE_TORRENT_C),   $(basename $(file)).o)    

MAKE_TORRENT_CMXAS := $(foreach file, $(MAKE_TORRENT_CMXA),   build/$(basename $(file)).cmxa)
MAKE_TORRENT_CMAS=$(foreach file, $(MAKE_TORRENT_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MAKE_TORRENT_ML4:.ml4=.ml) $(MAKE_TORRENT_MLT:.mlt=.ml) $(MAKE_TORRENT_MLP:.mlcpp=.ml) $(MAKE_TORRENT_MLL:.mll=.ml) $(MAKE_TORRENT_MLY:.mly=.ml) $(MAKE_TORRENT_MLY:.mly=.mli) $(MAKE_TORRENT_ZOG:.zog=.ml) 
 
make_torrent: $(MAKE_TORRENT_OBJS) $(MAKE_TORRENT_CMXS) $(MAKE_TORRENT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MAKE_TORRENT_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(MAKE_TORRENT_CMXAS) $(MAKE_TORRENT_CMXS) 
 
make_torrent.byte: $(MAKE_TORRENT_OBJS) $(MAKE_TORRENT_CMOS)  $(MAKE_TORRENT_CMAS)
	$(OCAMLC) -linkall -o $@  $(MAKE_TORRENT_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(MAKE_TORRENT_CMAS) $(MAKE_TORRENT_CMOS) 
 
make_torrent.static:  $(MAKE_TORRENT_OBJS) $(MAKE_TORRENT_CMXS)  $(MAKE_TORRENT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MAKE_TORRENT_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(MAKE_TORRENT_CMXAS) $(MAKE_TORRENT_CMXS)


SUBCONV_ZOG := $(filter %.zog, $(SUBCONV_SRCS)) 
SUBCONV_MLL := $(filter %.mll, $(SUBCONV_SRCS)) 
SUBCONV_MLY := $(filter %.mly, $(SUBCONV_SRCS)) 
SUBCONV_ML4 := $(filter %.ml4, $(SUBCONV_SRCS)) 
SUBCONV_MLT := $(filter %.mlt, $(SUBCONV_SRCS)) 
SUBCONV_MLP := $(filter %.mlcpp, $(SUBCONV_SRCS)) 
SUBCONV_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(SUBCONV_SRCS)) 
SUBCONV_C := $(filter %.c, $(SUBCONV_SRCS)) 
SUBCONV_CMOS=$(foreach file, $(SUBCONV_ML),   $(basename $(file)).cmo) 
SUBCONV_CMXS=$(foreach file, $(SUBCONV_ML),   $(basename $(file)).cmx) 
SUBCONV_OBJS=$(foreach file, $(SUBCONV_C),   $(basename $(file)).o)    

SUBCONV_CMXAS := $(foreach file, $(SUBCONV_CMXA),   build/$(basename $(file)).cmxa)
SUBCONV_CMAS=$(foreach file, $(SUBCONV_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(SUBCONV_ML4:.ml4=.ml) $(SUBCONV_MLT:.mlt=.ml) $(SUBCONV_MLP:.mlcpp=.ml) $(SUBCONV_MLL:.mll=.ml) $(SUBCONV_MLY:.mly=.ml) $(SUBCONV_MLY:.mly=.mli) $(SUBCONV_ZOG:.zog=.ml) 
 
subconv: $(SUBCONV_OBJS) $(SUBCONV_CMXS) $(SUBCONV_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(SUBCONV_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(SUBCONV_CMXAS) $(SUBCONV_CMXS) 
 
subconv.byte: $(SUBCONV_OBJS) $(SUBCONV_CMOS)  $(SUBCONV_CMAS)
	$(OCAMLC) -linkall -o $@  $(SUBCONV_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(SUBCONV_CMAS) $(SUBCONV_CMOS) 
 
subconv.static:  $(SUBCONV_OBJS) $(SUBCONV_CMXS)  $(SUBCONV_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(SUBCONV_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(SUBCONV_CMXAS) $(SUBCONV_CMXS)


MLSPLIT_ZOG := $(filter %.zog, $(MLSPLIT_SRCS)) 
MLSPLIT_MLL := $(filter %.mll, $(MLSPLIT_SRCS)) 
MLSPLIT_MLY := $(filter %.mly, $(MLSPLIT_SRCS)) 
MLSPLIT_ML4 := $(filter %.ml4, $(MLSPLIT_SRCS)) 
MLSPLIT_MLT := $(filter %.mlt, $(MLSPLIT_SRCS)) 
MLSPLIT_MLP := $(filter %.mlcpp, $(MLSPLIT_SRCS)) 
MLSPLIT_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLSPLIT_SRCS)) 
MLSPLIT_C := $(filter %.c, $(MLSPLIT_SRCS)) 
MLSPLIT_CMOS=$(foreach file, $(MLSPLIT_ML),   $(basename $(file)).cmo) 
MLSPLIT_CMXS=$(foreach file, $(MLSPLIT_ML),   $(basename $(file)).cmx) 
MLSPLIT_OBJS=$(foreach file, $(MLSPLIT_C),   $(basename $(file)).o)    

MLSPLIT_CMXAS := $(foreach file, $(MLSPLIT_CMXA),   build/$(basename $(file)).cmxa)
MLSPLIT_CMAS=$(foreach file, $(MLSPLIT_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLSPLIT_ML4:.ml4=.ml) $(MLSPLIT_MLT:.mlt=.ml) $(MLSPLIT_MLP:.mlcpp=.ml) $(MLSPLIT_MLL:.mll=.ml) $(MLSPLIT_MLY:.mly=.ml) $(MLSPLIT_MLY:.mly=.mli) $(MLSPLIT_ZOG:.zog=.ml) 
 
mlsplit: $(MLSPLIT_OBJS) $(MLSPLIT_CMXS) $(MLSPLIT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLSPLIT_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(MLSPLIT_CMXAS) $(MLSPLIT_CMXS) 
 
mlsplit.byte: $(MLSPLIT_OBJS) $(MLSPLIT_CMOS)  $(MLSPLIT_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLSPLIT_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(MLSPLIT_CMAS) $(MLSPLIT_CMOS) 
 
mlsplit.static:  $(MLSPLIT_OBJS) $(MLSPLIT_CMXS)  $(MLSPLIT_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLSPLIT_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(MLSPLIT_CMXAS) $(MLSPLIT_CMXS)


CONTESTER_ZOG := $(filter %.zog, $(CONTESTER_SRCS)) 
CONTESTER_MLL := $(filter %.mll, $(CONTESTER_SRCS)) 
CONTESTER_MLY := $(filter %.mly, $(CONTESTER_SRCS)) 
CONTESTER_ML4 := $(filter %.ml4, $(CONTESTER_SRCS)) 
CONTESTER_MLT := $(filter %.mlt, $(CONTESTER_SRCS)) 
CONTESTER_MLP := $(filter %.mlcpp, $(CONTESTER_SRCS)) 
CONTESTER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(CONTESTER_SRCS)) 
CONTESTER_C := $(filter %.c, $(CONTESTER_SRCS)) 
CONTESTER_CMOS=$(foreach file, $(CONTESTER_ML),   $(basename $(file)).cmo) 
CONTESTER_CMXS=$(foreach file, $(CONTESTER_ML),   $(basename $(file)).cmx) 
CONTESTER_OBJS=$(foreach file, $(CONTESTER_C),   $(basename $(file)).o)    

CONTESTER_CMXAS := $(foreach file, $(CONTESTER_CMXA),   build/$(basename $(file)).cmxa)
CONTESTER_CMAS=$(foreach file, $(CONTESTER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(CONTESTER_ML4:.ml4=.ml) $(CONTESTER_MLT:.mlt=.ml) $(CONTESTER_MLP:.mlcpp=.ml) $(CONTESTER_MLL:.mll=.ml) $(CONTESTER_MLY:.mly=.ml) $(CONTESTER_MLY:.mly=.mli) $(CONTESTER_ZOG:.zog=.ml) 
 
contester: $(CONTESTER_OBJS) $(CONTESTER_CMXS) $(CONTESTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(CONTESTER_OBJS) $(LIBS_opt) $(LIBS_flags) $(CRYPT_LIBS_opt) $(CRYPT_LIBS_flags) -I build $(CONTESTER_CMXAS) $(CONTESTER_CMXS) 
 
contester.byte: $(CONTESTER_OBJS) $(CONTESTER_CMOS)  $(CONTESTER_CMAS)
	$(OCAMLC) -linkall -o $@  $(CONTESTER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(CRYPT_LIBS_byte) $(CRYPT_LIBS_flags) -I build $(CONTESTER_CMAS) $(CONTESTER_CMOS) 
 
contester.static:  $(CONTESTER_OBJS) $(CONTESTER_CMXS)  $(CONTESTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(CONTESTER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(CRYPT_LIBS_flags)  $(CRYPT_STATIC_LIBS_opt) -I build $(CONTESTER_CMXAS) $(CONTESTER_CMXS)


SAFEEXEC_ZOG := $(filter %.zog, $(SAFEEXEC_SRCS)) 
SAFEEXEC_MLL := $(filter %.mll, $(SAFEEXEC_SRCS)) 
SAFEEXEC_MLY := $(filter %.mly, $(SAFEEXEC_SRCS)) 
SAFEEXEC_ML4 := $(filter %.ml4, $(SAFEEXEC_SRCS)) 
SAFEEXEC_MLT := $(filter %.mlt, $(SAFEEXEC_SRCS)) 
SAFEEXEC_MLP := $(filter %.mlcpp, $(SAFEEXEC_SRCS)) 
SAFEEXEC_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(SAFEEXEC_SRCS)) 
SAFEEXEC_C := $(filter %.c, $(SAFEEXEC_SRCS)) 
SAFEEXEC_CMOS=$(foreach file, $(SAFEEXEC_ML),   $(basename $(file)).cmo) 
SAFEEXEC_CMXS=$(foreach file, $(SAFEEXEC_ML),   $(basename $(file)).cmx) 
SAFEEXEC_OBJS=$(foreach file, $(SAFEEXEC_C),   $(basename $(file)).o)    

SAFEEXEC_CMXAS := $(foreach file, $(SAFEEXEC_CMXA),   build/$(basename $(file)).cmxa)
SAFEEXEC_CMAS=$(foreach file, $(SAFEEXEC_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(SAFEEXEC_ML4:.ml4=.ml) $(SAFEEXEC_MLT:.mlt=.ml) $(SAFEEXEC_MLP:.mlcpp=.ml) $(SAFEEXEC_MLL:.mll=.ml) $(SAFEEXEC_MLY:.mly=.ml) $(SAFEEXEC_MLY:.mly=.mli) $(SAFEEXEC_ZOG:.zog=.ml) 
 
safeexec: $(SAFEEXEC_OBJS) $(SAFEEXEC_CMXS) $(SAFEEXEC_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(SAFEEXEC_OBJS) $(LIBS_opt) $(LIBS_flags) $(CRYPT_LIBS_opt) $(CRYPT_LIBS_flags) -I build $(SAFEEXEC_CMXAS) $(SAFEEXEC_CMXS) 
 
safeexec.byte: $(SAFEEXEC_OBJS) $(SAFEEXEC_CMOS)  $(SAFEEXEC_CMAS)
	$(OCAMLC) -linkall -o $@  $(SAFEEXEC_OBJS) $(LIBS_byte) $(LIBS_flags)  $(CRYPT_LIBS_byte) $(CRYPT_LIBS_flags) -I build $(SAFEEXEC_CMAS) $(SAFEEXEC_CMOS) 
 
safeexec.static:  $(SAFEEXEC_OBJS) $(SAFEEXEC_CMXS)  $(SAFEEXEC_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(SAFEEXEC_OBJS) $(LIBS_opt) $(LIBS_flags)  $(CRYPT_LIBS_flags)  $(CRYPT_STATIC_LIBS_opt) -I build $(SAFEEXEC_CMXAS) $(SAFEEXEC_CMXS)


GET_RANGE_ZOG := $(filter %.zog, $(GET_RANGE_SRCS)) 
GET_RANGE_MLL := $(filter %.mll, $(GET_RANGE_SRCS)) 
GET_RANGE_MLY := $(filter %.mly, $(GET_RANGE_SRCS)) 
GET_RANGE_ML4 := $(filter %.ml4, $(GET_RANGE_SRCS)) 
GET_RANGE_MLT := $(filter %.mlt, $(GET_RANGE_SRCS)) 
GET_RANGE_MLP := $(filter %.mlcpp, $(GET_RANGE_SRCS)) 
GET_RANGE_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(GET_RANGE_SRCS)) 
GET_RANGE_C := $(filter %.c, $(GET_RANGE_SRCS)) 
GET_RANGE_CMOS=$(foreach file, $(GET_RANGE_ML),   $(basename $(file)).cmo) 
GET_RANGE_CMXS=$(foreach file, $(GET_RANGE_ML),   $(basename $(file)).cmx) 
GET_RANGE_OBJS=$(foreach file, $(GET_RANGE_C),   $(basename $(file)).o)    

GET_RANGE_CMXAS := $(foreach file, $(GET_RANGE_CMXA),   build/$(basename $(file)).cmxa)
GET_RANGE_CMAS=$(foreach file, $(GET_RANGE_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(GET_RANGE_ML4:.ml4=.ml) $(GET_RANGE_MLT:.mlt=.ml) $(GET_RANGE_MLP:.mlcpp=.ml) $(GET_RANGE_MLL:.mll=.ml) $(GET_RANGE_MLY:.mly=.ml) $(GET_RANGE_MLY:.mly=.mli) $(GET_RANGE_ZOG:.zog=.ml) 
 
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
COPYSOURCES_MLT := $(filter %.mlt, $(COPYSOURCES_SRCS)) 
COPYSOURCES_MLP := $(filter %.mlcpp, $(COPYSOURCES_SRCS)) 
COPYSOURCES_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(COPYSOURCES_SRCS)) 
COPYSOURCES_C := $(filter %.c, $(COPYSOURCES_SRCS)) 
COPYSOURCES_CMOS=$(foreach file, $(COPYSOURCES_ML),   $(basename $(file)).cmo) 
COPYSOURCES_CMXS=$(foreach file, $(COPYSOURCES_ML),   $(basename $(file)).cmx) 
COPYSOURCES_OBJS=$(foreach file, $(COPYSOURCES_C),   $(basename $(file)).o)    

COPYSOURCES_CMXAS := $(foreach file, $(COPYSOURCES_CMXA),   build/$(basename $(file)).cmxa)
COPYSOURCES_CMAS=$(foreach file, $(COPYSOURCES_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(COPYSOURCES_ML4:.ml4=.ml) $(COPYSOURCES_MLT:.mlt=.ml) $(COPYSOURCES_MLP:.mlcpp=.ml) $(COPYSOURCES_MLL:.mll=.ml) $(COPYSOURCES_MLY:.mly=.ml) $(COPYSOURCES_MLY:.mly=.mli) $(COPYSOURCES_ZOG:.zog=.ml) 
 
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
USE_TAGS_MLT := $(filter %.mlt, $(USE_TAGS_SRCS)) 
USE_TAGS_MLP := $(filter %.mlcpp, $(USE_TAGS_SRCS)) 
USE_TAGS_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(USE_TAGS_SRCS)) 
USE_TAGS_C := $(filter %.c, $(USE_TAGS_SRCS)) 
USE_TAGS_CMOS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmo) 
USE_TAGS_CMXS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmx) 
USE_TAGS_OBJS=$(foreach file, $(USE_TAGS_C),   $(basename $(file)).o)    

USE_TAGS_CMXAS := $(foreach file, $(USE_TAGS_CMXA),   build/$(basename $(file)).cmxa)
USE_TAGS_CMAS=$(foreach file, $(USE_TAGS_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(USE_TAGS_ML4:.ml4=.ml) $(USE_TAGS_MLT:.mlt=.ml) $(USE_TAGS_MLP:.mlcpp=.ml) $(USE_TAGS_MLL:.mll=.ml) $(USE_TAGS_MLY:.mly=.ml) $(USE_TAGS_MLY:.mly=.mli) $(USE_TAGS_ZOG:.zog=.ml) 
 
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
HASH_FILES_MLT := $(filter %.mlt, $(HASH_FILES_SRCS)) 
HASH_FILES_MLP := $(filter %.mlcpp, $(HASH_FILES_SRCS)) 
HASH_FILES_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(HASH_FILES_SRCS)) 
HASH_FILES_C := $(filter %.c, $(HASH_FILES_SRCS)) 
HASH_FILES_CMOS=$(foreach file, $(HASH_FILES_ML),   $(basename $(file)).cmo) 
HASH_FILES_CMXS=$(foreach file, $(HASH_FILES_ML),   $(basename $(file)).cmx) 
HASH_FILES_OBJS=$(foreach file, $(HASH_FILES_C),   $(basename $(file)).o)    

HASH_FILES_CMXAS := $(foreach file, $(HASH_FILES_CMXA),   build/$(basename $(file)).cmxa)
HASH_FILES_CMAS=$(foreach file, $(HASH_FILES_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(HASH_FILES_ML4:.ml4=.ml) $(HASH_FILES_MLT:.mlt=.ml) $(HASH_FILES_MLP:.mlcpp=.ml) $(HASH_FILES_MLL:.mll=.ml) $(HASH_FILES_MLY:.mly=.ml) $(HASH_FILES_MLY:.mly=.mli) $(HASH_FILES_ZOG:.zog=.ml) 
 
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
INSTALLER_MLT := $(filter %.mlt, $(INSTALLER_SRCS)) 
INSTALLER_MLP := $(filter %.mlcpp, $(INSTALLER_SRCS)) 
INSTALLER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(INSTALLER_SRCS)) 
INSTALLER_C := $(filter %.c, $(INSTALLER_SRCS)) 
INSTALLER_CMOS=$(foreach file, $(INSTALLER_ML),   $(basename $(file)).cmo) 
INSTALLER_CMXS=$(foreach file, $(INSTALLER_ML),   $(basename $(file)).cmx) 
INSTALLER_OBJS=$(foreach file, $(INSTALLER_C),   $(basename $(file)).o)    

INSTALLER_CMXAS := $(foreach file, $(INSTALLER_CMXA),   build/$(basename $(file)).cmxa)
INSTALLER_CMAS=$(foreach file, $(INSTALLER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(INSTALLER_ML4:.ml4=.ml) $(INSTALLER_MLT:.mlt=.ml) $(INSTALLER_MLP:.mlcpp=.ml) $(INSTALLER_MLL:.mll=.ml) $(INSTALLER_MLY:.mly=.ml) $(INSTALLER_MLY:.mly=.mli) $(INSTALLER_ZOG:.zog=.ml) 
 
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
MLPIC_MLT := $(filter %.mlt, $(MLPIC_SRCS)) 
MLPIC_MLP := $(filter %.mlcpp, $(MLPIC_SRCS)) 
MLPIC_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(MLPIC_SRCS)) 
MLPIC_C := $(filter %.c, $(MLPIC_SRCS)) 
MLPIC_CMOS=$(foreach file, $(MLPIC_ML),   $(basename $(file)).cmo) 
MLPIC_CMXS=$(foreach file, $(MLPIC_ML),   $(basename $(file)).cmx) 
MLPIC_OBJS=$(foreach file, $(MLPIC_C),   $(basename $(file)).o)    

MLPIC_CMXAS := $(foreach file, $(MLPIC_CMXA),   build/$(basename $(file)).cmxa)
MLPIC_CMAS=$(foreach file, $(MLPIC_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(MLPIC_ML4:.ml4=.ml) $(MLPIC_MLT:.mlt=.ml) $(MLPIC_MLP:.mlcpp=.ml) $(MLPIC_MLL:.mll=.ml) $(MLPIC_MLY:.mly=.ml) $(MLPIC_MLY:.mly=.mli) $(MLPIC_ZOG:.zog=.ml) 
 
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
SPIDER_MLT := $(filter %.mlt, $(SPIDER_SRCS)) 
SPIDER_MLP := $(filter %.mlcpp, $(SPIDER_SRCS)) 
SPIDER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(SPIDER_SRCS)) 
SPIDER_C := $(filter %.c, $(SPIDER_SRCS)) 
SPIDER_CMOS=$(foreach file, $(SPIDER_ML),   $(basename $(file)).cmo) 
SPIDER_CMXS=$(foreach file, $(SPIDER_ML),   $(basename $(file)).cmx) 
SPIDER_OBJS=$(foreach file, $(SPIDER_C),   $(basename $(file)).o)    

SPIDER_CMXAS := $(foreach file, $(SPIDER_CMXA),   build/$(basename $(file)).cmxa)
SPIDER_CMAS=$(foreach file, $(SPIDER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(SPIDER_ML4:.ml4=.ml) $(SPIDER_MLT:.mlt=.ml) $(SPIDER_MLP:.mlcpp=.ml) $(SPIDER_MLL:.mll=.ml) $(SPIDER_MLY:.mly=.ml) $(SPIDER_MLY:.mly=.mli) $(SPIDER_ZOG:.zog=.ml) 
 
mlspider: $(SPIDER_OBJS) $(SPIDER_CMXS) $(SPIDER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(SPIDER_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(SPIDER_CMXAS) $(SPIDER_CMXS) 
 
mlspider.byte: $(SPIDER_OBJS) $(SPIDER_CMOS)  $(SPIDER_CMAS)
	$(OCAMLC) -linkall -o $@  $(SPIDER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(SPIDER_CMAS) $(SPIDER_CMOS) 
 
mlspider.static:  $(SPIDER_OBJS) $(SPIDER_CMXS)  $(SPIDER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(SPIDER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(SPIDER_CMXAS) $(SPIDER_CMXS)


DISASM_ZOG := $(filter %.zog, $(DISASM_SRCS)) 
DISASM_MLL := $(filter %.mll, $(DISASM_SRCS)) 
DISASM_MLY := $(filter %.mly, $(DISASM_SRCS)) 
DISASM_ML4 := $(filter %.ml4, $(DISASM_SRCS)) 
DISASM_MLT := $(filter %.mlt, $(DISASM_SRCS)) 
DISASM_MLP := $(filter %.mlcpp, $(DISASM_SRCS)) 
DISASM_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(DISASM_SRCS)) 
DISASM_C := $(filter %.c, $(DISASM_SRCS)) 
DISASM_CMOS=$(foreach file, $(DISASM_ML),   $(basename $(file)).cmo) 
DISASM_CMXS=$(foreach file, $(DISASM_ML),   $(basename $(file)).cmx) 
DISASM_OBJS=$(foreach file, $(DISASM_C),   $(basename $(file)).o)    

DISASM_CMXAS := $(foreach file, $(DISASM_CMXA),   build/$(basename $(file)).cmxa)
DISASM_CMAS=$(foreach file, $(DISASM_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(DISASM_ML4:.ml4=.ml) $(DISASM_MLT:.mlt=.ml) $(DISASM_MLP:.mlcpp=.ml) $(DISASM_MLL:.mll=.ml) $(DISASM_MLY:.mly=.ml) $(DISASM_MLY:.mly=.mli) $(DISASM_ZOG:.zog=.ml) 
 
disasm: $(DISASM_OBJS) $(DISASM_CMXS) $(DISASM_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(DISASM_OBJS) $(LIBS_opt) $(LIBS_flags) $(CURSES_LIBS_opt) $(CURSES_LIBS_flags) -I build $(DISASM_CMXAS) $(DISASM_CMXS) 
 
disasm.byte: $(DISASM_OBJS) $(DISASM_CMOS)  $(DISASM_CMAS)
	$(OCAMLC) -linkall -o $@  $(DISASM_OBJS) $(LIBS_byte) $(LIBS_flags)  $(CURSES_LIBS_byte) $(CURSES_LIBS_flags) -I build $(DISASM_CMAS) $(DISASM_CMOS) 
 
disasm.static:  $(DISASM_OBJS) $(DISASM_CMXS)  $(DISASM_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(DISASM_OBJS) $(LIBS_opt) $(LIBS_flags)  $(CURSES_LIBS_flags)  $(CURSES_STATIC_LIBS_opt) -I build $(DISASM_CMXAS) $(DISASM_CMXS)


ANALYSER1_ZOG := $(filter %.zog, $(ANALYSER1_SRCS)) 
ANALYSER1_MLL := $(filter %.mll, $(ANALYSER1_SRCS)) 
ANALYSER1_MLY := $(filter %.mly, $(ANALYSER1_SRCS)) 
ANALYSER1_ML4 := $(filter %.ml4, $(ANALYSER1_SRCS)) 
ANALYSER1_MLT := $(filter %.mlt, $(ANALYSER1_SRCS)) 
ANALYSER1_MLP := $(filter %.mlcpp, $(ANALYSER1_SRCS)) 
ANALYSER1_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(ANALYSER1_SRCS)) 
ANALYSER1_C := $(filter %.c, $(ANALYSER1_SRCS)) 
ANALYSER1_CMOS=$(foreach file, $(ANALYSER1_ML),   $(basename $(file)).cmo) 
ANALYSER1_CMXS=$(foreach file, $(ANALYSER1_ML),   $(basename $(file)).cmx) 
ANALYSER1_OBJS=$(foreach file, $(ANALYSER1_C),   $(basename $(file)).o)    

ANALYSER1_CMXAS := $(foreach file, $(ANALYSER1_CMXA),   build/$(basename $(file)).cmxa)
ANALYSER1_CMAS=$(foreach file, $(ANALYSER1_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(ANALYSER1_ML4:.ml4=.ml) $(ANALYSER1_MLT:.mlt=.ml) $(ANALYSER1_MLP:.mlcpp=.ml) $(ANALYSER1_MLL:.mll=.ml) $(ANALYSER1_MLY:.mly=.ml) $(ANALYSER1_MLY:.mly=.mli) $(ANALYSER1_ZOG:.zog=.ml) 
 
analyser1: $(ANALYSER1_OBJS) $(ANALYSER1_CMXS) $(ANALYSER1_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(ANALYSER1_OBJS) $(LIBS_opt) $(LIBS_flags) $(BIGARRAY_LIBS_opt) $(BIGARRAY_LIBS_flags) -I build $(ANALYSER1_CMXAS) $(ANALYSER1_CMXS) 
 
analyser1.byte: $(ANALYSER1_OBJS) $(ANALYSER1_CMOS)  $(ANALYSER1_CMAS)
	$(OCAMLC) -linkall -o $@  $(ANALYSER1_OBJS) $(LIBS_byte) $(LIBS_flags)  $(BIGARRAY_LIBS_byte) $(BIGARRAY_LIBS_flags) -I build $(ANALYSER1_CMAS) $(ANALYSER1_CMOS) 
 
analyser1.static:  $(ANALYSER1_OBJS) $(ANALYSER1_CMXS)  $(ANALYSER1_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(ANALYSER1_OBJS) $(LIBS_opt) $(LIBS_flags)  $(BIGARRAY_LIBS_flags)  $(BIGARRAY_STATIC_LIBS_opt) -I build $(ANALYSER1_CMXAS) $(ANALYSER1_CMXS)


BTVIEW_ZOG := $(filter %.zog, $(BTVIEW_SRCS)) 
BTVIEW_MLL := $(filter %.mll, $(BTVIEW_SRCS)) 
BTVIEW_MLY := $(filter %.mly, $(BTVIEW_SRCS)) 
BTVIEW_ML4 := $(filter %.ml4, $(BTVIEW_SRCS)) 
BTVIEW_MLT := $(filter %.mlt, $(BTVIEW_SRCS)) 
BTVIEW_MLP := $(filter %.mlcpp, $(BTVIEW_SRCS)) 
BTVIEW_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(BTVIEW_SRCS)) 
BTVIEW_C := $(filter %.c, $(BTVIEW_SRCS)) 
BTVIEW_CMOS=$(foreach file, $(BTVIEW_ML),   $(basename $(file)).cmo) 
BTVIEW_CMXS=$(foreach file, $(BTVIEW_ML),   $(basename $(file)).cmx) 
BTVIEW_OBJS=$(foreach file, $(BTVIEW_C),   $(basename $(file)).o)    

BTVIEW_CMXAS := $(foreach file, $(BTVIEW_CMXA),   build/$(basename $(file)).cmxa)
BTVIEW_CMAS=$(foreach file, $(BTVIEW_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(BTVIEW_ML4:.ml4=.ml) $(BTVIEW_MLT:.mlt=.ml) $(BTVIEW_MLP:.mlcpp=.ml) $(BTVIEW_MLL:.mll=.ml) $(BTVIEW_MLY:.mly=.ml) $(BTVIEW_MLY:.mly=.mli) $(BTVIEW_ZOG:.zog=.ml) 
 
btview: $(BTVIEW_OBJS) $(BTVIEW_CMXS) $(BTVIEW_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(BTVIEW_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(BTVIEW_CMXAS) $(BTVIEW_CMXS) 
 
btview.byte: $(BTVIEW_OBJS) $(BTVIEW_CMOS)  $(BTVIEW_CMAS)
	$(OCAMLC) -linkall -o $@  $(BTVIEW_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(BTVIEW_CMAS) $(BTVIEW_CMOS) 
 
btview.static:  $(BTVIEW_OBJS) $(BTVIEW_CMXS)  $(BTVIEW_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(BTVIEW_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(BTVIEW_CMXAS) $(BTVIEW_CMXS)


CLUSTER_ZOG := $(filter %.zog, $(CLUSTER_SRCS)) 
CLUSTER_MLL := $(filter %.mll, $(CLUSTER_SRCS)) 
CLUSTER_MLY := $(filter %.mly, $(CLUSTER_SRCS)) 
CLUSTER_ML4 := $(filter %.ml4, $(CLUSTER_SRCS)) 
CLUSTER_MLT := $(filter %.mlt, $(CLUSTER_SRCS)) 
CLUSTER_MLP := $(filter %.mlcpp, $(CLUSTER_SRCS)) 
CLUSTER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(CLUSTER_SRCS)) 
CLUSTER_C := $(filter %.c, $(CLUSTER_SRCS)) 
CLUSTER_CMOS=$(foreach file, $(CLUSTER_ML),   $(basename $(file)).cmo) 
CLUSTER_CMXS=$(foreach file, $(CLUSTER_ML),   $(basename $(file)).cmx) 
CLUSTER_OBJS=$(foreach file, $(CLUSTER_C),   $(basename $(file)).o)    

CLUSTER_CMXAS := $(foreach file, $(CLUSTER_CMXA),   build/$(basename $(file)).cmxa)
CLUSTER_CMAS=$(foreach file, $(CLUSTER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(CLUSTER_ML4:.ml4=.ml) $(CLUSTER_MLT:.mlt=.ml) $(CLUSTER_MLP:.mlcpp=.ml) $(CLUSTER_MLL:.mll=.ml) $(CLUSTER_MLY:.mly=.ml) $(CLUSTER_MLY:.mly=.mli) $(CLUSTER_ZOG:.zog=.ml) 
 
cluster: $(CLUSTER_OBJS) $(CLUSTER_CMXS) $(CLUSTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(CLUSTER_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(CLUSTER_CMXAS) $(CLUSTER_CMXS) 
 
cluster.byte: $(CLUSTER_OBJS) $(CLUSTER_CMOS)  $(CLUSTER_CMAS)
	$(OCAMLC) -linkall -o $@  $(CLUSTER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(CLUSTER_CMAS) $(CLUSTER_CMOS) 
 
cluster.static:  $(CLUSTER_OBJS) $(CLUSTER_CMXS)  $(CLUSTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(CLUSTER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(CLUSTER_CMXAS) $(CLUSTER_CMXS)


DP500_ZOG := $(filter %.zog, $(DP500_SRCS)) 
DP500_MLL := $(filter %.mll, $(DP500_SRCS)) 
DP500_MLY := $(filter %.mly, $(DP500_SRCS)) 
DP500_ML4 := $(filter %.ml4, $(DP500_SRCS)) 
DP500_MLT := $(filter %.mlt, $(DP500_SRCS)) 
DP500_MLP := $(filter %.mlcpp, $(DP500_SRCS)) 
DP500_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(DP500_SRCS)) 
DP500_C := $(filter %.c, $(DP500_SRCS)) 
DP500_CMOS=$(foreach file, $(DP500_ML),   $(basename $(file)).cmo) 
DP500_CMXS=$(foreach file, $(DP500_ML),   $(basename $(file)).cmx) 
DP500_OBJS=$(foreach file, $(DP500_C),   $(basename $(file)).o)    

DP500_CMXAS := $(foreach file, $(DP500_CMXA),   build/$(basename $(file)).cmxa)
DP500_CMAS=$(foreach file, $(DP500_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(DP500_ML4:.ml4=.ml) $(DP500_MLT:.mlt=.ml) $(DP500_MLP:.mlcpp=.ml) $(DP500_MLL:.mll=.ml) $(DP500_MLY:.mly=.ml) $(DP500_MLY:.mly=.mli) $(DP500_ZOG:.zog=.ml) 
 
dp500: $(DP500_OBJS) $(DP500_CMXS) $(DP500_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(DP500_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(DP500_CMXAS) $(DP500_CMXS) 
 
dp500.byte: $(DP500_OBJS) $(DP500_CMOS)  $(DP500_CMAS)
	$(OCAMLC) -linkall -o $@  $(DP500_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(DP500_CMAS) $(DP500_CMOS) 
 
dp500.static:  $(DP500_OBJS) $(DP500_CMXS)  $(DP500_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(DP500_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(DP500_CMXAS) $(DP500_CMXS)


TESTRSS_ZOG := $(filter %.zog, $(TESTRSS_SRCS)) 
TESTRSS_MLL := $(filter %.mll, $(TESTRSS_SRCS)) 
TESTRSS_MLY := $(filter %.mly, $(TESTRSS_SRCS)) 
TESTRSS_ML4 := $(filter %.ml4, $(TESTRSS_SRCS)) 
TESTRSS_MLT := $(filter %.mlt, $(TESTRSS_SRCS)) 
TESTRSS_MLP := $(filter %.mlcpp, $(TESTRSS_SRCS)) 
TESTRSS_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(TESTRSS_SRCS)) 
TESTRSS_C := $(filter %.c, $(TESTRSS_SRCS)) 
TESTRSS_CMOS=$(foreach file, $(TESTRSS_ML),   $(basename $(file)).cmo) 
TESTRSS_CMXS=$(foreach file, $(TESTRSS_ML),   $(basename $(file)).cmx) 
TESTRSS_OBJS=$(foreach file, $(TESTRSS_C),   $(basename $(file)).o)    

TESTRSS_CMXAS := $(foreach file, $(TESTRSS_CMXA),   build/$(basename $(file)).cmxa)
TESTRSS_CMAS=$(foreach file, $(TESTRSS_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(TESTRSS_ML4:.ml4=.ml) $(TESTRSS_MLT:.mlt=.ml) $(TESTRSS_MLP:.mlcpp=.ml) $(TESTRSS_MLL:.mll=.ml) $(TESTRSS_MLY:.mly=.ml) $(TESTRSS_MLY:.mly=.mli) $(TESTRSS_ZOG:.zog=.ml) 
 
testrss: $(TESTRSS_OBJS) $(TESTRSS_CMXS) $(TESTRSS_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(TESTRSS_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(TESTRSS_CMXAS) $(TESTRSS_CMXS) 
 
testrss.byte: $(TESTRSS_OBJS) $(TESTRSS_CMOS)  $(TESTRSS_CMAS)
	$(OCAMLC) -linkall -o $@  $(TESTRSS_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(TESTRSS_CMAS) $(TESTRSS_CMOS) 
 
testrss.static:  $(TESTRSS_OBJS) $(TESTRSS_CMXS)  $(TESTRSS_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(TESTRSS_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(TESTRSS_CMXAS) $(TESTRSS_CMXS)


SVG_CONVERTER_ZOG := $(filter %.zog, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_MLL := $(filter %.mll, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_MLY := $(filter %.mly, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_ML4 := $(filter %.ml4, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_MLT := $(filter %.mlt, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_MLP := $(filter %.mlcpp, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_ML := $(filter %.ml %.mll %.zog %.mly %.ml4 %.mlt %.mlcpp, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_C := $(filter %.c, $(SVG_CONVERTER_SRCS)) 
SVG_CONVERTER_CMOS=$(foreach file, $(SVG_CONVERTER_ML),   $(basename $(file)).cmo) 
SVG_CONVERTER_CMXS=$(foreach file, $(SVG_CONVERTER_ML),   $(basename $(file)).cmx) 
SVG_CONVERTER_OBJS=$(foreach file, $(SVG_CONVERTER_C),   $(basename $(file)).o)    

SVG_CONVERTER_CMXAS := $(foreach file, $(SVG_CONVERTER_CMXA),   build/$(basename $(file)).cmxa)
SVG_CONVERTER_CMAS=$(foreach file, $(SVG_CONVERTER_CMXA),   build/$(basename $(file)).cma)    

TMPSOURCES += $(SVG_CONVERTER_ML4:.ml4=.ml) $(SVG_CONVERTER_MLT:.mlt=.ml) $(SVG_CONVERTER_MLP:.mlcpp=.ml) $(SVG_CONVERTER_MLL:.mll=.ml) $(SVG_CONVERTER_MLY:.mly=.ml) $(SVG_CONVERTER_MLY:.mly=.mli) $(SVG_CONVERTER_ZOG:.zog=.ml) 
 
svg_converter: $(SVG_CONVERTER_OBJS) $(SVG_CONVERTER_CMXS) $(SVG_CONVERTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(SVG_CONVERTER_OBJS) $(LIBS_opt) $(LIBS_flags) $(_LIBS_opt) $(_LIBS_flags) -I build $(SVG_CONVERTER_CMXAS) $(SVG_CONVERTER_CMXS) 
 
svg_converter.byte: $(SVG_CONVERTER_OBJS) $(SVG_CONVERTER_CMOS)  $(SVG_CONVERTER_CMAS)
	$(OCAMLC) -linkall -o $@  $(SVG_CONVERTER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(_LIBS_byte) $(_LIBS_flags) -I build $(SVG_CONVERTER_CMAS) $(SVG_CONVERTER_CMOS) 
 
svg_converter.static:  $(SVG_CONVERTER_OBJS) $(SVG_CONVERTER_CMXS)  $(SVG_CONVERTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -ccopt -static -o $@ $(SVG_CONVERTER_OBJS) $(LIBS_opt) $(LIBS_flags)  $(_LIBS_flags)  $(_STATIC_LIBS_opt) -I build $(SVG_CONVERTER_CMXAS) $(SVG_CONVERTER_CMXS)



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
	$(OCAMLMKTOP) -linkall $(PLUGIN_FLAG) -o $@  $(TOP_OBJS) $(LIBS_byte) $(LIBS_flags) $(_LIBS_byte) $(_LIBS_flags) -I build $(TOP_CMAS) $(TOP_CMOS) 



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
	rm -f svg_converter ed2k_hash make_torrent copysources get_range subconv dp500
	(for i in $(SUBDIRS); do \
		rm -f  $$i/*.cm? $$i/*.o $$i/*.annot ; \
	done)



tmpclean: 
	rm -f $(TMPSOURCES)

moreclean: clean tmpclean

releaseclean: clean moreclean
	rm -f .depend
	rm -rf patches/build
	rm -f config/Makefile.config
	rm -f config/mldonkey.rc
	rm -f config/config.cache config/config.log config/config.status
	rm -f config/config.h
	rm -f config/confdefs.h
	rm -rf config/autom4te.cache/
	rm -f packages/rpm/*.spec
	rm -f packages/windows/mlnet.nsi
	rm -f src/daemon/common/commonDownloads.ml
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
	rm -f icons/rsvg/*.ml_icons
	rm -f icons/rsvg/*.ml
	rm -f tools/zoggy/*.cm?
	rm -rf mldonkey-distrib*
	rm -f mldonkey-$(CURRENT_VERSION).*

distclean: releaseclean
	rm -rf patches/local

maintainerclean: distclean
	echo rm -f $(GUI)/gui.ml $(GUI)/gui_zog.ml
	rm -f config/configure
	rm -f packages/rpm/Makefile
	rm -f Makefile

LOCAL=patches/build

PA_ZOG_FILES=tools/zoggy/zog_types.ml tools/zoggy/zog_messages.ml tools/zoggy/zog_misc.ml tools/zoggy/pa_zog.ml

pa_zog.cma: $(PA_ZOG_FILES)
	$(OCAMLC) -I tools/zoggy -I +camlp4 -pp "$(CAMLP4) pa_o.cmo pr_dump.cmo" -a -o pa_zog.cma  $(PA_ZOG_FILES)


OCAMLPP=./ocamlpp.byte

$(ZOGSOURCES): pa_zog.cma
$(MLTSOURCES): $(OCAMLPP)
#$(TMPSOURCES): $(OCAMLPP)

#ocamlpp.byte: tools/ocamlpp.ml
#	$(OCAMLC) str.cma -o ocamlpp.byte tools/ocamlpp.ml

depend:  pa_zog.cma $(LIB)/http_lexer.ml $(TMPSOURCES) $(TMPFILES)
	$(OCAMLDEP) $(OCAMLDEP_OPTIONS) $(patsubst -I +labl$(GTK),,$(INCLUDES)) *.ml *.mli > .depend
	(for i in $(SUBDIRS); do \
		$(OCAMLDEP) $(OCAMLDEP_OPTIONS) $(patsubst -I +labl$(GTK),,$(INCLUDES)) $$i/*.ml $$i/*.mli  >> .depend; \
		$(OCAMLPP) $$i/*.mlt  >> .depend; \
	done)
	if test "$(COMPILE_GUI)" = "yes"; then \
		if test "$(USE_GTK2)" = "yes"; then \
			$(MAKE) svg_converter; \
		fi; \
	fi

$(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/Makefile: patches/ocamlopt-$(REQUIRED_OCAML).tar.gz
	rm -rf $(LOCAL)/ocamlopt-$(REQUIRED_OCAML)
	mkdir -p $(LOCAL)
	cd $(LOCAL); \
	gzip -cd ../ocamlopt-$(REQUIRED_OCAML).tar.gz | tar xf -; \
	touch ocamlopt-$(REQUIRED_OCAML)/Makefile

$(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/ocamlopt: $(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/Makefile
	cd $(LOCAL)/ocamlopt-$(REQUIRED_OCAML); $(MAKE)

utils: svg_converter ed2k_hash make_torrent copysources get_range subconv dp500
utils.byte: ed2k_hash.byte make_torrent.byte copysources.byte get_range.byte subconv.byte dp500.byte
utils.static: svg_converter ed2k_hash.static make_torrent.static copysources.static get_range.static subconv.static dp500.static

#######################################################################

#                      Building binary distribs

#######################################################################

DISDIR=mldonkey-distrib
#distrib/Readme.txt: $(GUI)/gui_messages.ml
#	grep -A 1000 help_text $(GUI)/gui_messages.ml | grep -v '"' > distrib/Readme.txt


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
	./configure --host=i386-pc-linux-gnu
	rm -f mlnet mlnet.static mlnet+gui mlnet+gui.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i686
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	./configure --host=i686-pc-linux-gnu
	rm -f  mlnet+gui mlnet+gui.static mlnet mlnet.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i586
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	./configure --host=i586-pc-linux-gnu
	rm -f  mlnet+gui mlnet+gui.static mlnet mlnet.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i486
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	./configure --host=i486-pc-linux-gnu
	rm -f  mlnet+gui mlnet+gui.static mlnet mlnet.static $(LIB)/md4_comp.* $(LIB)/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i486-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i486-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/

buildrpm: 
	./configure --host=i586-pc-linux-gnu
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

.SUFFIXES: .mli .ml .cmx .cmo .o .c .cmi .mll .mly .zog .plugindep .xpm .ml .cc .ml_icons .ml4 .mlt .mlii .mlcpp .svg

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

.svg.ml_icons :
	cp $< $@
	./svg_converter $@

.ml.cmx :
	$(OCAMLOPT) $(PLUGIN_FLAG) $(OFLAGS) $(INCLUDES) -c $<

.ml.cmo :
	$(OCAMLC) $(DEVFLAGS) $(OFLAGS) $(INCLUDES) -c $<

.mlcpp.ml:
	cpp $< > $@

.mll.ml :
	$(OCAMLLEX) $<

.mly.ml :
	$(OCAMLYACC) $<

.mly.mli:
	$(OCAMLYACC) $<

.zog.ml:
	$(CAMLP4) pa_o.cmo ./pa_zog.cma pr_o.cmo -impl $< > $@

.ml4.ml:
	echo '# 1 "$<"' > $@
	$(CAMLP4) pa_o.cmo pa_op.cmo pr_o.cmo -impl $< >> $@

.mlt.ml:
	$(OCAMLPP) -pp $< > $@

.c.o :
	$(OCAMLC) -verbose -ccopt "-I $(OCAML_SRC)/byterun -o $*.o" -ccopt "$(CFLAGS)" $(LIBS_flags) -c $<

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
