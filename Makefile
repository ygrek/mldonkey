
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

ICONS_CHOICE=kde


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

SUBDIRS=cdk chat lib net tools common driver mp3tagui config/$(OS_FILES)

INCLUDES += $(foreach file, $(SUBDIRS), -I $(file))

CFLAGS:=$(CFLAGS) $(CONFIG_INCLUDES)

# use_tags$(EXE) 
TARGETS= mlnet$(EXE) 


#######################################################################

##              Objects files for "mldonkey"

#######################################################################


ifeq ("$(OS_FILES)", "mingw")
  LIBS_flags += -cclib  -lws2_32
#  LIBS_byte += -cclib -lws2_32
endif

ifeq ("$(ZLIB)" , "yes")
  LIBS_flags += -cclib -lz
#  LIBS_byte += -cclib -lz
  CDK_SRCS +=  cdk/zlib.ml cdk/zlibstubs.c
endif


ifeq ("$(ICONV)" , "yes")
  LIBS_flags += -cclib -liconv
#  LIBS_byte += -cclib -liconv
endif

CDK_SRCS += lib/autoconf.ml

CDK_SRCS+= lib/fifo.ml  cdk/printf2.ml \
   cdk/heap.ml cdk/dprintf.ml \
  cdk/printexc2.ml cdk/genlex2.ml cdk/sysenv.ml \
  cdk/netbase.ml cdk/filepath.ml cdk/string2.ml \
  cdk/filename2.ml cdk/list2.ml cdk/hashtbl2.ml \
  cdk/file.ml cdk/unix2.ml cdk/weak2.ml \
  cdk/heap_c.c cdk/array2.ml cdk/sort2.ml \
  cdk/xmllex.mll cdk/xmlyacc.mly cdk/xml.ml

ifneq ("$(PTHREAD_CFLAGS)" , "")
  CFLAGS += $(PTHREAD_CFLAGS)
  LIBS_flags += -ccopt "$(PTHREAD_CFLAGS)"
#  LIBS_byte += -ccopt "$(PTHREAD_CFLAGS)"
endif

ifneq ("$(PTHREAD_LIBS)" , "")
  LIBS_flags += -cclib "$(PTHREAD_LIBS)"
#  LIBS_byte += -cclib "$(PTHREAD_LIBS)"
endif

MP3TAG_SRCS=     mp3tagui/mp3_info.ml  mp3tagui/mp3_genres.ml \
  mp3tagui/mp3_misc.ml\
  mp3tagui/mp3_tag.ml mp3tagui/mp3tag.ml


LIB_SRCS=   \
  config/$(OS_FILES)/mlUnix.ml \
  config/$(OS_FILES)/os_stubs_c.c \
  lib/intmap.ml lib/stringMap.ml \
  lib/int32ops.ml lib/options.ml4 lib/numset.ml  \
  lib/fifo2.ml lib/intset.ml \
  lib/hole_tab.ml lib/store.ml \
  lib/indexer.ml lib/indexer1.ml lib/indexer2.ml lib/host.ml  \
  lib/misc.ml lib/unix32.ml  lib/md4.ml \
  lib/avifile.ml lib/http_lexer.mll lib/url.ml \
  lib/date.ml \
  lib/md4_comp.c lib/md4_c.c \
  lib/gettext.ml lib/md5_c.c lib/sha1_c.c \
  lib/tiger.c \
  lib/stubs_c.c

NET_SRCS = \
  net/basicSocket.ml \
  net/ip.ml net/mailer.ml \
  net/anyEndian.ml net/bigEndian.ml net/littleEndian.ml \
  net/tcpBufferedSocket.ml \
  net/tcpClientSocket.ml net/tcpServerSocket.ml \
  net/udpSocket.ml net/http_server.ml net/http_client.ml \
  net/multicast.ml net/multicast_c.c  \
  net/terminal.ml


CHAT_SRCS = chat/chat_messages.ml\
	chat/chat_misc.ml\
        chat/chat_proto.ml\
        chat/chat_types.ml\
        chat/chat_options.ml\
        chat/chat_config.ml\

COMMON_SRCS=common/commonTypes.ml \
  common/guiTypes.ml \
  common/guiProto.ml \
  common/commonEvent.ml \
  common/commonOptions.ml \
  common/commonMessages.ml \
  common/commonGlobals.ml \
  common/guiDecoding.ml \
  common/guiEncoding.ml \
  common/commonChat.ml \
  common/commonHasher.ml \
  common/commonHasher_c.c

COMMON_CLIENT_SRCS= \
  common/commonUser.ml \
  common/commonServer.ml \
  common/commonClient.ml \
  common/commonFile.ml \
  common/commonResult.ml \
  common/commonNetwork.ml \
  common/commonShared.ml \
  common/commonRoom.ml \
  common/commonComplexOptions.ml \
  common/commonSearch.ml \
  common/commonMultimedia.ml \
  common/commonInteractive.ml \
  common/commonDownloads.ml \
  common/commonUploads.ml \
  common/commonSwarming.ml

all: Makefile config/Makefile.config $(TARGET_TYPE)

config/configure: config/configure.in
	cd config; autoconf

ifeq ("$(CONFIG_ARGS_DEFINED)" , "yes")

config/Makefile.config: config/configure config/Makefile.config.in lib/autoconf.ml.new.in
	./configure $(CONFIG_ARGS)

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

MAIN_SRCS=common/commonMain.ml

DONKEY_SRCS= \
  \
  donkey/donkeyTypes.ml \
  donkey/donkeyOptions.ml \
  donkey/donkeyMftp.ml donkey/donkeyImport.ml \
  donkey/donkeyOpenProtocol.ml \
  donkey/donkeyProtoClient.ml \
  donkey/donkeyProtoServer.ml  \
  donkey/donkeyProtoUdp.ml  \
  \
  donkey/donkeyGlobals.ml \
  donkey/donkeyProtoCom.ml  \
  donkey/donkeySourcesMisc.ml \
  donkey/donkeySources1.ml  \
  donkey/donkeySources2.ml  \
  donkey/donkeySources3.ml  \
  donkey/donkeySources.ml  \
 \
  donkey/donkeyComplexOptions.ml \
  donkey/donkeySupernode.ml \
  donkey/donkeyIndexer.ml \
  donkey/donkeyShare.ml \
  donkey/donkeyReliability.ml \
  donkey/donkeyChunks.ml \
  donkey/donkeyOneFile.ml \
  donkey/donkeyStats.ml \
  donkey/donkeyClient.ml \
  donkey/donkeyProtoOvernet.ml \
  \
  donkey/donkeyOvernet.ml \
  donkey/donkeyFiles.ml  \
  donkey/donkeyServers.ml \
  donkey/donkeySearch.ml \
  donkey/donkeyInteractive.ml \
  donkey/donkeyMain.ml


OBSERVER_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(COMMON_CLIENT_SRCS) $(DONKEY_SRCS) \
  tools/observer.ml


DONKEY_SERVER_SRCS=\
  server/serverTypes.ml \
  server/serverOptions.ml \
  server/serverGlobals.ml \
  server/serverMessages.ml \
  server/serverLocate.ml \
  server/serverIndexer.ml \
  server/serverLog.ml \
  server/serverSubscriptions.ml \
  server/serverServer.ml \
  server/serverClients.ml \
  server/serverUdp.ml  \
  server/serverMain.ml

OPENNAP_SRCS=opennap/napigator.mll \
 opennap/opennapTypes.ml \
 opennap/opennapProtocol.ml \
 opennap/opennapOptions.ml \
 opennap/opennapGlobals.ml \
 opennap/opennapComplexOptions.ml \
 opennap/opennapClients.ml \
 opennap/opennapServers.ml \
 opennap/opennapInteractive.ml \
 opennap/opennapMain.ml 

GNUTELLA_SRCS= \
  gnutella/gnutellaTypes.ml \
  gnutella/gnutellaOptions.ml \
  gnutella/gnutellaGlobals.ml \
  gnutella/gnutellaComplexOptions.ml \
  gnutella/gnutellaProtocol.ml \
  gnutella/gnutella1.ml \
  gnutella/gnutella2.ml \
  gnutella/gnutella.ml \
  gnutella/gnutellaClients.ml \
  gnutella/gnutella1Handler.ml \
  gnutella/gnutella2Handler.ml \
  gnutella/gnutella1Redirector.ml \
  gnutella/gnutella2Redirector.ml \
  gnutella/gnutellaServers.ml \
  gnutella/gnutellaInteractive.ml \
  gnutella/gnutellaMain.ml

BITTORRENT_SRCS= \
  bittorrent/bencode.ml \
  bittorrent/bTTypes.ml \
  bittorrent/bTOptions.ml \
  bittorrent/bTGlobals.ml \
  bittorrent/bTComplexOptions.ml \
  bittorrent/bTProtocol.ml \
  bittorrent/bTClients.ml \
  bittorrent/bTInteractive.ml \
  bittorrent/bTMain.ml
  
OPENFT_SRCS= \
  openFT/openFTTypes.ml \
  openFT/openFTOptions.ml \
  openFT/openFTGlobals.ml \
  openFT/openFTComplexOptions.ml \
  openFT/openFTProtocol.ml \
  openFT/openFTClients.ml \
  openFT/openFTServers.ml \
  openFT/openFTInteractive.ml \
  openFT/openFTMain.ml

SOULSEEK_SRCS= \
  soulseek/slskTypes.ml \
  soulseek/slskOptions.ml \
  soulseek/slskGlobals.ml \
  soulseek/slskComplexOptions.ml \
  soulseek/slskProtocol.ml \
  soulseek/slskClients.ml \
  soulseek/slskServers.ml \
  soulseek/slskInteractive.ml \
  soulseek/slskMain.ml

DIRECT_CONNECT_SRCS= \
  direct_connect/dcTypes.ml \
  direct_connect/dcOptions.ml \
  direct_connect/che3_c.c \
  direct_connect/che3.ml \
  direct_connect/dcGlobals.ml \
  direct_connect/dcComplexOptions.ml \
  direct_connect/dcKey.ml \
  direct_connect/dcProtocol.ml \
  direct_connect/dcClients.ml \
  direct_connect/dcServers.ml \
  direct_connect/dcInteractive.ml \
  direct_connect/dcMain.ml

AUDIOGALAXY_SRCS=audio_galaxy/agTypes.ml \
  audio_galaxy/agOptions.ml \
  audio_galaxy/agGlobals.ml \
  audio_galaxy/agComplexOptions.ml \
  audio_galaxy/agProtocol.ml \
  audio_galaxy/agClients.ml \
  audio_galaxy/agServers.ml \
  audio_galaxy/agInteractive.ml \
  audio_galaxy/agHttpForward.ml \
  audio_galaxy/agMain.ml


CYMES_SRCS=\
  cymes/cymesTypes.ml \
  cymes/cymesOptions.ml \
  cymes/cymesGlobals.ml \
  cymes/cymesProtocol.ml \
  cymes/cymesMessages.ml \
  cymes/cymesLocate.ml \
  cymes/cymesIndexer.ml \
  cymes/cymesLog.ml \
  cymes/cymesSubscriptions.ml \
  cymes/cymesServer.ml \
  cymes/cymesClients.ml \
  cymes/cymesMain.ml

KDE_APPLET=yes


ifeq ("$(CYMES)" , "yes")
SUBDIRS += cymes
CORE_SRCS += $(CYMES_SRCS)
endif

ifeq ("$(OPENFT)" , "yes")
SUBDIRS += openFT

CORE_SRCS += $(OPENFT_SRCS)

endif

ifeq ("$(AUDIO_GALAXY)" , "yes")
SUBDIRS += audio_galaxy

CORE_SRCS += $(AUDIOGALAXY_SRCS)
endif

DRIVER_SRCS= \
  driver/driverInteractive.ml  \
  driver/driverCommands.ml  \
  driver/driverControlers.ml  \
  driver/driverInterface.ml \
  driver/driverMain.ml 

MLCAST_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  tools/mlcast.ml

CDK_CMXA=cdk.cmxa
MLNET_CMXA=cdk.cmxa common.cmxa client.cmxa core.cmxa driver.cmxa
MLNET_SRCS= $(MAIN_SRCS)

mlnet+gui_CMXA=cdk.cmxa common.cmxa client.cmxa core.cmxa driver.cmxa \
  gmisc.cmxa guibase.cmxa gui.cmxa
mlnet+gui_SRCS=$(MAIN_SRCS)



#######################################################################

#              Objects files for "mldonkey_gui"

#######################################################################


ifeq ("$(COMPILE_GUI)" , "yes")

SUBDIRS += gui gui2 configwin okey gpattern icons/$(ICONS_CHOICE) +lablgtk

GTK_LIBS_byte=-I +lablgtk $(LABLGL_CMA) lablgtk.cma
GTK_LIBS_opt=-I +lablgtk  $(LABLGL_CMXA) lablgtk.cmxa
GTK_STATIC_LIBS_opt=-I +lablgtk lablgtk.cmxa


CONFIGWIN_SRCS=configwin/configwin_types.ml \
  configwin/configwin_messages.ml \
  configwin/configwin_ihm.ml configwin/configwin.ml

MP3TAGUI_SRCS=  mp3tagui/mp3_messages.ml mp3tagui/mp3_ui.ml

GPATTERN_SRCS=  lib/gAutoconf.ml gpattern/gpattern.ml

OKEY_SRCS= okey/okey.ml

ICONS= \
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
 
SMALL_ICONS= \
  icons/small/add_to_friends_small.xpm icons/small/cancel_small.xpm \
  icons/small/connect_more_small.xpm icons/small/connect_small.xpm \
  icons/small/disconnect_small.xpm icons/small/download_small.xpm \
  icons/small/edit_mp3_small.xpm icons/small/extend_search_small.xpm \
  icons/small/get_format_small.xpm icons/small/local_search_small.xpm \
  icons/small/preview_small.xpm icons/small/refres_small.xpm \
  icons/small/save_all_small.xpm icons/small/save_as_small.xpm icons/small/save_small.xpm \
  icons/small/trash_small.xpm icons/small/verify_chunks_small.xpm \
  icons/small/view_users_small.xpm


ALL_ICONS=$(foreach file, $(ICONS),   $(basename $(file)).ml_icons)
ALL_ICONS_SRCS=$(foreach file, $(ICONS),   $(basename $(file))_xpm.ml)

$(ALL_ICONS_SRCS): $(ALL_ICONS)

GUI_BASE_SRCS= \
  gui/gui_messages.ml   gui/gui_global.ml \
  gui/gui_columns.ml \
  gui/gui_keys.ml \
  $(ALL_ICONS_SRCS) gui/gui_options.ml 

GUI_SRCS=  \
  gui/gui_misc.ml \
  gui/gui_com.ml \
  gui/gui_help_base.zog gui/gui_help.ml \
  gui/gui_console_base.zog gui/gui_console.ml \
  gui/gui_uploads_base.zog gui/gui_uploads.ml \
  gui/gui_users_base.zog gui/gui_users.ml \
  gui/gui_results_base.zog gui/gui_results.ml \
  gui/gui_rooms_base.zog gui/gui_rooms.ml \
  gui/gui_friends_base.zog gui/gui_friends.ml \
  gui/gui_cdget_base.zog gui/gui_cdget.ml \
  gui/gui_queries_base.zog gui/gui_queries.ml \
  gui/gui_servers_base.zog gui/gui_servers.ml \
  gui/gui_downloads_base.zog gui/gui_downloads.ml \
  gui/gui_window_base.zog gui/gui_window.ml \
  $(IM_GUI_CORE) \
  gui/gui_config.ml \
  gui/gui_main.ml

GUI2_SRCS= gui2/gui2_messages.ml gui2/gui2_keys.ml \
  gui2/gui2_options.ml gui2/gui2_GList.ml gui2/gui2.zog \
  gui2/myCList.ml gui2/gui2_handler.ml \
  gui2/gui2_misc.ml gui2/gui2_config.ml \
  gui2/gui2_main.ml

MLDONKEYGUI_CMXA= cdk.cmxa gmisc.cmxa common.cmxa guibase.cmxa gui.cmxa
MLDONKEYGUI_SRCS= $(MAIN_SRCS)

MLDONKEYGUI2_CMXA= cdk.cmxa gmisc.cmxa common.cmxa guibase.cmxa
MLDONKEYGUI2_SRCS= $(GUI2_SRCS) $(MAIN_SRCS)

MLDONKEY_IM_CMXA= cdk.cmxa gmisc.cmxa common.cmxa guibase.cmxa
MLDONKEY_IM_SRCS= \
   $(GUI_BASE_SRCS) $(IM_GUI_CORE) im/gui_im_main.ml  $(MAIN_SRCS)

STARTER_SRCS= gui/gui_starter.ml

INSTALLER_CMXA= cdk.cmxa gmisc.cmxa common.cmxa
INSTALLER_SRCS= \
  $(GUI_BASE_SRCS) gui/gui_installer_base.zog gui/gui_installer.ml


#######################################################################

#              Objects files for "mlchat"

#######################################################################


CHAT_EXE_SRCS= \
        chat/chat_data.ml\
        chat/chat_icons.ml\
        chat/chat_gui_base.ml\
        chat/chat_gui.ml\
        chat/chat_app.ml \
        chat/mlchat.ml \
	chat/chat_args.ml \
	chat/chat_main.ml

MLCHAT_CMXA= cdk.cmxa gmisc.cmxa
MLCHAT_SRCS=  $(CHAT_SRCS) $(CHAT_EXE_SRCS)


TARGETS += mldonkey_gui$(EXE)   mldonkey_gui2$(EXE)  mlchat$(EXE) mldonkey_guistarter$(EXE) mlnet+gui$(EXE)

ifeq ("$(DEVEL)", "yes")

TARGETS += mldonkey_installer$(EXE)

endif


#### IM stuff is now automatically included in the GUI


SUBDIRS += im im/yahoo  im/irc
  
IM_CORE += im/imTypes.ml im/imEvent.ml \
   im/imProtocol.ml im/imIdentity.ml im/imAccount.ml \
   im/imChat.ml im/imRoom.ml \
   im/imOptions.ml

IM_CORE +=  im/irc/irc.ml

IM_GUI_CORE += im/gui_im_base.zog   \
    im/gui_im.ml

TARGETS += mlim

ifeq ("$(DEVEL)", "yes")
  SUBDIRS += im/msn

  IM_CORE +=    im/yahoo/yahoo.ml   im/msn/msn.ml
endif

IM_CORE +=   im/imMain.ml

endif


TOP_CMXA=cdk.cmxa common.cmxa client.cmxa
TOP_SRCS= 








ifeq ("$(DIRECT_CONNECT)" , "yes")
SUBDIRS += direct_connect

CORE_SRCS += $(DIRECT_CONNECT_SRCS)

TARGETS += mldc$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

TARGETS += mldc+gui$(EXE)

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
   gmisc.cmxa guibase.cmxa gui.cmxa
mldc+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(OPENNAP)" , "yes")
SUBDIRS += opennap

CORE_SRCS += $(OPENNAP_SRCS)

TARGETS += mlnap$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

TARGETS += mlnap+gui$(EXE)

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
   gmisc.cmxa guibase.cmxa gui.cmxa
mlnap+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(GNUTELLA)" , "yes")
SUBDIRS += gnutella

CORE_SRCS += $(GNUTELLA_SRCS)

TARGETS += mlgnut$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

TARGETS += mlgnut+gui$(EXE)

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
   gmisc.cmxa guibase.cmxa gui.cmxa
mlgnut+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(BITTORRENT)" , "yes")
SUBDIRS += bittorrent

CORE_SRCS += $(BITTORRENT_SRCS)

TARGETS += mlbt$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

TARGETS += mlbt+gui$(EXE)

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
   gmisc.cmxa guibase.cmxa gui.cmxa
mlbt+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(DONKEY)" , "yes")
SUBDIRS += donkey

CORE_SRCS += $(DONKEY_SRCS)

TARGETS += mldonkey$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

TARGETS += mldonkey+gui$(EXE)

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
   gmisc.cmxa guibase.cmxa gui.cmxa
mldonkey+gui_SRCS= $(MAIN_SRCS)




ifeq ("$(SOULSEEK)" , "yes")
SUBDIRS += soulseek

CORE_SRCS += $(SOULSEEK_SRCS)

TARGETS += mlslsk$(EXE)

ifeq ("$(COMPILE_GUI)" , "yes")

TARGETS += mlslsk+gui$(EXE)

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
   gmisc.cmxa guibase.cmxa gui.cmxa
mlslsk+gui_SRCS= $(MAIN_SRCS)



libcdk_SRCS=  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) $(MP3TAG_SRCS)
libcommon_SRCS= $(CHAT_SRCS) $(COMMON_SRCS)
libclient_SRCS= $(COMMON_CLIENT_SRCS)
libgmisc_SRCS=  $(CONFIGWIN_SRCS) $(MP3TAGUI_SRCS) \
  $(OKEY_SRCS) $(GPATTERN_SRCS)
libguibase_SRCS= $(IM_CORE)
libgui_SRCS=   $(GUI_BASE_SRCS) $(GUI_SRCS)


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
  lib/cddb_lexer.mll lib/cddb_file.ml \
  tools/use_tags.ml

HASH_FILES_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  donkey/donkeyHasher.ml tools/hash_files.ml


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
 
mldonkey_gui: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS) $(MLDONKEYGUI_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI_CMXAS) $(MLDONKEYGUI_CMXS) 
 
mldonkey_gui.byte: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMOS)  $(MLDONKEYGUI_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI_CMAS) $(MLDONKEYGUI_CMOS) 
 
mldonkey_gui.static:  $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS)  $(MLDONKEYGUI_CMXAS)
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
 
mldonkey_gui2: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS) $(MLDONKEYGUI2_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI2_CMXAS) $(MLDONKEYGUI2_CMXS) 
 
mldonkey_gui2.byte: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMOS)  $(MLDONKEYGUI2_CMAS)
	$(OCAMLC) -linkall -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(MLDONKEYGUI2_CMAS) $(MLDONKEYGUI2_CMOS) 
 
mldonkey_gui2.static:  $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS)  $(MLDONKEYGUI2_CMXAS)
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
 
mldonkey_guistarter: $(STARTER_OBJS) $(STARTER_CMXS) $(STARTER_CMXAS)
	$(OCAMLOPT) -linkall $(PLUGIN_FLAG) -o $@  $(STARTER_OBJS) $(LIBS_opt) $(LIBS_flags) $(GTK_LIBS_opt) $(GTK_LIBS_flags) -I build $(STARTER_CMXAS) $(STARTER_CMXS) 
 
mldonkey_guistarter.byte: $(STARTER_OBJS) $(STARTER_CMOS)  $(STARTER_CMAS)
	$(OCAMLC) -linkall -o $@  $(STARTER_OBJS) $(LIBS_byte) $(LIBS_flags)  $(GTK_LIBS_byte) $(GTK_LIBS_flags) -I build $(STARTER_CMAS) $(STARTER_CMOS) 
 
mldonkey_guistarter.static:  $(STARTER_OBJS) $(STARTER_CMXS)  $(STARTER_CMXAS)
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

TOP_CMXAS := $(TOP_CMXA)
TOP_CMAS=$(foreach file, $(TOP_CMXAS),   $(basename $(file)).cma)    

TMPSOURCES += $(TOP_ML4:.ml4=.ml) $(TOP_MLL:.mll=.ml) $(TOP_MLY:.mly=.ml) $(TOP_MLY:.mly=.mli) $(TOP_ZOG:.zog=.ml) 
 
mldonkeytop: $(TOP_OBJS) $(TOP_CMOS) $(TOP_CMAS)
	ocamlmktop -linkall $(PLUGIN_FLAG) -o $@  $(TOP_OBJS) $(LIBS_byte) $(LIBS_flags) $(_LIBS_byte) $(_LIBS_flags) -I build $(TOP_CMAS) $(TOP_CMOS) 
 


#######################################################################

##                      Other rules

#######################################################################


opt:  $(PATCHED_OCAMLOPT)  $(TMPSOURCES) $(TARGETS)

byte:  $(TMPSOURCES) $(foreach target, $(TARGETS), $(target).byte)
static: $(PATCHED_OCAMLOPT) $(foreach target, $(TARGETS), $(target).static)

kde_applet: $(APPLET_OBJS)
	cd applets/kde; ./configure --prefix /usr; make
	@echo; echo
	@echo "      Go in applets/kde, su root, and call 'make install'"
	@echo; echo

gnome_applet: $(APPLET_OBJS)
	cd applets/gnome; make
	@echo; echo
	@echo "      Go in applets/gnome, su root, and call 'make install'"
	@echo; echo

APPLET_SRCS=\
  applets/api/endianess.c \
  applets/api/gui_protocol.c \
  applets/api/main.c

APPLET_OBJS += $(foreach file, $(APPLET_SRCS), $(basename $(file)).o)

text_applet: $(APPLET_OBJS)
	$(CC) -o text_applet $(APPLET_OBJS)


lib/md4_cc.o: lib/md4.c
	$(OCAMLC) -ccopt "$(CFLAGS) -O6 -o lib/md4_cc.o" -ccopt "" -c lib/md4.c

cdk/heap_c.o: cdk/heap_c.c
	$(OCAMLC) -ccopt "$(CFLAGS) $(MORECFLAGS) -o cdk/heap_c.o" -ccopt "" -c cdk/heap_c.c

lib/md4_as.o: lib/md4_$(MD4ARCH).s
	as -o lib/md4_as.o lib/md4_$(MD4ARCH).s

lib/md4_comp.o: lib/md4_$(MD4COMP).o
	cp -f lib/md4_$(MD4COMP).o lib/md4_comp.o


zogml:
	(for i in gui/gui*_base.zog ; do \
		$(CAMLP4) pa_o.cmo pa_zog.cma pr_o.cmo -impl $$i > gui/`basename $$i zog`ml ;\
	done)

#######################################################################

#                      Other rules

#######################################################################


clean: 
	rm -f *.cm? donkey_* *.byte *.cm?? $(TARGETS) *~ *.o core *.static *.a
	rm -f build/*.a build/*.cma build/*.cmxa
	rm -f *_plugin
	rm -f mldonkey mldonkey_gui
	(for i in $(SUBDIRS); do \
		rm -f  $$i/*.cm? $$i/*.o ; \
	done)

releaseclean: clean
	rm -f config/config.cache config/config.log config/config.status
	rm -f config/config.h config/Makefile.config
	rm -f tools/zoggy/*.cm?
	rm -f $(TMPSOURCES)
	rm -rf patches/build

distclean: releaseclean
	rm -rf patches/local

maintainerclean: distclean
	rm -f gui/gui.ml gui/gui_zog.ml 

LOCAL=patches/build

PA_ZOG_FILES=tools/zoggy/zog_types.ml tools/zoggy/zog_messages.ml tools/zoggy/zog_misc.ml tools/zoggy/pa_zog.ml

pa_zog.cma: $(PA_ZOG_FILES)
	$(OCAMLC) -I tools/zoggy -I +camlp4 -pp "$(CAMLP4) pa_o.cmo pr_dump.cmo" -a -o pa_zog.cma  $(PA_ZOG_FILES)

$(TMPSOURCES): pa_zog.cma

depend:  pa_zog.cma lib/http_lexer.ml $(TMPSOURCES) $(TMPFILES)
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .depend
	(for i in $(SUBDIRS); do \
		$(OCAMLDEP) $(INCLUDES) $$i/*.ml $$i/*.mli  >> .depend; \
	done)

$(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/Makefile: patches/ocamlopt-$(REQUIRED_OCAML).tar.gz
	rm -rf $(LOCAL)/ocamlopt-$(REQUIRED_OCAML)
	mkdir -p $(LOCAL)
	cd $(LOCAL); \
	gzip -cd ../ocamlopt-$(REQUIRED_OCAML).tar.gz | tar xf -; \
	touch ocamlopt-$(REQUIRED_OCAML)/Makefile

$(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/ocamlopt: $(LOCAL)/ocamlopt-$(REQUIRED_OCAML)/Makefile
	cd $(LOCAL)/ocamlopt-$(REQUIRED_OCAML); $(MAKE)

#######################################################################

#                      Building binary distribs

#######################################################################

DISDIR=mldonkey-distrib
distrib/Readme.txt: gui/gui_messages.ml
	grep -A 1000 help_text gui/gui_messages.ml | grep -v '"' > distrib/Readme.txt


debug:
	rm -f cdk/heap_c.o
	MORECFLAGS="-I patches/ocaml-3.06/ -DHEAP_DUMP" $(MAKE) cdk/heap_c.o
	$(MAKE)

RELEASE_TARGETS=mldonkey mlnet mldonkey_gui

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

upload.static: release.static
	scp mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar.$(COMPRESS_EXT) lachesis:devel/mldonkey-release/


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
	rm -f mldonkey mldonkey.static mlnet mlnet.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i686
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i686 config/Makefile.config
	rm -f mldonkey mldonkey.static mlnet mlnet.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i586
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i586 config/Makefile.config
	rm -f mldonkey mldonkey.static mlnet mlnet.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i486
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i486 config/Makefile.config
	rm -f mldonkey mldonkey.static mlnet mlnet.static lib/md4_comp.* lib/md4_as.*
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
