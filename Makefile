
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


GTK_LIBS_byte=-I +lablgtk $(LABLGL_CMA) lablgtk.cma
GTK_LIBS_opt=-I +lablgtk  $(LABLGL_CMXA) lablgtk.cmxa
GTK_STATIC_LIBS_opt=-I +lablgtk lablgtk.cmxa

NO_LIBS_byte=str.cma
NO_LIBS_opt=str.cmxa
NO_STATIC_LIBS_opt=str.cmxa


LIBS_byte=-custom unix.cma str.cma
LIBS_opt= unix.cmxa str.cmxa


#######################################################################

##              General options

#######################################################################

SUBDIRS=cdk chat lib net tools common driver mp3tagui config/$(OS_FILES)

INCLUDES +=-I +lablgtk $(foreach file, $(SUBDIRS), -I $(file))

CFLAGS:=$(CFLAGS) $(CONFIG_INCLUDES)

TARGETS= use_tags$(EXE) mlnet$(EXE)


#######################################################################

##              Objects files for "mldonkey"

#######################################################################


CDK_SRCS=\
  cdk/dprintf.ml \
  cdk/printexc2.ml cdk/genlex2.ml cdk/sysenv.ml \
  cdk/netbase.ml cdk/filepath.ml cdk/string2.ml \
  cdk/filename2.ml cdk/list2.ml cdk/hashtbl2.ml \
  cdk/file.ml cdk/unix2.ml cdk/heap.ml cdk/weak2.ml \
  cdk/heap_c.c cdk/array2.ml cdk/sort2.ml 

ifeq ("$(ZLIB)" , "yes")
  LIBS_opt += -cclib -lz
  LIBS_byte += -cclib -lz
  CDK_SRCS +=  cdk/zlib.ml cdk/zlibstubs.c
endif

MP3TAG_SRCS=     mp3tagui/mp3_info.ml  mp3tagui/mp3_genres.ml \
  mp3tagui/mp3_misc.ml\
  mp3tagui/mp3_tag.ml mp3tagui/mp3tag.ml


LIB_SRCS=   lib/autoconf.ml \
  config/$(OS_FILES)/mlUnix.ml \
  config/$(OS_FILES)/os_stubs_c.c \
  lib/intmap.ml \
  lib/int32ops.ml lib/options.ml lib/ip.ml  lib/numset.ml  \
  lib/fifo.ml lib/fifo2.ml lib/intset.ml \
  lib/hole_tab.ml lib/store.ml \
  lib/indexer.ml lib/indexer1.ml lib/indexer2.ml lib/host.ml  \
  lib/misc.ml lib/unix32.ml  lib/md4.ml \
  lib/avifile.ml lib/http_lexer.mll lib/url.ml \
  lib/mailer.ml lib/date.ml \
  lib/md4_comp.c lib/md4_c.c \
  lib/gettext.ml lib/md5_c.c \
  lib/stubs_c.c

NET_SRCS = \
  net/bigEndian.ml net/littleEndian.ml \
  net/basicSocket.ml net/tcpBufferedSocket.ml \
  net/tcpClientSocket.ml net/tcpServerSocket.ml \
  net/udpSocket.ml net/http_server.ml net/http_client.ml \
  net/multicast.ml net/multicast_c.c 


# net/terminal.ml


CHAT_SRCS = chat/chat_messages.ml\
	chat/chat_misc.ml\
        chat/chat_proto.ml\
        chat/chat_types.ml\
        chat/chat_options.ml\
        chat/chat_config.ml\

COMMON_SRCS=common/commonTypes.ml \
  common/commonMessages.ml \
  common/guiTypes.ml \
  common/commonEvent.ml \
  common/commonOptions.ml \
  common/commonGlobals.ml \
  common/guiProto.ml \
  common/guiDecoding.ml \
  common/guiEncoding.ml \
  common/commonChat.ml 

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
  common/commonDownloads.ml

all: Makefile config/Makefile.config opt

config/configure: config/configure.in
	cd config; autoconf
	
ifeq ("$(CONFIG_ARGS_DEFINED)" , "yes")

config/Makefile.config: Makefile config/configure config/Makefile.config.in
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


DONKEY_SRCS= \
  \
  donkey/donkeyTypes.ml \
  donkey/donkeyOptions.ml \
  donkey/donkeyMftp.ml donkey/donkeyImport.ml \
  donkey/donkeyOpenProtocol.ml \
  donkey/donkeyProtoClient.ml donkey/donkeyProtoServer.ml  \
  \
  donkey/donkeyGlobals.ml \
  donkey/donkeyProtoCom.ml  \
  donkey/donkeySources1.ml  \
 \
  donkey/donkeyComplexOptions.ml \
  donkey/donkeySupernode.ml \
  donkey/donkeyIndexer.ml \
  donkey/donkeyShare.ml \
  donkey/donkeyOneFile.ml \
  donkey/donkeyStats.ml \
  donkey/donkeyClient.ml \
  donkey/donkeyProtoOvernet.ml \
  donkey/donkeySources2.ml  \
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

LIMEWIRE_SRCS= \
  limewire/limewireTypes.ml \
  limewire/limewireOptions.ml \
  limewire/limewireGlobals.ml \
  limewire/limewireComplexOptions.ml \
  limewire/limewireProtocol.ml \
  limewire/limewireClients.ml \
  limewire/limewireServers.ml \
  limewire/limewireInteractive.ml \
  limewire/limewireMain.ml

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
CORE_PLUGINS += $(CYMES_SRCS)
endif

ifeq ("$(OPENFT)" , "yes")
SUBDIRS += openFT

CORE_PLUGINS += $(OPENFT_SRCS)

endif

limewire_plugin:
	$(MAKE) PLUGIN_SRCS=LIMEWIRE_SRCS PLUGIN=limewire plugin

ifeq ("$(AUDIO_GALAXY)" , "yes")
SUBDIRS += audio_galaxy

CORE_PLUGINS += $(AUDIOGALAXY_SRCS)
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





ifeq ("$(DIRECT_CONNECT)" , "yes")
SUBDIRS += direct_connect

CORE_PLUGINS += $(DIRECT_CONNECT_SRCS)

TARGETS += mldc$(EXE)
endif

mldc_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  $(COMMON_SRCS) \
  $(COMMON_CLIENT_SRCS) \
  \
  $(DIRECT_CONNECT_SRCS) \
  \
  $(DRIVER_SRCS)




ifeq ("$(OPENNAP)" , "yes")
SUBDIRS += opennap

CORE_PLUGINS += $(OPENNAP_SRCS)

TARGETS += mlnap$(EXE)
endif

mlnap_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  $(COMMON_SRCS) \
  $(COMMON_CLIENT_SRCS) \
  \
  $(OPENNAP_SRCS) \
  \
  $(DRIVER_SRCS)




ifeq ("$(LIMEWIRE)" , "yes")
SUBDIRS += limewire

CORE_PLUGINS += $(LIMEWIRE_SRCS)

TARGETS += mlgnut$(EXE)
endif

mlgnut_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  $(COMMON_SRCS) \
  $(COMMON_CLIENT_SRCS) \
  \
  $(LIMEWIRE_SRCS) \
  \
  $(DRIVER_SRCS)




ifeq ("$(DONKEY)" , "yes")
SUBDIRS += donkey

CORE_PLUGINS += $(DONKEY_SRCS)

TARGETS += mldonkey$(EXE)
endif

mldonkey_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  $(COMMON_SRCS) \
  $(COMMON_CLIENT_SRCS) \
  \
  $(DONKEY_SRCS) \
  \
  $(DRIVER_SRCS)




ifeq ("$(SOULSEEK)" , "yes")
SUBDIRS += soulseek

CORE_PLUGINS += $(SOULSEEK_SRCS)

TARGETS += mlslsk$(EXE)
endif

mlslsk_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  $(COMMON_SRCS) \
  $(COMMON_CLIENT_SRCS) \
  \
  $(SOULSEEK_SRCS) \
  \
  $(DRIVER_SRCS)




MLNET_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  $(CHAT_SRCS) \
  $(COMMON_SRCS) \
  $(COMMON_CLIENT_SRCS) \
  \
  $(CORE_PLUGINS) \
  \
  $(DRIVER_SRCS)



#######################################################################

#              Objects files for "mldonkey_gui"

#######################################################################


ifeq ("$(COMPILE_GUI)" , "yes")

SUBDIRS += gui gui2 configwin okey gpattern icons/$(ICONS_CHOICE)

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


ALL_ICONS=$(foreach file, $(ICONS),   $(basename $(file)).ml_icon)
ALL_ICONS_SRCS=$(foreach file, $(ICONS),   $(basename $(file))_xpm.ml)

$(ALL_ICONS_SRCS): $(ALL_ICONS)

GUI_BASE_SRCS= \
  gui/gui_messages.ml   gui/gui_global.ml \
  gui/gui_columns.ml \
  gui/gui_keys.ml \
  $(ALL_ICONS_SRCS) \
  gui/gui_icons.ml \

GUI_SRCS=  \
  gui/gui_options.ml gui/gui_misc.ml \
  gui/gui_com.ml \
  gui/gui_help_base.zog gui/gui_help.ml \
  gui/gui_console_base.zog gui/gui_console.ml \
  gui/gui_uploads_base.zog gui/gui_uploads.ml \
  gui/gui_users_base.zog gui/gui_users.ml \
  gui/gui_results_base.zog gui/gui_results.ml \
  gui/gui_rooms_base.zog gui/gui_rooms.ml \
  gui/gui_friends_base.zog gui/gui_friends.ml \
  gui/gui_servers_base.zog gui/gui_servers.ml \
  gui/gui_queries_base.zog gui/gui_queries.ml \
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


MLDONKEYGUI_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS)  $(CONFIGWIN_SRCS) $(MP3TAGUI_SRCS) \
  $(OKEY_SRCS) $(GPATTERN_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(IM_CORE) $(GUI_BASE_SRCS) $(GUI_SRCS)

MLDONKEY_IM_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS)  $(CONFIGWIN_SRCS) $(MP3TAGUI_SRCS) \
  $(OKEY_SRCS) $(GPATTERN_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(IM_CORE) \
  $(GUI_BASE_SRCS) $(IM_GUI_CORE) \
  im/gui_im_main.ml

STARTER_SRCS= gui/gui_starter.ml

MLDONKEYGUI2_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS)  $(CONFIGWIN_SRCS) $(MP3TAGUI_SRCS) \
  $(OKEY_SRCS) $(GPATTERN_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(IM_CORE) $(GUI2_SRCS)

TOP_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) 


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

MLCHAT_SRCS= \
  $(CDK_SRCS) $(CONFIGWIN_SRCS) $(OKEY_SRCS) $(CHAT_SRCS) $(CHAT_EXE_SRCS)


TARGETS += mldonkey_gui$(EXE)   mldonkey_gui2$(EXE)  mlchat$(EXE) mldonkey_guistarter$(EXE)


#### IM stuff is now automatically included in the GUI


SUBDIRS += im im/yahoo
  
IM_CORE += im/imTypes.ml im/imEvent.ml \
   im/imProtocol.ml im/imIdentity.ml im/imAccount.ml \
   im/imChat.ml im/imRoom.ml \
   im/imOptions.ml

IM_CORE +=   im/yahoo/yahoo.ml

IM_GUI_CORE += im/gui_im_base.zog   \
    im/gui_im.ml

TARGETS += mlim

ifeq ("$(DEVEL)", "yes")
  SUBDIRS += im/msn im/irc

  IM_CORE +=    im/msn/msn.ml im/irc/irc.ml
endif

IM_CORE +=   im/imMain.ml

endif


#######################################################################

#              Objects files for "use_tags"

#######################################################################


USE_TAGS_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  lib/cddb_lexer.mll lib/cddb_file.ml \
  tools/use_tags.ml


######################################################################

#         From sources to objects files

######################################################################





mldonkey_ZOG := $(filter %.zog, $(mldonkey_SRCS)) 
mldonkey_MLL := $(filter %.mll, $(mldonkey_SRCS)) 
mldonkey_MLY := $(filter %.mly, $(mldonkey_SRCS)) 
mldonkey_ML := $(filter %.ml %.mll %.zog %.mly, $(mldonkey_SRCS)) 
mldonkey_C := $(filter %.c, $(mldonkey_SRCS)) 
mldonkey_CMOS=$(foreach file, $(mldonkey_ML),   $(basename $(file)).cmo) 
mldonkey_CMXS=$(foreach file, $(mldonkey_ML),   $(basename $(file)).cmx) 
mldonkey_OBJS=$(foreach file, $(mldonkey_C),   $(basename $(file)).o)    

TMPSOURCES += $(mldonkey_MLL:.mll=.ml) $(mldonkey_MLY:.mly=.ml) $(mldonkey_MLY:.mly=.mli) $(mldonkey_ZOG:.zog=.ml) 
 
mldonkey: $(mldonkey_OBJS) $(mldonkey_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(mldonkey_OBJS) $(LIBS_opt) $(_LIBS_opt) $(mldonkey_CMXS) 
 
mldonkey.byte: $(mldonkey_OBJS) $(mldonkey_CMOS) 
	$(OCAMLC) -o $@  $(mldonkey_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(mldonkey_CMOS) 
 
mldonkey.static:  $(mldonkey_OBJS) $(mldonkey_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(mldonkey_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(mldonkey_CMXS) 


MLDONKEYGUI_ZOG := $(filter %.zog, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_MLL := $(filter %.mll, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_MLY := $(filter %.mly, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_ML := $(filter %.ml %.mll %.zog %.mly, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_C := $(filter %.c, $(MLDONKEYGUI_SRCS)) 
MLDONKEYGUI_CMOS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmo) 
MLDONKEYGUI_CMXS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmx) 
MLDONKEYGUI_OBJS=$(foreach file, $(MLDONKEYGUI_C),   $(basename $(file)).o)    

TMPSOURCES += $(MLDONKEYGUI_MLL:.mll=.ml) $(MLDONKEYGUI_MLY:.mly=.ml) $(MLDONKEYGUI_MLY:.mly=.mli) $(MLDONKEYGUI_ZOG:.zog=.ml) 
 
mldonkey_gui: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_opt) $(GTK_LIBS_opt) $(MLDONKEYGUI_CMXS) 
 
mldonkey_gui.byte: $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMOS) 
	$(OCAMLC) -o $@  $(MLDONKEYGUI_OBJS) $(LIBS_byte)  $(GTK_LIBS_byte) $(MLDONKEYGUI_CMOS) 
 
mldonkey_gui.static:  $(MLDONKEYGUI_OBJS) $(MLDONKEYGUI_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEYGUI_OBJS) $(LIBS_opt)  $(GTK_STATIC_LIBS_opt) $(MLDONKEYGUI_CMXS) 


MLDONKEYGUI2_ZOG := $(filter %.zog, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLL := $(filter %.mll, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_MLY := $(filter %.mly, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_ML := $(filter %.ml %.mll %.zog %.mly, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_C := $(filter %.c, $(MLDONKEYGUI2_SRCS)) 
MLDONKEYGUI2_CMOS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmo) 
MLDONKEYGUI2_CMXS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmx) 
MLDONKEYGUI2_OBJS=$(foreach file, $(MLDONKEYGUI2_C),   $(basename $(file)).o)    

TMPSOURCES += $(MLDONKEYGUI2_MLL:.mll=.ml) $(MLDONKEYGUI2_MLY:.mly=.ml) $(MLDONKEYGUI2_MLY:.mly=.mli) $(MLDONKEYGUI2_ZOG:.zog=.ml) 
 
mldonkey_gui2: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_opt) $(GTK_LIBS_opt) $(MLDONKEYGUI2_CMXS) 
 
mldonkey_gui2.byte: $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMOS) 
	$(OCAMLC) -o $@  $(MLDONKEYGUI2_OBJS) $(LIBS_byte)  $(GTK_LIBS_byte) $(MLDONKEYGUI2_CMOS) 
 
mldonkey_gui2.static:  $(MLDONKEYGUI2_OBJS) $(MLDONKEYGUI2_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEYGUI2_OBJS) $(LIBS_opt)  $(GTK_STATIC_LIBS_opt) $(MLDONKEYGUI2_CMXS) 


mldc_ZOG := $(filter %.zog, $(mldc_SRCS)) 
mldc_MLL := $(filter %.mll, $(mldc_SRCS)) 
mldc_MLY := $(filter %.mly, $(mldc_SRCS)) 
mldc_ML := $(filter %.ml %.mll %.zog %.mly, $(mldc_SRCS)) 
mldc_C := $(filter %.c, $(mldc_SRCS)) 
mldc_CMOS=$(foreach file, $(mldc_ML),   $(basename $(file)).cmo) 
mldc_CMXS=$(foreach file, $(mldc_ML),   $(basename $(file)).cmx) 
mldc_OBJS=$(foreach file, $(mldc_C),   $(basename $(file)).o)    

TMPSOURCES += $(mldc_MLL:.mll=.ml) $(mldc_MLY:.mly=.ml) $(mldc_MLY:.mly=.mli) $(mldc_ZOG:.zog=.ml) 
 
mldc: $(mldc_OBJS) $(mldc_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(mldc_OBJS) $(LIBS_opt) $(_LIBS_opt) $(mldc_CMXS) 
 
mldc.byte: $(mldc_OBJS) $(mldc_CMOS) 
	$(OCAMLC) -o $@  $(mldc_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(mldc_CMOS) 
 
mldc.static:  $(mldc_OBJS) $(mldc_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(mldc_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(mldc_CMXS) 


mlnap_ZOG := $(filter %.zog, $(mlnap_SRCS)) 
mlnap_MLL := $(filter %.mll, $(mlnap_SRCS)) 
mlnap_MLY := $(filter %.mly, $(mlnap_SRCS)) 
mlnap_ML := $(filter %.ml %.mll %.zog %.mly, $(mlnap_SRCS)) 
mlnap_C := $(filter %.c, $(mlnap_SRCS)) 
mlnap_CMOS=$(foreach file, $(mlnap_ML),   $(basename $(file)).cmo) 
mlnap_CMXS=$(foreach file, $(mlnap_ML),   $(basename $(file)).cmx) 
mlnap_OBJS=$(foreach file, $(mlnap_C),   $(basename $(file)).o)    

TMPSOURCES += $(mlnap_MLL:.mll=.ml) $(mlnap_MLY:.mly=.ml) $(mlnap_MLY:.mly=.mli) $(mlnap_ZOG:.zog=.ml) 
 
mlnap: $(mlnap_OBJS) $(mlnap_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(mlnap_OBJS) $(LIBS_opt) $(_LIBS_opt) $(mlnap_CMXS) 
 
mlnap.byte: $(mlnap_OBJS) $(mlnap_CMOS) 
	$(OCAMLC) -o $@  $(mlnap_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(mlnap_CMOS) 
 
mlnap.static:  $(mlnap_OBJS) $(mlnap_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlnap_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(mlnap_CMXS) 


MLNET_ZOG := $(filter %.zog, $(MLNET_SRCS)) 
MLNET_MLL := $(filter %.mll, $(MLNET_SRCS)) 
MLNET_MLY := $(filter %.mly, $(MLNET_SRCS)) 
MLNET_ML := $(filter %.ml %.mll %.zog %.mly, $(MLNET_SRCS)) 
MLNET_C := $(filter %.c, $(MLNET_SRCS)) 
MLNET_CMOS=$(foreach file, $(MLNET_ML),   $(basename $(file)).cmo) 
MLNET_CMXS=$(foreach file, $(MLNET_ML),   $(basename $(file)).cmx) 
MLNET_OBJS=$(foreach file, $(MLNET_C),   $(basename $(file)).o)    

TMPSOURCES += $(MLNET_MLL:.mll=.ml) $(MLNET_MLY:.mly=.ml) $(MLNET_MLY:.mly=.mli) $(MLNET_ZOG:.zog=.ml) 
 
mlnet: $(MLNET_OBJS) $(MLNET_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(MLNET_OBJS) $(LIBS_opt) $(_LIBS_opt) $(MLNET_CMXS) 
 
mlnet.byte: $(MLNET_OBJS) $(MLNET_CMOS) 
	$(OCAMLC) -o $@  $(MLNET_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(MLNET_CMOS) 
 
mlnet.static:  $(MLNET_OBJS) $(MLNET_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLNET_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(MLNET_CMXS) 


mlgnut_ZOG := $(filter %.zog, $(mlgnut_SRCS)) 
mlgnut_MLL := $(filter %.mll, $(mlgnut_SRCS)) 
mlgnut_MLY := $(filter %.mly, $(mlgnut_SRCS)) 
mlgnut_ML := $(filter %.ml %.mll %.zog %.mly, $(mlgnut_SRCS)) 
mlgnut_C := $(filter %.c, $(mlgnut_SRCS)) 
mlgnut_CMOS=$(foreach file, $(mlgnut_ML),   $(basename $(file)).cmo) 
mlgnut_CMXS=$(foreach file, $(mlgnut_ML),   $(basename $(file)).cmx) 
mlgnut_OBJS=$(foreach file, $(mlgnut_C),   $(basename $(file)).o)    

TMPSOURCES += $(mlgnut_MLL:.mll=.ml) $(mlgnut_MLY:.mly=.ml) $(mlgnut_MLY:.mly=.mli) $(mlgnut_ZOG:.zog=.ml) 
 
mlgnut: $(mlgnut_OBJS) $(mlgnut_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(mlgnut_OBJS) $(LIBS_opt) $(_LIBS_opt) $(mlgnut_CMXS) 
 
mlgnut.byte: $(mlgnut_OBJS) $(mlgnut_CMOS) 
	$(OCAMLC) -o $@  $(mlgnut_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(mlgnut_CMOS) 
 
mlgnut.static:  $(mlgnut_OBJS) $(mlgnut_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlgnut_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(mlgnut_CMXS) 


mlslsk_ZOG := $(filter %.zog, $(mlslsk_SRCS)) 
mlslsk_MLL := $(filter %.mll, $(mlslsk_SRCS)) 
mlslsk_MLY := $(filter %.mly, $(mlslsk_SRCS)) 
mlslsk_ML := $(filter %.ml %.mll %.zog %.mly, $(mlslsk_SRCS)) 
mlslsk_C := $(filter %.c, $(mlslsk_SRCS)) 
mlslsk_CMOS=$(foreach file, $(mlslsk_ML),   $(basename $(file)).cmo) 
mlslsk_CMXS=$(foreach file, $(mlslsk_ML),   $(basename $(file)).cmx) 
mlslsk_OBJS=$(foreach file, $(mlslsk_C),   $(basename $(file)).o)    

TMPSOURCES += $(mlslsk_MLL:.mll=.ml) $(mlslsk_MLY:.mly=.ml) $(mlslsk_MLY:.mly=.mli) $(mlslsk_ZOG:.zog=.ml) 
 
mlslsk: $(mlslsk_OBJS) $(mlslsk_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(mlslsk_OBJS) $(LIBS_opt) $(_LIBS_opt) $(mlslsk_CMXS) 
 
mlslsk.byte: $(mlslsk_OBJS) $(mlslsk_CMOS) 
	$(OCAMLC) -o $@  $(mlslsk_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(mlslsk_CMOS) 
 
mlslsk.static:  $(mlslsk_OBJS) $(mlslsk_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(mlslsk_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(mlslsk_CMXS) 


MLDONKEY_IM_ZOG := $(filter %.zog, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_MLL := $(filter %.mll, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_MLY := $(filter %.mly, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_ML := $(filter %.ml %.mll %.zog %.mly, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_C := $(filter %.c, $(MLDONKEY_IM_SRCS)) 
MLDONKEY_IM_CMOS=$(foreach file, $(MLDONKEY_IM_ML),   $(basename $(file)).cmo) 
MLDONKEY_IM_CMXS=$(foreach file, $(MLDONKEY_IM_ML),   $(basename $(file)).cmx) 
MLDONKEY_IM_OBJS=$(foreach file, $(MLDONKEY_IM_C),   $(basename $(file)).o)    

TMPSOURCES += $(MLDONKEY_IM_MLL:.mll=.ml) $(MLDONKEY_IM_MLY:.mly=.ml) $(MLDONKEY_IM_MLY:.mly=.mli) $(MLDONKEY_IM_ZOG:.zog=.ml) 
 
mlim: $(MLDONKEY_IM_OBJS) $(MLDONKEY_IM_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(MLDONKEY_IM_OBJS) $(LIBS_opt) $(GTK_LIBS_opt) $(MLDONKEY_IM_CMXS) 
 
mlim.byte: $(MLDONKEY_IM_OBJS) $(MLDONKEY_IM_CMOS) 
	$(OCAMLC) -o $@  $(MLDONKEY_IM_OBJS) $(LIBS_byte)  $(GTK_LIBS_byte) $(MLDONKEY_IM_CMOS) 
 
mlim.static:  $(MLDONKEY_IM_OBJS) $(MLDONKEY_IM_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLDONKEY_IM_OBJS) $(LIBS_opt)  $(GTK_STATIC_LIBS_opt) $(MLDONKEY_IM_CMXS) 


STARTER_ZOG := $(filter %.zog, $(STARTER_SRCS)) 
STARTER_MLL := $(filter %.mll, $(STARTER_SRCS)) 
STARTER_MLY := $(filter %.mly, $(STARTER_SRCS)) 
STARTER_ML := $(filter %.ml %.mll %.zog %.mly, $(STARTER_SRCS)) 
STARTER_C := $(filter %.c, $(STARTER_SRCS)) 
STARTER_CMOS=$(foreach file, $(STARTER_ML),   $(basename $(file)).cmo) 
STARTER_CMXS=$(foreach file, $(STARTER_ML),   $(basename $(file)).cmx) 
STARTER_OBJS=$(foreach file, $(STARTER_C),   $(basename $(file)).o)    

TMPSOURCES += $(STARTER_MLL:.mll=.ml) $(STARTER_MLY:.mly=.ml) $(STARTER_MLY:.mly=.mli) $(STARTER_ZOG:.zog=.ml) 
 
mldonkey_guistarter: $(STARTER_OBJS) $(STARTER_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(STARTER_OBJS) $(LIBS_opt) $(GTK_LIBS_opt) $(STARTER_CMXS) 
 
mldonkey_guistarter.byte: $(STARTER_OBJS) $(STARTER_CMOS) 
	$(OCAMLC) -o $@  $(STARTER_OBJS) $(LIBS_byte)  $(GTK_LIBS_byte) $(STARTER_CMOS) 
 
mldonkey_guistarter.static:  $(STARTER_OBJS) $(STARTER_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(STARTER_OBJS) $(LIBS_opt)  $(GTK_STATIC_LIBS_opt) $(STARTER_CMXS) 


TOP_ZOG := $(filter %.zog, $(TOP_SRCS)) 
TOP_MLL := $(filter %.mll, $(TOP_SRCS)) 
TOP_MLY := $(filter %.mly, $(TOP_SRCS)) 
TOP_ML := $(filter %.ml %.mll %.zog %.mly, $(TOP_SRCS)) 
TOP_C := $(filter %.c, $(TOP_SRCS)) 
TOP_CMOS=$(foreach file, $(TOP_ML),   $(basename $(file)).cmo) 
TOP_CMXS=$(foreach file, $(TOP_ML),   $(basename $(file)).cmx) 
TOP_OBJS=$(foreach file, $(TOP_C),   $(basename $(file)).o)    

TMPSOURCES += $(TOP_MLL:.mll=.ml) $(TOP_MLY:.mly=.ml) $(TOP_MLY:.mly=.mli) $(TOP_ZOG:.zog=.ml) 
 
mldonkeytop: $(TOP_OBJS) $(TOP_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(TOP_OBJS) $(LIBS_opt) $(_LIBS_opt) $(TOP_CMXS) 
 
mldonkeytop.byte: $(TOP_OBJS) $(TOP_CMOS) 
	$(OCAMLC) -o $@  $(TOP_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(TOP_CMOS) 
 
mldonkeytop.static:  $(TOP_OBJS) $(TOP_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(TOP_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(TOP_CMXS) 


MLCHAT_ZOG := $(filter %.zog, $(MLCHAT_SRCS)) 
MLCHAT_MLL := $(filter %.mll, $(MLCHAT_SRCS)) 
MLCHAT_MLY := $(filter %.mly, $(MLCHAT_SRCS)) 
MLCHAT_ML := $(filter %.ml %.mll %.zog %.mly, $(MLCHAT_SRCS)) 
MLCHAT_C := $(filter %.c, $(MLCHAT_SRCS)) 
MLCHAT_CMOS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmo) 
MLCHAT_CMXS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmx) 
MLCHAT_OBJS=$(foreach file, $(MLCHAT_C),   $(basename $(file)).o)    

TMPSOURCES += $(MLCHAT_MLL:.mll=.ml) $(MLCHAT_MLY:.mly=.ml) $(MLCHAT_MLY:.mly=.mli) $(MLCHAT_ZOG:.zog=.ml) 
 
mlchat: $(MLCHAT_OBJS) $(MLCHAT_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(MLCHAT_OBJS) $(LIBS_opt) $(GTK_LIBS_opt) $(MLCHAT_CMXS) 
 
mlchat.byte: $(MLCHAT_OBJS) $(MLCHAT_CMOS) 
	$(OCAMLC) -o $@  $(MLCHAT_OBJS) $(LIBS_byte)  $(GTK_LIBS_byte) $(MLCHAT_CMOS) 
 
mlchat.static:  $(MLCHAT_OBJS) $(MLCHAT_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(MLCHAT_OBJS) $(LIBS_opt)  $(GTK_STATIC_LIBS_opt) $(MLCHAT_CMXS) 


OBSERVER_ZOG := $(filter %.zog, $(OBSERVER_SRCS)) 
OBSERVER_MLL := $(filter %.mll, $(OBSERVER_SRCS)) 
OBSERVER_MLY := $(filter %.mly, $(OBSERVER_SRCS)) 
OBSERVER_ML := $(filter %.ml %.mll %.zog %.mly, $(OBSERVER_SRCS)) 
OBSERVER_C := $(filter %.c, $(OBSERVER_SRCS)) 
OBSERVER_CMOS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmo) 
OBSERVER_CMXS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmx) 
OBSERVER_OBJS=$(foreach file, $(OBSERVER_C),   $(basename $(file)).o)    

TMPSOURCES += $(OBSERVER_MLL:.mll=.ml) $(OBSERVER_MLY:.mly=.ml) $(OBSERVER_MLY:.mly=.mli) $(OBSERVER_ZOG:.zog=.ml) 
 
observer: $(OBSERVER_OBJS) $(OBSERVER_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(OBSERVER_OBJS) $(LIBS_opt) $(_LIBS_opt) $(OBSERVER_CMXS) 
 
observer.byte: $(OBSERVER_OBJS) $(OBSERVER_CMOS) 
	$(OCAMLC) -o $@  $(OBSERVER_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(OBSERVER_CMOS) 
 
observer.static:  $(OBSERVER_OBJS) $(OBSERVER_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(OBSERVER_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(OBSERVER_CMXS) 


USE_TAGS_ZOG := $(filter %.zog, $(USE_TAGS_SRCS)) 
USE_TAGS_MLL := $(filter %.mll, $(USE_TAGS_SRCS)) 
USE_TAGS_MLY := $(filter %.mly, $(USE_TAGS_SRCS)) 
USE_TAGS_ML := $(filter %.ml %.mll %.zog %.mly, $(USE_TAGS_SRCS)) 
USE_TAGS_C := $(filter %.c, $(USE_TAGS_SRCS)) 
USE_TAGS_CMOS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmo) 
USE_TAGS_CMXS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmx) 
USE_TAGS_OBJS=$(foreach file, $(USE_TAGS_C),   $(basename $(file)).o)    

TMPSOURCES += $(USE_TAGS_MLL:.mll=.ml) $(USE_TAGS_MLY:.mly=.ml) $(USE_TAGS_MLY:.mly=.mli) $(USE_TAGS_ZOG:.zog=.ml) 
 
use_tags: $(USE_TAGS_OBJS) $(USE_TAGS_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@  $(USE_TAGS_OBJS) $(LIBS_opt) $(_LIBS_opt) $(USE_TAGS_CMXS) 
 
use_tags.byte: $(USE_TAGS_OBJS) $(USE_TAGS_CMOS) 
	$(OCAMLC) -o $@  $(USE_TAGS_OBJS) $(LIBS_byte)  $(_LIBS_byte) $(USE_TAGS_CMOS) 
 
use_tags.static:  $(USE_TAGS_OBJS) $(USE_TAGS_CMXS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(USE_TAGS_OBJS) $(LIBS_opt)  $(_STATIC_LIBS_opt) $(USE_TAGS_CMXS) 



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
	rm -f *.cm? donkey_* *.byte *.cmi $(TARGETS) *~ *.o core *.static
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

release.shared: opt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	for i in $(TARGETS); do \
	   cp $$i $(DISDIR)/$$i; \
	   strip  $(DISDIR)/$$i; \
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
	for i in $(TARGETS); do \
	   cp $$i.static $(DISDIR)/$$i; \
	   strip  $(DISDIR)/$$i; \
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
	cp mldonkey.static $(DISDIR)/mldonkey
	strip  $(DISDIR)/mldonkey
	cp mldonkey_gui.static $(DISDIR)/mldonkey_gui
	strip $(DISDIR)/mldonkey_gui
	tar cf $(DISDIR).tar $(DISDIR)
	mv $(DISDIR).tar mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-$(CURRENT_VERSION).static.$(MD4ARCH)-`uname -s`.tar

macosx:  opt distrib/Readme.txt
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	rm -rf $(DISDIR)/CVS
	cp mldonkey $(DISDIR)/mldonkey
	strip  $(DISDIR)/mldonkey
	cp mldonkey_gui $(DISDIR)/mldonkey_gui
	strip $(DISDIR)/mldonkey_gui
	tar cf $(DISDIR).tar $(DISDIR)
	mv $(DISDIR).tar mldonkey-$(CURRENT_VERSION).shared.ppc-MacOS-X.tar
	gzip mldonkey-$(CURRENT_VERSION).shared.ppc-MacOS-X.tar

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
	mv $(SHADIR).tar mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-$(CURRENT_VERSION).shared.$(MD4ARCH)-`uname -s`.tar

auto-release:
## i386
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i386 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i386-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i686
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i686 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i686-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i586
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i586 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
	$(MAKE) opt static
	$(MAKE) distrib
	cp mldonkey-$(CURRENT_VERSION).static.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
	$(MAKE) shared
	cp mldonkey-$(CURRENT_VERSION).shared.i586-Linux.tar.bz2 $(HOME)/release-$(CURRENT_VERSION)/
## i486
	mkdir -p $(HOME)/release-$(CURRENT_VERSION)
	cp -f config/Makefile.config.i486 config/Makefile.config
	rm -f mldonkey mldonkey.static lib/md4_comp.* lib/md4_as.*
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

.SUFFIXES: .mli .ml .cmx .cmo .o .c .cmi .mll .mly .zog .plugindep .xpm .ml .cc .ml_icon
.mli.cmi :
	$(OCAMLC) $(OFLAGS) $(INCLUDES) -c $<

.ml.cmi :
	$(OCAMLC) $(OFLAGS) $(INCLUDES) -c $<

.xpm.ml_icon :
	echo "let t = [|" > $@
	grep '"' $< | sed 's/",$$/";/' | sed 's/"};$$/"/' >> $@
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
