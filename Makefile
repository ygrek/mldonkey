include config/Makefile.config

#######################################################################

#              Bytecode or Native ?

#######################################################################

GTK_LIBS_byte=-I +lablgtk $(LABLGL_CMA) lablgtk.cma
GTK_LIBS_opt=-I +lablgtk  $(LABLGL_CMXA) lablgtk.cmxa
GTK_STATIC_LIBS_opt=-I +lablgtk lablgtk.cmxa

#STR_LIBS_byte=str.cma
#STR_LIBS_opt=str.cmxa


LIBS_byte=-custom unix.cma
LIBS_opt= unix.cmxa



#######################################################################

#              General options

#######################################################################

ifdef PLUGIN
  PLUGIN_FLAG += -plugin $(PLUGIN)_
  INCLUDES += -I $(PLUGIN)
endif

SUBDIRS=cdk chat lib net tools common driver mp3tagui 

INCLUDES +=-I +lablgtk $(foreach file, $(SUBDIRS), -I $(file))

CFLAGS:=$(CFLAGS) $(CONFIG_INCLUDES)

TARGETS= use_tags$(EXE) mldonkey$(EXE)


#######################################################################

#              'Objects files for "mldonkey"

#######################################################################

CDK_SRCS=cdk/printexc.ml cdk/genlex2.ml cdk/sysenv.ml \
  cdk/netbase.ml cdk/filepath.ml cdk/string2.ml \
  cdk/filename2.ml cdk/list2.ml cdk/hashtbl2.ml \
  cdk/file.ml cdk/unix2.ml cdk/heap.ml cdk/weak2.ml \
  cdk/select_c.c cdk/heap_c.c

ifeq ("$(ZLIB)" , "yes")
  LIBS_opt += -cclib -lz
  LIBS_byte += -cclib -lz
  CDK_SRCS +=  cdk/zlib.ml cdk/zlibstubs.c
endif

MP3TAG_SRCS=     mp3tagui/mp3_info.ml  mp3tagui/mp3_genres.ml \
  mp3tagui/mp3_misc.ml\
  mp3tagui/mp3_tag.ml mp3tagui/mp3tag.ml

LIB_SRCS= lib/autoconf.ml \
  lib/int32ops.ml lib/options.ml lib/ip.ml  lib/numset.ml  \
  lib/fifo.ml lib/intmap.ml \
  lib/hole_tab.ml lib/store.ml lib/indexer.ml lib/indexer1.ml lib/indexer2.ml lib/host.ml  \
  lib/misc.ml lib/unix32.ml  lib/md4.ml \
  lib/avifile.ml lib/http_lexer.mll lib/url.ml \
  lib/mailer.ml lib/date.ml \
  lib/md4_comp.c lib/md4_c.c lib/unix32_c.c lib/inet_c.c

NET_SRCS = \
  net/bigEndian.ml net/littleEndian.ml \
  net/basicSocket.ml net/tcpBufferedSocket.ml \
  net/tcpClientSocket.ml net/tcpServerSocket.ml \
  net/udpSocket.ml net/http_server.ml net/http_client.ml

OBJS=

CHAT_SRCS = chat/chat_messages.ml\
	chat/chat_misc.ml\
        chat/chat_proto.ml\
        chat/chat_types.ml\
        chat/chat_options.ml\
        chat/chat_config.ml\

COMMON_SRCS=common/commonTypes.ml \
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
  common/commonInteractive.ml

all: opt


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
  donkey/donkeyProtoCom.ml  \
  \
  donkey/donkeyGlobals.ml \
  donkey/donkeyComplexOptions.ml \
  donkey/donkeyIndexer.ml \
  donkey/donkeyShare.ml \
  donkey/donkeyOneFile.ml \
  donkey/donkeyClient.ml \
  donkey/donkeyFiles.ml  \
  donkey/donkeyServers.ml \
  donkey/donkeySearch.ml \
  donkey/donkeyProtoOvernet.ml \
  donkey/donkeyOvernet.ml \
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

DIRECTCONNECT_SRCS= \
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

#   cymes/cymesUdp.ml 

ifeq ("$(DONKEY)" , "yes")
SUBDIRS += donkey

CORE_PLUGINS += $(DONKEY_SRCS)

ifeq ("$(DONKEY_SERVER)" , "yes")
  SUBDIRS += server

  CORE_PLUGINS += $(DONKEY_SERVER_SRCS)
  endif

endif

ifeq ("$(CYMES)" , "yes")
SUBDIRS += cymes
CORE_PLUGINS += $(CYMES_SRCS)
endif

ifeq ("$(OPEN_NAPSTER)" , "yes")
SUBDIRS += opennap
CORE_PLUGINS += $(OPENNAP_SRCS)
endif

ifeq ("$(LIMEWIRE)" , "yes")
SUBDIRS += limewire

CORE_PLUGINS += $(LIMEWIRE_SRCS)

endif

ifeq ("$(OPENFT)" , "yes")
SUBDIRS += openFT

CORE_PLUGINS += $(OPENFT_SRCS)

endif

limewire_plugin:
	$(MAKE) PLUGIN_SRCS=LIMEWIRE_SRCS PLUGIN=limewire plugin

ifeq ("$(SOULSEEK)" , "yes")
SUBDIRS += soulseek

CORE_PLUGINS += $(SOULSEEK_SRCS)
endif

ifeq ("$(DIRECT_CONNECT)" , "yes")
SUBDIRS += direct_connect

CORE_PLUGINS += $(DIRECTCONNECT_SRCS)
endif

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


MLDONKEY_SRCS= \
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

#              'Objects files for "mldonkey_gui"

#######################################################################

# gui2 is the old GUI which is not working anymore ... maybe some users
# might want to keep it for a while. We don't want to force users to move

ifeq ("$(COMPILE_GUI)" , "yes")
SUBDIRS += gui gui2 configwin okey gpattern 

CONFIGWIN_SRCS=configwin/configwin_types.ml \
  configwin/configwin_messages.ml \
  configwin/configwin_ihm.ml configwin/configwin.ml

MP3TAGUI_SRCS=  mp3tagui/mp3_messages.ml mp3tagui/mp3_ui.ml

GPATTERN_SRCS=  gpattern/gpattern.ml

OKEY_SRCS= okey/okey.ml

GUI_SRCS= gui/gui_messages.ml \
  gui/gui_columns.ml \
  gui/gui_global.ml \
  gui/gui_keys.ml gui/gui_options.ml \
  gui/gui_com.ml gui/gui_misc.ml \
  gui/gui_icons.ml \
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
  $(MIN_PROTO_SRCS) $(OKEY_SRCS) $(GPATTERN_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(GUI_SRCS)


MLDONKEYGUI2_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS)  $(CONFIGWIN_SRCS) $(MP3TAGUI_SRCS) \
  $(MIN_PROTO_SRCS) $(OKEY_SRCS) $(GPATTERN_SRCS) \
  $(CHAT_SRCS) $(COMMON_SRCS) $(GUI2_SRCS)

TOP_SRCS= \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) 

#######################################################################

#              'Objects files for "mlchat"

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


TARGETS += mldonkey_gui$(EXE)   mldonkey_gui2$(EXE)  mlchat$(EXE)

endif


#######################################################################

#              'Objects files for "use_tags"

#######################################################################

USE_TAGS_SRCS = \
  $(CDK_SRCS) $(LIB_SRCS) $(NET_SRCS) \
  $(MP3TAG_SRCS) \
  lib/cddb_lexer.mll lib/cddb_file.ml \
  tools/use_tags.ml

######################################################################

#         From sources to objects files

######################################################################

MLDONKEY_ZOG := $(filter %.zog, $(MLDONKEY_SRCS))
MLDONKEY_MLL := $(filter %.mll, $(MLDONKEY_SRCS))
MLDONKEY_MLY := $(filter %.mly, $(MLDONKEY_SRCS))

ifeq  ("$(OCAMLC_DIL)" , "yes")
  MLDONKEY_BYTE_ML := $(filter %.ml %.mll %.zog %.mly %.lam, $(MLDONKEY_SRCS))
else
  MLDONKEY_BYTE_ML := $(filter %.ml %.mll %.zog %.mly, $(MLDONKEY_SRCS))
endif

ifeq  ("$(OCAMLOPT_DIL)" , "yes")
  MLDONKEY_OPT_ML := $(filter %.ml %.mll %.zog %.mly %.lam, $(MLDONKEY_SRCS))
else
  MLDONKEY_OPT_ML := $(filter %.ml %.mll %.zog %.mly, $(MLDONKEY_SRCS))
endif

MLDONKEY_C := $(filter %.c, $(MLDONKEY_SRCS))

MLDONKEY_CMOS=$(foreach file, $(MLDONKEY_BYTE_ML),   $(basename $(file)).cmo)
MLDONKEY_CMXS=$(foreach file, $(MLDONKEY_OPT_ML),   $(basename $(file)).cmx)
MLDONKEY_OBJS=$(foreach file, $(MLDONKEY_C),   $(basename $(file)).o)

TMPSOURCES += $(MLDONKEY_MLL:.mll=.ml) $(MLDONKEY_MLY:.mly=.ml) $(MLDONKEY_MLY:.mly=.mli) $(MLDONKEY_ZOG:.zog=.ml)





MLDONKEYGUI_ZOG := $(filter %.zog, $(MLDONKEYGUI_SRCS))
MLDONKEYGUI_MLL := $(filter %.mll, $(MLDONKEYGUI_SRCS))
MLDONKEYGUI_MLY := $(filter %.mly, $(MLDONKEYGUI_SRCS))

MLDONKEYGUI_ML := $(filter %.ml %.mll %.zog %.mly, $(MLDONKEYGUI_SRCS))
MLDONKEYGUI_C := $(filter %.c, $(MLDONKEYGUI_SRCS))
MLDONKEYGUI_OBJS=$(foreach file, $(MLDONKEYGUI_C),   $(basename $(file)).o)

MLDONKEYGUI_CMOS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmo)
MLDONKEYGUI_CMXS=$(foreach file, $(MLDONKEYGUI_ML),   $(basename $(file)).cmx)

TMPSOURCES += $(MLDONKEYGUI_MLL:.mll=.ml) $(MLDONKEYGUI_MLY:.mly=.ml) $(MLDONKEYGUI_MLY:.mly=.mli) $(MLDONKEYGUI_ZOG:.zog=.ml)






MLDONKEYGUI2_ZOG := $(filter %.zog, $(MLDONKEYGUI2_SRCS))
MLDONKEYGUI2_MLL := $(filter %.mll, $(MLDONKEYGUI2_SRCS))
MLDONKEYGUI2_MLY := $(filter %.mly, $(MLDONKEYGUI2_SRCS))

MLDONKEYGUI2_ML := $(filter %.ml %.mll %.zog %.mly, $(MLDONKEYGUI2_SRCS))
MLDONKEYGUI2_C := $(filter %.c, $(MLDONKEYGUI2_SRCS))
MLDONKEYGUI2_OBJS=$(foreach file, $(MLDONKEYGUI2_C),   $(basename $(file)).o)

MLDONKEYGUI2_CMOS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmo)
MLDONKEYGUI2_CMXS=$(foreach file, $(MLDONKEYGUI2_ML),   $(basename $(file)).cmx)

TMPSOURCES += $(MLDONKEYGUI2_MLL:.mll=.ml) $(MLDONKEYGUI2_MLY:.mly=.ml) $(MLDONKEYGUI2_MLY:.mly=.mli) $(MLDONKEYGUI2_ZOG:.zog=.ml)






TOP_ZOG := $(filter %.zog, $(TOP_SRCS))
TOP_MLL := $(filter %.mll, $(TOP_SRCS))
TOP_MLY := $(filter %.mly, $(TOP_SRCS))

TOP_ML := $(filter %.ml %.mll %.zog %.mly, $(TOP_SRCS))
TOP_C := $(filter %.c, $(TOP_SRCS))
TOP_OBJS=$(foreach file, $(TOP_C),   $(basename $(file)).o)

TOP_CMOS=$(foreach file, $(TOP_ML),   $(basename $(file)).cmo)
TOP_CMXS=$(foreach file, $(TOP_ML),   $(basename $(file)).cmx)

TMPSOURCES += $(TOP_MLL:.mll=.ml) $(TOP_MLY:.mly=.ml) $(TOP_MLY:.mly=.mli) $(TOP_ZOG:.zog=.ml)








MLCHAT_ZOG := $(filter %.zog, $(MLCHAT_SRCS))
MLCHAT_MLL := $(filter %.mll, $(MLCHAT_SRCS))
MLCHAT_MLY := $(filter %.mly, $(MLCHAT_SRCS))

MLCHAT_ML := $(filter %.ml %.mll %.zog %.mly, $(MLCHAT_SRCS))

MLCHAT_ML := $(filter %.ml %.mll %.zog %.mly, $(MLCHAT_SRCS))
MLCHAT_C := $(filter %.c, $(MLCHAT_SRCS))
MLCHAT_OBJS=$(foreach file, $(MLCHAT_C),   $(basename $(file)).o)

MLCHAT_CMOS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmo)
MLCHAT_CMXS=$(foreach file, $(MLCHAT_ML),   $(basename $(file)).cmx)

TMPSOURCES += $(MLCHAT_MLL:.mll=.ml) $(MLCHAT_MLY:.mly=.ml) $(MLCHAT_MLY:.mly=.mli) $(MLCHAT_ZOG:.zog=.ml)






USE_TAGS_ZOG := $(filter %.zog, $(USE_TAGS_SRCS))
USE_TAGS_MLL := $(filter %.mll, $(USE_TAGS_SRCS))
USE_TAGS_MLY := $(filter %.mly, $(USE_TAGS_SRCS))


USE_TAGS_ML := $(filter %.ml %.mll %.zog %.mly, $(USE_TAGS_SRCS))
USE_TAGS_C := $(filter %.c, $(USE_TAGS_SRCS))
USE_TAGS_OBJS=$(foreach file, $(USE_TAGS_C),   $(basename $(file)).o)

USE_TAGS_CMOS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmo)
USE_TAGS_CMXS=$(foreach file, $(USE_TAGS_ML),   $(basename $(file)).cmx)

TMPSOURCES += $(USE_TAGS_MLL:.mll=.ml) $(USE_TAGS_MLY:.mly=.ml) $(USE_TAGS_MLY:.mly=.mli) $(USE_TAGS_ZOG:.zog=.ml)






OBSERVER_ZOG := $(filter %.zog, $(OBSERVER_SRCS))
OBSERVER_MLL := $(filter %.mll, $(OBSERVER_SRCS))
OBSERVER_MLY := $(filter %.mly, $(OBSERVER_SRCS))


OBSERVER_ML := $(filter %.ml %.mll %.zog %.mly, $(OBSERVER_SRCS))
OBSERVER_C := $(filter %.c, $(OBSERVER_SRCS))
OBSERVER_OBJS=$(foreach file, $(OBSERVER_C),   $(basename $(file)).o)

OBSERVER_CMOS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmo)
OBSERVER_CMXS=$(foreach file, $(OBSERVER_ML),   $(basename $(file)).cmx)

TMPSOURCES += $(OBSERVER_MLL:.mll=.ml) $(OBSERVER_MLY:.mly=.ml) $(OBSERVER_MLY:.mly=.mli) $(OBSERVER_ZOG:.zog=.ml)






PLUGIN_ZOG := $(filter %.zog, $($(PLUGIN_SRCS)))
PLUGIN_MLL := $(filter %.mll, $($(PLUGIN_SRCS)))
PLUGIN_MLY := $(filter %.mly, $($(PLUGIN_SRCS)))

PLUGIN_CMXS=$(foreach file, $($(PLUGIN_SRCS)),   $(basename $(file)).cmx)

TMPSOURCES += $(PLUGIN_MLL:.mll=.ml) $(PLUGIN_MLY:.mly=.ml) $(PLUGIN_MLY:.mly=.mli) $(PLUGIN_ZOG:.zog=.ml)

#######################################################################

#                      Other rules

#######################################################################

opt:  $(PATCHED_OCAMLOPT)  $(TMPSOURCES) $(TARGETS)

byte:  $(TMPSOURCES) $(foreach target, $(TARGETS), $(target).byte)
static: $(PATCHED_OCAMLOPT) $(foreach target, $(TARGETS), $(target).static)


PLUGINS_FILES:=$(foreach plugin, $(PLUGINS), $(plugin)_plugin)

plugins: $(PLUGINS_FILES)
	echo $(PLUGINS) $(PLUGINS_FILES)

plugin: $(PLUGIN_CMXS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $(PLUGIN)_plugin -a $(PLUGIN_CMXS)

lib/md4_cc.o: lib/md4.c
	$(OCAMLC) -ccopt "$(CFLAGS) -O6 -I /byterun -o lib/md4_cc.o" -ccopt "" -c lib/md4.c

lib/md4_as.o: lib/md4_$(MD4ARCH).s
	as -o lib/md4_as.o lib/md4_$(MD4ARCH).s

lib/md4_comp.o: lib/md4_$(MD4COMP).o
	cp -f lib/md4_$(MD4COMP).o lib/md4_comp.o

######## TAGS

use_tags: $(USE_TAGS_CMXS) $(OBJS) $(USE_TAGS_OBJS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@ str.cmxa $(LIBS_opt) $(STR_LIBS_opt) $(USE_TAGS_CMXS) $(USE_TAGS_OBJS) $(OBJS)

use_tags.static: $(USE_TAGS_CMXS) $(OBJS) $(USE_TAGS_OBJS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -cclib -static -o $@ str.cmxa $(LIBS_opt) $(STR_LIBS_opt) $(USE_TAGS_CMXS) $(USE_TAGS_OBJS) $(OBJS)

use_tags.byte: $(USE_TAGS_CMOS) $(OBJS)
	$(OCAMLC) -o $@ str.cma $(LIBS_byte) $(STR_LIBS_byte) $(USE_TAGS_CMOS) $(OBJS)

######## MLCHAT
mlchat: $(MLCHAT_CMXS) $(OBJS)  $(MLCHAT_OBJS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@ $(LIBS_opt) $(GTK_LIBS_opt) $(MLCHAT_CMXS) $(OBJS)   $(MLCHAT_OBJS)

mlchat.byte: $(MLCHAT_CMOS) $(OBJS)
	$(OCAMLC) -o $@ $(LIBS_byte) $(GTK_LIBS_byte) $(MLCHAT_CMOS) $(OBJS)

######## MLDONKEYGUI

mldonkey_gui: $(MLDONKEYGUI_CMXS) $(OBJS)  $(MLDONKEYGUI_OBJS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@ $(LIBS_opt) $(GTK_LIBS_opt) $(MLDONKEYGUI_CMXS) $(MLDONKEYGUI_OBJS) $(OBJS)

mldonkey_gui.byte: $(MLDONKEYGUI_CMOS) $(OBJS) 
	$(OCAMLC) -o $@ $(LIBS_byte) $(GTK_LIBS_byte) $(MLDONKEYGUI_CMOS) $(OBJS)

mldonkey_gui.static: $(MLDONKEYGUI_CMXS) $(OBJS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(LIBS_opt) $(GTK_STATIC_LIBS_opt) $(MLDONKEYGUI_CMXS) $(OBJS)

######## MLDONKEYGUI2

mldonkey_gui2: $(MLDONKEYGUI2_CMXS) $(OBJS)  $(MLDONKEYGUI2_OBJS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@ $(LIBS_opt) $(GTK_LIBS_opt) $(MLDONKEYGUI2_CMXS) $(OBJS) $(MLDONKEYGUI2_OBJS) 

mldonkey_gui2.byte: $(MLDONKEYGUI2_CMOS) $(OBJS) 
	$(OCAMLC) -o $@ $(LIBS_byte) $(GTK_LIBS_byte) $(MLDONKEYGUI2_CMOS) $(OBJS)

mldonkey_gui2.static: $(MLDONKEYGUI2_CMXS) $(OBJS) 
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@ $(LIBS_opt) $(GTK_STATIC_LIBS_opt) $(MLDONKEYGUI2_CMXS) $(OBJS)


######## OBSERVER

observer: $(OBSERVER_CMXS) $(OBJS) $(OBSERVER_OBJS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@ str.cmxa $(LIBS_opt) $(STR_LIBS_opt) $(OBSERVER_CMXS) $(OBSERVER_OBJS) $(OBJS)

observer.byte: $(OBSERVER_CMOS) $(OBJS)
	$(OCAMLC) -o $@ str.cma $(LIBS_byte) $(STR_LIBS_byte) $(OBSERVER_CMOS) $(OBJS)


zogml:
	(for i in gui/gui*_base.zog ; do \
		$(CAMLP4) pa_o.cmo pa_zog.cma pr_o.cmo -impl $$i > gui/`basename $$i zog`ml ;\
	done)

####### TOP

mldonkeytop: $(TOP_CMOS) $(TOP_OBJS)
	$(OCAMLMKTOP) -o $@ $(LIBS_byte) $(TOP_CMOS) $(TOP_OBJS) $(OBJS)

######## MLDONKEY

mldonkey: $(MLDONKEY_OBJS) $(MLDONKEY_CMXS) $(OBJS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $@ $(LIBS_opt)  $(MLDONKEY_OBJS) $(MLDONKEY_CMXS) $(OBJS)

mldonkey.byte: $(MLDONKEY_OBJS) $(MLDONKEY_CMOS) $(OBJS)
	$(OCAMLC) -o $@ $(LIBS_byte)  $(MLDONKEY_OBJS) $(MLDONKEY_CMOS) $(OBJS)

mldonkey.static:  $(MLDONKEY_OBJS) $(MLDONKEY_CMXS) $(OBJS)
	$(OCAMLOPT) $(PLUGIN_FLAG) -ccopt -static -o $@  $(LIBS_opt)  $(MLDONKEY_OBJS) $(MLDONKEY_CMXS) $(OBJS)

open_mldonkey: mldonkey
open_mldonkey.opt:  mldonkey
open_mldonkey.byte: mldonkey.byte
open_mldonkey.static: mldonkey.state

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

release: opt VERSION
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	for i in $(TARGETS); do \
	   cp $$i $(DISDIR)/$$i; \
   	   strip  $(DISDIR)/$$i; \
	done
	mv $(DISDIR) $(DISDIR)-`cat VERSION`
	tar cf $(DISDIR).tar $(DISDIR)-`cat VERSION`
	mv $(DISDIR).tar mldonkey-`cat VERSION`.shared.$(ARCH)-`uname -s`.tar
	$(COMPRESS) mldonkey-`cat VERSION`.shared.$(ARCH)-`uname -s`.tar
	scp mldonkey-`cat VERSION`.shared.$(ARCH)-`uname -s`.tar.$(COMPRESS_EXT) lachesis:devel/mldonkey-release/

release-sources: VERSION
	rm -rf **/CVS
	rm -f config/Makefile.config
	cd ..; tar zcf mldonkey-`cat mldonkey/VERSION`.sources.tar.gz mldonkey
	scp ../mldonkey-`cat VERSION`.sources.tar.gz lachesis:devel/mldonkey-release/

distrib: $(DISDIR)

$(DISDIR):  static distrib/Readme.txt VERSION
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	rm -rf $(DISDIR)/CVS
	cp mldonkey.static $(DISDIR)/mldonkey
	strip  $(DISDIR)/mldonkey
	cp mldonkey_gui.static $(DISDIR)/mldonkey_gui
	strip $(DISDIR)/mldonkey_gui
	tar cf $(DISDIR).tar $(DISDIR)
	mv $(DISDIR).tar mldonkey-`cat VERSION`.static.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-`cat VERSION`.static.$(MD4ARCH)-`uname -s`.tar

macosx:  opt distrib/Readme.txt VERSION
	rm -rf mldonkey-*
	cp -R distrib $(DISDIR)
	rm -rf $(DISDIR)/CVS
	cp mldonkey $(DISDIR)/mldonkey
	strip  $(DISDIR)/mldonkey
	cp mldonkey_gui $(DISDIR)/mldonkey_gui
	strip $(DISDIR)/mldonkey_gui
	tar cf $(DISDIR).tar $(DISDIR)
	mv $(DISDIR).tar mldonkey-`cat VERSION`.shared.ppc-MacOS-X.tar
	gzip mldonkey-`cat VERSION`.shared.ppc-MacOS-X.tar

SHADIR=mldonkey-shared

shared: $(SHADIR)

$(SHADIR):  static distrib/Readme.txt VERSION
	rm -rf mldonkey-*
	cp -R distrib $(SHADIR)
	rm -rf $(SHADIR)/CVS
	cp mldonkey.static $(SHADIR)/mldonkey
	strip  $(SHADIR)/mldonkey
	cp mldonkey_gui $(SHADIR)/mldonkey_gui
	strip $(SHADIR)/mldonkey_gui
	tar cf $(SHADIR).tar $(SHADIR)
	mv $(SHADIR).tar mldonkey-`cat VERSION`.shared.$(MD4ARCH)-`uname -s`.tar
	bzip2 mldonkey-`cat VERSION`.shared.$(MD4ARCH)-`uname -s`.tar

VERSION: common/commonGlobals.ml
	grep Release: common/commonGlobals.ml | awk -F: '{ print $$2 }' | awk '{ print $$1 }' > VERSION	

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


#######################################################################

#              Specific rules

#######################################################################

-include .depend

.SUFFIXES: .mli .ml .cmx .cmo .o .c .cmi .mll .mly .zog .plugindep
.mli.cmi :
	$(OCAMLC) $(OFLAGS) $(INCLUDES) -c $<

.ml.cmi :
	$(OCAMLC) $(OFLAGS) $(INCLUDES) -c $<

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

.cmo.byte:
	$(OCAMLC) -o $*.byte $(LIBS) $<

.cmx.opt:
	$(OCAMLOPT) $(PLUGIN_FLAG) -o $*.opt $(OPTLIBS) $<


.plugindep:
	echo toto
