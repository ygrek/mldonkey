INCLUDES = $(all_includes) 

lib_LTLIBRARIES = libmldonkeyapplet.la

libmldonkeyapplet_la_SOURCES = mldonkeyapplet.cpp

METASOURCES = AUTO
noinst_HEADERS = mldonkeyapplet.h

lnkdir = $(kde_datadir)/kicker/applets
lnk_DATA = mldonkeyapplet.desktop

EXTRA_DIST = $(lnk_DATA)

libmldonkeyapplet_la_LDFLAGS = $(all_libraries) -version-info 1:0:0 -module -no-undefined
libmldonkeyapplet_la_LIBADD = $(LIB_KDEUI)

messages:
	$(XGETTEXT) *.cpp *.h -o $(podir)/mldonkeyapplet.pot
