## Config file for GNU Automake. 
##
## Copyright (C) 2012-2013 Artyom Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of LazyCat.
## 
## LazyCat is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## LazyCat is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.

AM_CPPFLAGS=`pkg-config --cflags guile-1.8`
AM_LDFLAGS=`pkg-config --libs guile-1.8` -lssh

INCLUDES = \
	-I$(top_srcdir)/server \
	-I$(top_srcdir)/common \
	-I$(top_srcdir)/proxies \
	-I$(top_srcdir)/proxies/tcp-proxy \
	-I$(top_srcdir)/proxies/ssh-proxy

dist_doc_DATA = README NEWS COPYING AUTHORS ChangeLog TODO

bin_PROGRAMS = lazycatd lctcpd

# Scheme files
lazycat_serverdir	= $(pkgdatadir)/server
lazycat_ui_gtkdir	= $(pkgdatadir)/ui/gtk
lazycat_ui_clidir	= $(pkgdatadir)/ui/cli
lazycat_server_DATA	= server/scm/*
lazycat_ui_gtk_DATA	= ui/gtk/*
lazycat_ui_cli_DATA	= ui/cli/*

bin_SCRIPTS = lc lazycat-gtk lazycat-daemon

lc: ui/lc.scm
	sed -e 's|__UI_CLI_DATA_DIR__|$(lazycat_ui_clidir)|g' \
		-e 's|__SERVER_DATA_DIR__|$(lazycat_serverdir)|g' $< > $@
	chmod +x $@

lazycat-gtk: ui/lazycat-gtk.scm
	sed -e 's|__UI_GTK_DATA_DIR__|$(lazycat_ui_gtkdir)|g' \
		-e 's|__SERVER_DATA_DIR__|$(lazycat_serverdir)|g' $< > $@
	chmod +x $@

lazycat-daemon:
	@echo -n "Generating lazycat-daemon script... "
	@echo -e "#!/bin/sh\n"\
		"\n"\
		"# Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>\n"\
		"#\n"\
		"# This auto-generated file is a part of LazyCat.\n"\
		"#\n"\
		"# Copying and distribution of this file, with or without modification,\n"\
		"# are permitted in any medium without royalty provided the copyright\n"\
		"# notice and this notice are preserved.  This file is offered as-is,\n"\
		"# without any warranty.\n"\
		"\n"\
		"export GUILE_LOAD_PATH=\$${GUILE_LOAD_PATH}:${lazycat_serverdir}\n"\
		"lazycatd\n" > $@
	@echo "done"


CLEANFILES	= lc lazycat-gtk lazycat-daemon

################################################################################
# Lazycat sources
#

lazycatd_SOURCES =

# Common

lazycatd_SOURCES += \
	common/common.h	\
	common/xsockets.c \
	common/xsockets.h \
	common/parsers.c \
	common/parsers.h

# Proxies

lazycatd_SOURCES += \
	proxies/tcp-proxy/tcp-proxy.c \
	proxies/tcp-proxy/tcp-proxy.h

lazycatd_SOURCES += \
	proxies/ssh-proxy/ssh-proxy.c \
	proxies/ssh-proxy/ssh-proxy.h \
	proxies/ssh-proxy/lazy-ssh.c \
	proxies/ssh-proxy/lazy-ssh.h

# Server

lazycatd_SOURCES += \
	server/db.c \
	server/db.h \
	server/sighandlers.c \
	server/sighandlers.h \
	server/scmapi.c \
	server/scmapi.h \
	server/server.c

################################################################################
# Host processes
#

# lctcpd

lctcpd_SOURCES =

lctcpd_SOURCES += \
	common/common.h \
	common/xsockets.c \
	common/xsockets.h \
	host/lctcpd.c
