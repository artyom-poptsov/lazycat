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

AM_CFLAGS = @GUILE_CFLAGS@ `pkg-config --cflags guile-1.8` 
AM_CFLAGS += -DGUILE_LOAD_PATH=\"$(datadir)\"

AM_LDFLAGS=`pkg-config --libs guile-1.8` -lssh

INCLUDES = \
	-I$(top_srcdir)/server \
	-I$(top_srcdir)/common \
	-I$(top_srcdir)/proxies \
	-I$(top_srcdir)/proxies/tcp-proxy \
	-I$(top_srcdir)/proxies/ssh-proxy

dist_doc_DATA = README NEWS COPYING AUTHORS ChangeLog TODO

bin_PROGRAMS = lazycat-daemon lctcpd

# Scheme files
lazycat_serverdir	= $(pkgdatadir)/server
lazycat_ui_gtkdir	= $(pkgdatadir)/ui/gtk
lazycat_ui_clidir	= $(pkgdatadir)/ui/cli
lazycat_server_DATA	= server/scm/*
lazycat_ui_gtk_DATA	= ui/gtk/*
lazycat_ui_cli_DATA	= ui/cli/*

bin_SCRIPTS = lc lazycat-gtk

lc: ui/lc.scm
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

lazycat-gtk: ui/lazycat-gtk.scm
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

CLEANFILES	= lc lazycat-gtk

################################################################################
# Lazycat sources
#

lazycat_daemon_SOURCES =

# Common

lazycat_daemon_SOURCES += \
	common/common.h	\
	common/xsockets.c \
	common/xsockets.h \
	common/parsers.c \
	common/parsers.h

# Proxies

lazycat_daemon_SOURCES += \
	proxies/tcp-proxy/tcp-proxy.c \
	proxies/tcp-proxy/tcp-proxy.h

lazycat_daemon_SOURCES += \
	proxies/ssh-proxy/ssh-proxy.c \
	proxies/ssh-proxy/ssh-proxy.h \
	proxies/ssh-proxy/lazy-ssh.c \
	proxies/ssh-proxy/lazy-ssh.h

# Server

lazycat_daemon_SOURCES += \
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
