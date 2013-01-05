## Config file for GNU Automake. 
##
## Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
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

bin_PROGRAMS = lazycat lctcpd

################################################################################
# Lazycat sources
#

lazycat_SOURCES =

# Common

lazycat_SOURCES += \
	common/common.h	\
	common/xsockets.c \
	common/xsockets.h \
	common/parsers.c \
	common/parsers.h

# Proxies

lazycat_SOURCES += \
	proxies/tcp-proxy/tcp-proxy.c \
	proxies/tcp-proxy/tcp-proxy.h

lazycat_SOURCES += \
	proxies/ssh-proxy/ssh-proxy.c \
	proxies/ssh-proxy/ssh-proxy.h \
	proxies/ssh-proxy/lazy-ssh.c \
	proxies/ssh-proxy/lazy-ssh.h

# Server

lazycat_SOURCES += \
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
