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

AM_CFLAGS = $(GUILE_CFLAGS)
AM_CFLAGS += -DGUILE_LOAD_PATH=\"$(datadir)\"
AM_LDFLAGS = $(GUILE_LDFLAGS)

bin_PROGRAMS = lazycat-daemon

lazycat_daemondir	= $(pkgdatadir)/daemon
lazycat_daemon_DATA	= \
	config.scm				\
	curiosity.scm				\
	diff.scm				\
	host-list.scm				\
	host.scm				\
	lazycatd.scm				\
	periodical-ping.scm			\
	proxy-list.scm				\
	translator-list.scm

lazycat_daemon_SOURCES = lazycat-daemon.c
