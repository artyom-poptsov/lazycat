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

lazycat_ui_gtkdir	= $(pkgdatadir)/ui/gtk
lazycat_ui_clidir	= $(pkgdatadir)/ui/cli
lazycat_ui_gtk_DATA	= gtk/*
lazycat_ui_cli_DATA	= cli/*

bin_SCRIPTS = lc lazycat-gtk

lc: lc.scm
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

lazycat-gtk: lazycat-gtk.scm
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

CLEANFILES	= lc lazycat-gtk
