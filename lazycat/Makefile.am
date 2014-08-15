## Config file for GNU Automake. 
##
## Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

SUBDIRS = daemon trans

pkgdata_DATA = \
	message.scm		\
	protocol.scm		\
	proxy.scm		\
	translator.scm		\
	utils.scm		\
	rpc-types+constants.scm	\
	rpc-client.scm		\
	rpc-server.scm

rpc-types+constants.scm: rpc.x
	echo ';;; -*- buffer-read-only: t; -*-' > $@
	echo ';;; Automatically generated -- do not modify.' >> $@
	echo '(define-module (lazycat rpc-types+constants)' >> $@
	echo '  #:use-module (rpc xdr types)' >> $@
	echo '  #:use-module (rpc xdr))' >> $@
	$(GRPCC) --xdr --constants < $< |\
		sed 's|^(define |(define-public |g' >> $@

rpc-client.scm: rpc.x
	echo ';;; -*- buffer-read-only: t; -*-' > $@
	echo ';;; Automatically generated -- do not modify.' >> $@
	echo '(define-module (lazycat rpc-client)' >> $@
	echo '  #:use-module (lazycat rpc-types+constants)' >> $@
	echo '  #:use-module (rpc rpc)' >> $@
	echo '  #:use-module (rpc xdr types))' >> $@
	$(GRPCC) --client < $< |\
		sed 's|^(define |(define-public |g' >> $@

rpc-server.scm: rpc.x
	echo ';;; -*- buffer-read-only: t; -*-' > $@
	echo ';;; Automatically generated -- do not modify.' >> $@
	echo '(define-module (lazycat rpc-server)' >> $@
	echo '  #:use-module (lazycat rpc-types+constants)' >> $@
	echo '  #:use-module (srfi srfi-1)' >> $@
	echo '  #:use-module (rpc rpc server)' >> $@
	echo '  #:use-module (rpc xdr types))' >> $@
	$(GRPCC) --server < $< |\
		sed 's|^(define |(define-public |g' >> $@

## Makefile.am ends here
