dnl   configuration script for LazyCat
dnl   Process this file with autoconf to produce a configure script.

define(LAZYCAT_CONFIGURE_COPYRIGHT, [[
Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>

This file is part of LazyCat.
 
LazyCat is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

LazyCat is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.
]])

AC_INIT([lazycat], [0.2])

AC_COPYRIGHT(LAZYCAT_CONFIGURE_COPYRIGHT)

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE

# Find a C compiler.
AC_PROG_CC

# Checks related to GNU Guile
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

# Check for required Guile modules
#GUILE_MODULE_REQUIRED(gnome gtk)
GUILE_MODULE_REQUIRED(ssh session)

AC_CHECK_FUNCS([mkdir socket])
AC_CHECK_HEADERS([stdint.h stdlib.h string.h sys/socket.h syslog.h unistd.h])

AC_FUNC_FORK
AC_FUNC_MALLOC

AC_TYPE_MODE_T
AC_TYPE_SIZE_T

AC_CONFIG_FILES([
	Makefile
        lazycat-daemon/Makefile
        ui/Makefile
        proxies/Makefile
        host/Makefile
        common/Makefile
        trans/Makefile
])

# Generate a Makefile, based on the results.
AC_OUTPUT()
