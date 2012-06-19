## Config file for GNU Autoconf.
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

AC_INIT([lazycat], [0.2])

AM_INIT_AUTOMAKE

# Find a C compiler.
AC_PROG_CC

# Check for Guile
GUILE_FLAGS

AC_CHECK_FUNCS([mkdir socket])
AC_CHECK_HEADERS([stdint.h stdlib.h string.h sys/socket.h syslog.h unistd.h])

AC_FUNC_FORK
AC_FUNC_MALLOC

AC_TYPE_MODE_T
AC_TYPE_SIZE_T

AC_CONFIG_FILES([
	Makefile
])

# Generate a Makefile, based on the results.
AC_OUTPUT()