/* Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of LazyCat.
 * 
 * LazyCat is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * LazyCat is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __XSOCKETS_H__
#define __XSOCKETS_H__

int open_socket (const char* socket_name);
int open_inet_socket (const uint16_t port);
int connect_to_socket (const char* socket_name);
int connect_to_inet_socket (const uint32_t address, const uint16_t port);
int parse_ip_addr (const char* str, uint32_t* address, uint16_t* port);

int xsend_msg (const int sfd, const char* data, size_t data_sz);
int xrecv_msg (const int sfd, char** buf, size_t* buf_sz);

#endif
