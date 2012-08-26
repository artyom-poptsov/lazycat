/* Various useful functions for working with sockets.
 *
 * Copyright (C) 2012 Artyom Poptsov
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

#include <stdio.h>
#include <string.h>

#include <errno.h>      /* errno */
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>   /* mkdir() */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>     /* struct sockaddr_un */
#include <netinet/in.h> /* struct sockaddr_in */

#include <errno.h>      /* errno */
#include <syslog.h>
#include <time.h>       /* sleep() */

#include "common.h"
#include "xsockets.h"

/*
 * Static functions
 */

static int xsend (const int sfd, const void* data, size_t data_sz);
static int xrecv (const int sfd, void* buf, size_t data_sz);

/*
 * Global definitions
 */

const char SOCK_DIR[] = "/tmp/lazycat";

/*
 * This function opens AF_LOCAL socket with given name
 */
int
open_socket (const char* socket_name)
{
  struct sockaddr_un addr;
  socklen_t          addr_len;
  int                sfd;
  char*              socket_full_path;

  if (mkdir(SOCK_DIR, (mode_t) 0777) < 0)
    {
      switch (errno)
        {
        case EEXIST:
	  syslog (LOG_NOTICE, "mkdir(): Directory %s already exists.",
		  SOCK_DIR);
          break;

        default:
	  syslog (LOG_WARNING,
		  "mkdir(): Error occured while creating directory %s.",
		  SOCK_DIR);
	  return -1;
        }
    }

  socket_full_path = (char*) calloc (strlen (SOCK_DIR) +
				     strlen (socket_name) + 2,
				     sizeof(char));

  sprintf (socket_full_path, "%s/%s", SOCK_DIR, socket_name);

  if (unlink (socket_full_path) < 0)
    {
      if (errno != ENOENT)
	{
	  syslog (LOG_WARNING, "unlink(): Unable to unlink socket.");
	  return -1;
	}
    }

  sfd = socket (AF_LOCAL, SOCK_STREAM, 0);
  if (sfd < 0)
    {
      syslog (LOG_WARNING, "socket(): Unable to open socket.");
      return -1;
    }

  addr.sun_family = AF_LOCAL;
  strcpy (addr.sun_path, socket_full_path);

  addr_len = sizeof (addr);

  if (bind (sfd, (struct sockaddr*) &addr, addr_len) < 0)
    {
      close(sfd);
      syslog (LOG_WARNING, "bind(): Unable to bind to the server socket.");
      return -1;
    }

  return sfd;
}

/*
 * This function opens AF_INET socket with given port
 */
int
open_inet_socket (const uint16_t port)
{
  struct sockaddr_in sa;
  int                sa_size;
  int sfd_server;
  int retval;

  sfd_server = socket (PF_INET, SOCK_STREAM, 0);
  if (sfd_server < 0)
    {
      syslog (LOG_ERR, "Unable to open socket.");
      return -1;
    }

  sa_size = sizeof (sa);
  bzero (&sa, sa_size);

  sa.sin_family = AF_INET;
  sa.sin_port   = htons (port);

  if (INADDR_ANY)
    sa.sin_addr.s_addr = htonl (INADDR_ANY);

  retval = bind (sfd_server, (struct sockaddr*) &sa, sa_size);
  if (retval < 0)
    {
      syslog (LOG_ERR, "An error occured during binding to the port %d\n",
	      port);
      return -1;
    }

  return sfd_server;
}

/*
 * This function does connection to socket with given name
 */
int
connect_to_socket (const char* socket_name)
{
  int sfd;
  struct sockaddr_un sa;
  int                sa_size;
  char*              socket_full_path;

  sfd = socket (AF_UNIX, SOCK_STREAM, 0);

  sa.sun_family = AF_UNIX;

  socket_full_path = (char*) calloc (strlen (SOCK_DIR) +
				     strlen (socket_name) + 2,
				     sizeof(char));

  sprintf (socket_full_path, "%s/%s", SOCK_DIR, socket_name);

  strcpy (sa.sun_path, socket_full_path);

  sa_size = sizeof (sa);

  while (connect (sfd, (struct sockaddr *) &sa, sa_size) < 0)
    {
      syslog(LOG_DEBUG, "Unable connect() to socket: %s", strerror(errno));
      syslog(LOG_DEBUG, "\tSocket: %s", socket_full_path);
      sleep(1);
    }

  return sfd;
}

/*
 * This function does connection to a remote host with given address and port
 */
int
connect_to_inet_socket (const uint32_t address, const uint16_t port)
{
  int sfd;
  struct sockaddr_in sa;
  socklen_t          sa_size;
  int retval;

  sfd = socket (PF_INET, SOCK_STREAM, 0);
  if (sfd < 0)
    return -1;

  sa_size = sizeof (sa);
  bzero (&sa, sa_size);

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons (port);
  sa.sin_addr.s_addr = htonl (address);

  retval = connect (sfd, (struct sockaddr*) &sa, sa_size);
  if (retval < 0)
    return -1;

  return sfd;
}

/*
 * This function sends data through a socket with given file descriptor
 */
int
xsend_msg (const int sfd, const char* data, size_t data_sz)
{
  int ndata_sz = htonl (data_sz);
  int ret = 0;

  if ((ret = xsend (sfd, &ndata_sz, sizeof (ndata_sz)) < 0)
      || (ret = xsend (sfd, (void*) data, data_sz) < 0))
    {
      return ret;
    }

  return ret;
}

/*
 * This function receives data from a socket with given file descriptor
 */
int
xrecv_msg (const int sfd, char** buf, size_t* buf_sz)
{
  int data_sz;
  int ret;

  ret = xrecv (sfd, &data_sz, sizeof (int));
  if (ret <= 0)
    return ret;

  *buf_sz = ntohl (data_sz);
  *buf = (char*) calloc (*buf_sz, sizeof (char));

  ret = xrecv (sfd, *buf, *buf_sz);

  return ret;
}

/*
 * Static functions
 */

static int
xrecv (const int sfd, void* buf, size_t data_sz)
{
  int ret;
  int tries   = 5; /* Number of tries before giving up */
  int timeout = 1; /* Basic timeout between tries */

  while (data_sz && (tries > 0))
    {
      /* Read data from the socket */
      ret = recv (sfd, buf, data_sz, 0);

      if (ret < 0)
	return ret;

      if (ret == 0)
	{
	  /* There is no data to read. Here we waiting for a little
	     time before the next try. Timeout grows after every
	     attempt. */
	  tries--;
	  sleep (timeout++);
	  continue;
	}

      buf     += ret;
      data_sz -= ret;
    }

  return ret;
}

static int
xsend (const int sfd, const void* data, size_t data_sz)
{
  int ret;

  while (data_sz)
    {
      ret = send (sfd, data, data_sz, 0);
      if (ret < 0)
	return ret;

      data    += ret;
      data_sz -= ret;
    }

  return 0;
}
