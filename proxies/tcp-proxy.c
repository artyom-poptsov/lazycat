/* Proxy for communicating with remote hosts through TCP sockets.
 *
 * Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
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
#include <stdlib.h>
#include <syslog.h>
#include <time.h>

#include <stdint.h>

#include <sys/select.h>

#include "common.h"
#include "xsockets.h"
#include "tcp-proxy.h"

/*
 * External functions
 */

extern open_inet_socket (const uint16_t port);
extern int connect_to_inet_socket (const uint32_t address, const uint16_t port);
extern int xsend_msg (const int sfd, const char* data, size_t data_sz);
extern int xrecv_msg (const int sfd, char** buf, size_t* buf_sz);

/*
 * Global variables
 */

static const char SYSLOG_MSG[] = "lazycat [tcp-proxy]";
static const char PROXY_NAME[] = "tcp-proxy";

static int sfd_proxy;
static int sfd_client;

/*
 * Static functions
 */

static void
tcp_proxy_main_loop (void);

static int 
parse_ip_addr (const char* str, uint32_t* address, uint16_t* port);

/*
 * The entry point of this proxy
 */
void
tcp_proxy_init (void)
{
  enum { BACKLOG = 1 };

  openlog (SYSLOG_MSG, LOG_CONS, LOG_DAEMON);
  syslog (LOG_INFO, "Log is opened.");

  /* Open a socket */
  sfd_proxy = open_socket (PROXY_NAME);
  if (sfd_proxy < 0)
    {
      SYSLOG_ERROR ("Unable to open socket.");
      closelog ();
      exit (EXIT_FAILURE);
    }
  
  /* Start listening to the socket */
  if (listen (sfd_proxy, BACKLOG) < 0)
    {
      SYSLOG_ERROR ("An error occured during listen() call.");
      close (sfd_proxy);
      closelog ();
      exit (EXIT_FAILURE);
    }

  /* Accept the lazycat server */
  sfd_client = accept (sfd_proxy, NULL, 0);
  if (sfd_client < 0)
    {
      SYSLOG_ERROR ("accept() error");
      close (sfd_proxy);
      closelog ();
      exit (EXIT_FAILURE);
    }

  /* Start the main loop */
  tcp_proxy_main_loop ();

  /* Teardown of the proxy */
  
  close (sfd_client);
  close (sfd_proxy);
  
  closelog ();
  
  exit (EXIT_SUCCESS);
}

/*
 * The main loop of the proxy
 */

static void
tcp_proxy_main_loop (void)
{
  static const char ERR_UNABLE_TO_CONNECT[] = "ERROR: Unable to connect to the remote host";
  static const char ERR_WRONG_IP_ADDRESS[]  = "ERROR: Wrong IP address";
  
  size_t msg_size;
  char*  msg_buf;

  size_t ip_addr_len;
  char*  ip_addr_str;

  uint32_t ip_addr;
  uint16_t ip_port;

  int sfd_remote;

  int32_t retval;
  
  while (TRUE)
    {
      msg_buf     = NULL;
      ip_addr_str = NULL;
      
      /*
       * Receive destination address
       */

      retval = xrecv_msg (sfd_client, &ip_addr_str, &ip_addr_len);
      if (retval == 0)
	{
	  SYSLOG_INFO ("Server has performed an ordered shutdown.");
	  goto end;
	}
      else if (retval < 0)
	{
	  SYSLOG_WARNING ("xrecv_msg() error");
	  goto end;
	}

      SYSLOG_RECV ("Address: %s", ip_addr_str);

      /*
       * Receive a message
       */

      retval = xrecv_msg (sfd_client, &msg_buf, &msg_size);
      if (retval == 0)
	{
	  SYSLOG_INFO ("Server has performed an ordered shutdown.");
	  goto end;
	}
      else if (retval < 0)
	{
	  SYSLOG_WARNING ("xrecv_msg() error");
	  goto end;
	}

      SYSLOG_RECV ("Message: %s", msg_buf);

      /*
       * Parse IP address
       */

      retval = parse_ip_addr (ip_addr_str, &ip_addr, &ip_port);
      if (retval < 0)
	{
	  SYSLOG_WARNING ("%s: %s", ERR_WRONG_IP_ADDRESS, ip_addr_str);

	  xsend_msg (sfd_client, ERR_WRONG_IP_ADDRESS,
		     sizeof (ERR_WRONG_IP_ADDRESS));

	  free (ip_addr_str);
	  free (msg_buf);
	  close (sfd_client);
	  continue;
	}

      SYSLOG_DEBUG ("IP Address = %d", ip_addr);
      SYSLOG_DEBUG ("IP port    = %d", ip_port);

      free (ip_addr_str);

      /*
       * Send the message to the remote host
       */

      sfd_remote = connect_to_inet_socket (ip_addr, ip_port);
      if (sfd_remote < 0)
	{
	  syslog (LOG_WARNING, ERR_UNABLE_TO_CONNECT);

	  xsend_msg (sfd_client, ERR_UNABLE_TO_CONNECT,
		     sizeof (ERR_UNABLE_TO_CONNECT));

	  free (msg_buf);
	  close (sfd_remote);
	  continue;
	}

      retval = xsend_msg (sfd_remote, msg_buf, msg_size);

      free (msg_buf);

      retval = xrecv_msg (sfd_remote, &msg_buf, &msg_size);

      close (sfd_remote);

      /*
       * Send a response
       */

      SYSLOG_SEND ("Response: %s", msg_buf);

      retval = xsend_msg (sfd_client, msg_buf, msg_size);
      if (retval < 0)
	{
	  SYSLOG_WARNING ("xsend_msg() error");
	  free (msg_buf);
	  close (sfd_client);
	  continue;
	}
    }

 end:
  if (msg_buf)
    free (msg_buf);

  if (ip_addr_str)
    free (ip_addr_str);
}

/*
 * This function is used for parsing IP address from a string.
 */
static int
parse_ip_addr (const char* str, uint32_t* address, uint16_t* port)
{
  union IP_ADDR
  {
    uint32_t uint32;
    struct {
      uint8_t b0 : 8;
      uint8_t b1 : 8;
      uint8_t b2 : 8;
      uint8_t b3 : 8;
    } byte;
  } ip_addr;

  uint8_t b0, b1, b2, b3;
  int retval;

  if ((str == NULL) || (address == NULL) || (port == NULL))
    return -1;

  retval = sscanf (str, "%hhu.%hhu.%hhu.%hhu:%hu", &b3, &b2, &b1, &b0, port);
  if (retval <= 0)
    return -1;

  ip_addr.byte.b3 = b3;
  ip_addr.byte.b2 = b2;
  ip_addr.byte.b1 = b1;
  ip_addr.byte.b0 = b0;

  *address = ip_addr.uint32;

  return 0;
}
