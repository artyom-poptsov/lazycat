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
 *   External functions
 * ==============================================================================
 */

extern open_inet_socket (const uint16_t port);
extern int connect_to_inet_socket (const uint32_t address, const uint16_t port);
extern int xsend_msg (const int sfd, const char* data, size_t data_sz);
extern int xrecv_msg (const int sfd, char** buf, size_t* buf_sz);

/*
 *   Static functions
 * ==============================================================================
 */

static int parse_ip_addr (const char* str,
			  uint32_t* address,
			  uint16_t* port);

/*
 * ==============================================================================
 */

/* Entry point of the proxy */
void
tcp_main_loop (void)
{
  static const char SYSLOG_MSG[] = "lazycat";
  
  static const char ERROR_UNABLE_TO_CONNECT[] = "ERROR: Unable to connect to the remote host";
  static const char ERROR_WRONG_IP_ADDRESS[]  = "ERROR: Wrong IP address";

  const char PROXY_NAME[] = "tcp-proxy";
  
  int sfd_proxy;
  int sfd_client;
  int sfd_remote;

  size_t msg_size;
  char*  msg_buf;

  size_t ip_addr_len;
  char*  ip_addr_str;

  uint32_t ip_addr;
  uint16_t ip_port;

  int retval;

  openlog (SYSLOG_MSG, LOG_CONS, LOG_DAEMON);
  syslog (LOG_INFO, "%s: Log is opened.", PROXY_NAME);
  
  sfd_proxy = open_socket (PROXY_NAME);

  enum { BACKLOG = 1 };
  
  if (listen (sfd_proxy, BACKLOG) < 0)
    {
      HANDLE_ERROR ("%s: An error occured during listen() call.",
		    PROXY_NAME);
    }

  sfd_client = accept (sfd_proxy, NULL, 0);
  if (sfd_client < 0)
    {
      syslog (LOG_WARNING, "%s: -*- accept() error", PROXY_NAME);
      return;
    }
  
  /* main loop */
  
  while (TRUE)
    {
      /*
       * Receive destination address
       * ---------------------------
       */

      syslog (LOG_DEBUG, "%s: Receive destination address", PROXY_NAME);

      retval = xrecv_msg (sfd_client, &ip_addr_str, &ip_addr_len);
      if (retval == 0)
	{
	  syslog (LOG_INFO, "%s: -i- xrecv_msg() server has performed "
		  "an ordered shutdown.", PROXY_NAME);
	  
	  close (sfd_client);
	  break;
	}
      else if (retval < 0)
	{
	  syslog (LOG_WARNING, "%s: -*- xrecv_msg() error", PROXY_NAME);
	  
	  free (ip_addr_str);
	  close (sfd_client);
	  break;
	}

      syslog(LOG_DEBUG, "%s: <-- address = %s", PROXY_NAME, ip_addr_str);

      /*
       * Receive message
       * ---------------
       */

      syslog(LOG_DEBUG, "%s: Receive message", PROXY_NAME);

      retval = xrecv_msg (sfd_client, &msg_buf, &msg_size);
      if (retval == 0)
	{
	  syslog (LOG_INFO, "%s: -i- xrecv_msg() server has performed "
		  "an ordered shutdown.", PROXY_NAME);
	  close (sfd_client);
	  break;
	}
      else if (retval < 0)
	{
	  syslog(LOG_WARNING, "%s: -*- xrecv_msg() error", PROXY_NAME);
	  free (msg_buf);
	  close (sfd_client);
	  break;
	}

      syslog (LOG_DEBUG, "%s: <-- msg = %s", PROXY_NAME, msg_buf);
      
      /*
       * Parse IP address
       * ----------------
       */

      retval = parse_ip_addr (ip_addr_str, &ip_addr, &ip_port);
      if (retval < 0)
	{
	  syslog(LOG_WARNING, "%s: -*- %s: %s",
		 PROXY_NAME, ERROR_WRONG_IP_ADDRESS, ip_addr_str);
	  
	  xsend_msg (sfd_client, ERROR_WRONG_IP_ADDRESS,
		     sizeof (ERROR_WRONG_IP_ADDRESS));
	  
	  free (ip_addr_str);
	  free (msg_buf);
	  close (sfd_client);
	  continue;
	}

      syslog (LOG_DEBUG, "%s: -i- IP Address = %d", PROXY_NAME, ip_addr);
      syslog (LOG_DEBUG, "%s: -i- IP port = %d", PROXY_NAME, ip_port);

      free (ip_addr_str);

      /*
       * Send message to the remote host
       * -------------------------------
       */
      
      sfd_remote = connect_to_inet_socket (ip_addr, ip_port);
      if (sfd_remote < 0)
	{
	  syslog (LOG_WARNING, ERROR_UNABLE_TO_CONNECT);
	  xsend_msg (sfd_client, ERROR_UNABLE_TO_CONNECT,
		     sizeof (ERROR_UNABLE_TO_CONNECT));
	  
	  free (msg_buf);
	  close (sfd_remote);
	  continue;
	}

      retval = xsend_msg (sfd_remote, msg_buf, msg_size);

      free (msg_buf);

      retval = xrecv_msg (sfd_remote, &msg_buf, &msg_size);

      close (sfd_remote);

      /*
       * Send response
       * -------------
       */

      syslog(LOG_DEBUG, "%s: Send response", PROXY_NAME);
      
      retval = xsend_msg (sfd_client, msg_buf, msg_size);
      if (retval < 0)
	{
	  syslog(LOG_WARNING, "%s: -*- xsend_msg() error", PROXY_NAME);
	  free (msg_buf);
	  close (sfd_client);
	  continue;
	}

      free (msg_buf);
    }

  close (sfd_client);
  closelog ();

  exit (EXIT_SUCCESS);
}

/*
 * This function is used for parsing IP addres from string.
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
    {
      return -1;
    }

  retval = sscanf (str, "%hhu.%hhu.%hhu.%hhu:%hu", &b3, &b2, &b1, &b0, port);
  if (retval <= 0)
    {
      return -1;
    }

  ip_addr.byte.b3 = b3;
  ip_addr.byte.b2 = b2;
  ip_addr.byte.b1 = b1;
  ip_addr.byte.b0 = b0;

  *address = ip_addr.uint32;
  
  return 0;
}
