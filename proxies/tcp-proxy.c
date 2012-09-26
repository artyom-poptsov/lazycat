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

static void tcp_proxy_main_loop (void);

static int32_t ip4_address_to_uint32 (const char *ip_addr_str,
                                      uint32_t *ip_addr_uint32);

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
  
  size_t message_size;
  char*  message;
  size_t response_size;
  char*  response;
  size_t address_len;
  char*  address;

  char*  host;

  uint32_t ip_addr;
  uint16_t ip_port;

  int sfd_remote;

  int32_t retval;
  
  while (TRUE)
    {
      message  = NULL;
      response = NULL;
      address  = NULL;
      host     = NULL;
      
      /*
       * Receive destination address
       */

      retval = xrecv_msg (sfd_client, &address, &address_len);
      if (retval == 0)
	{
	  SYSLOG_INFO ("Server has performed an ordered shutdown.");
	  goto end;
	}
      else if (retval < 0)
	{
	  SYSLOG_WARNING ("xrecv_msg(): Unable to receive the address.");
	  goto end;
	}

      SYSLOG_RECV ("[server] Address: %s", address);

      /*
       * Receive a message
       */

      retval = xrecv_msg (sfd_client, &message, &message_size);
      if (retval == 0)
	{
	  SYSLOG_INFO ("Server has performed an ordered shutdown.");
	  goto end;
	}
      else if (retval < 0)
	{
	  SYSLOG_WARNING ("xrecv_msg(): Unable to receive the message");
	  goto end;
	}

      SYSLOG_RECV ("[server] Message: %s", message);

      /*
       * Parse IP address
       */

      if ((get_hostname_from_address (address, &host) < 0)
          || (get_port_from_address (address, &ip_port)  < 0))
        {
          SYSLOG_WARNING ("%s: %s", ERR_WRONG_IP_ADDRESS, address);

          xsend_msg (sfd_client, ERR_WRONG_IP_ADDRESS,
		     sizeof (ERR_WRONG_IP_ADDRESS));

          goto next;
        }

      ip4_address_to_uint32 (host, &ip_addr);

      SYSLOG_DEBUG ("Host:       %s", host);
      SYSLOG_DEBUG ("IP Address: %d", ip_addr);
      SYSLOG_DEBUG ("IP port:    %d", ip_port);

      /*
       * Send the message to the remote host
       */

      sfd_remote = connect_to_inet_socket (ip_addr, ip_port);
      if (sfd_remote < 0)
	{
	  syslog (LOG_WARNING, ERR_UNABLE_TO_CONNECT);

	  xsend_msg (sfd_client, ERR_UNABLE_TO_CONNECT,
		     sizeof (ERR_UNABLE_TO_CONNECT));

	  close (sfd_remote);
          goto next;
	}

      retval = xsend_msg (sfd_remote, message, message_size);
      SYSLOG_SEND ("[host] Message: %s", message);
      
      retval = xrecv_msg (sfd_remote, &response, &response_size);
      SYSLOG_RECV ("[host] Response: %s", message);
      
      close (sfd_remote);

      /*
       * Send a response back to the server
       */

      SYSLOG_SEND ("[server] Response: %s", response);

      retval = xsend_msg (sfd_client, response, response_size);
      if (retval < 0)
	{
	  SYSLOG_WARNING ("xsend_msg() error");
          goto next;
	}

      free (response);

    next:
      free (address);
      free (message);
      free (host);
    }

 end:
  if (message != NULL)
    free (message);

  if (response != NULL)
    free (response);
  
  if (address != NULL)
    free (address);

  close (sfd_proxy);
  close (sfd_client);
  exit (EXIT_SUCCESS);
}

/*
 * This function is used for parsing IP address and port from a string in the
 * folowing format:
 *   127.0.0.1:50001
 */
static int32_t
ip4_address_to_uint32 (const char *ip_addr_str, uint32_t *ip_addr_uint32)
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

  if (ip_addr_str == NULL)
    return -1;

  if (sscanf (ip_addr_str, "%hhu.%hhu.%hhu.%hhu", &b3, &b2, &b1, &b0) <= 0)
    return -1;

  ip_addr.byte.b3 = b3;
  ip_addr.byte.b2 = b2;
  ip_addr.byte.b1 = b1;
  ip_addr.byte.b0 = b0;

  *ip_addr_uint32 = ip_addr.uint32;

  return 0;
}
