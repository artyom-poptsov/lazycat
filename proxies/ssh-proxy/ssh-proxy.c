/* Proxy for communicating with remote hosts through SSH.
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

#include <stdint.h>
#include <libssh/libssh.h>
#include <syslog.h>
#include <stdlib.h>

#include "common.h"             /* SYSLOG_*() */
#include "lazy-ssh.h"           /* lazy_ssh_*() */

/*
 * Local variables
 */

static ssh_session session;
static ssh_channel channel;

static int32_t sfd_proxy;
static int32_t sfd_client;

/*
 * Local functions
 */

static void ssh_proxy_main_loop (void);

/*
 * The entry point of this proxy
 */
void
ssh_proxy_init (void)
{
  static const char SYSLOG_MSG[] = "lazycat [ssh-proxy]";
  static const char PROXY_NAME[] = "ssh-proxy";

  enum { BACKLOG = 1 };

  openlog (SYSLOG_MSG, LOG_CONS, LOG_DAEMON);

  sfd_proxy = open_socket (PROXY_NAME);
  if (sfd_proxy < 0)
    {
      SYSLOG_ERROR ("Unable to open socket.");
      closelog ();
      exit (EXIT_FAILURE);
    }

  if (listen (sfd_proxy, BACKLOG) < 0)
    {
      SYSLOG_ERROR ("An error occured during listen() call.");
      close (sfd_proxy);
      exit (EXIT_FAILURE);
    }

  sfd_client = accept (sfd_proxy, NULL, 0);
  if (sfd_client < 0)
    {
      SYSLOG_ERROR ("accept() error");
      close (sfd_proxy);
      closelog ();
      exit (EXIT_FAILURE);
    }

  ssh_proxy_main_loop ();

  close (sfd_client);
  close (sfd_proxy);

  closelog ();

  exit (EXIT_SUCCESS);
}

/*
 * Static functions
 */

/* The main loop of the proxy */
static void
ssh_proxy_main_loop (void)
{
  char   *ssh_addr;
  size_t ssh_addr_len;

  char   *cmd;
  size_t cmd_len;

  char   *output;
  size_t output_len;

  int32_t result;

  while (TRUE)
    {
      ssh_addr = NULL;
      cmd      = NULL;

      /*
       * Receive destination address in the following format:
       *   user@host:port
       */

      result = xrecv_msg (sfd_client, &ssh_addr, &ssh_addr_len);
      if (result == 0)
        {
          SYSLOG_INFO ("xrecv_msg(): server has performed "
                       "an ordered shutdown.");
          goto end;
        }
      else if (result < 0)
        {
          SYSLOG_WARNING ("xrecv_msg(): Unable to receive an address.");
          goto end;
        }

      SYSLOG_RECV ("Destination: %s", ssh_addr);

      /* Receive the message itself */

      result = xrecv_msg (sfd_client, &cmd, &cmd_len);
      if (result == 0)
        {
          SYSLOG_INFO ("xrecv_msg(): server has performed "
                       "an ordered shutdown.");
          goto end;
        }
      else if (result < 0)
        {
          SYSLOG_WARNING ("xrecv_msg(): Unable to receive a message.");
          goto end;
        }

      SYSLOG_RECV ("Command to execute: %s", cmd);

      session = lazy_ssh_login (ssh_addr);
      if (session == NULL)
        goto end;

      SYSLOG_DEBUG ("Send the command.");

      output_len = lazy_ssh_exec (session, cmd, &output);
      if (output_len <= 0)
        goto end;

      lazy_ssh_logout (session);

      result = xsend_msg (sfd_client, output, output_len);
      if (result < 0)
        {
          SYSLOG_WARNING ("xsend_msg() error");

          free (output);
          close (sfd_client);

          continue;
        }
    }

 end:
  if (ssh_addr != NULL)
    free (ssh_addr);

  if (cmd != NULL)
    free (cmd);
}
