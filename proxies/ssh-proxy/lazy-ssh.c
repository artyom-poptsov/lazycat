/* Lazy functions for working with the SSH protocol.
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
#include <stdlib.h>
#include <string.h>
#include <libssh/libssh.h>

#include <syslog.h>

#include "lazy-ssh.h"
#include "common.h"             /* calcpy(), calncpy(), SYSLOG_*() */
#include "xsockets.h"           /* xrecv_msg(), xsend_msg() */
#include "parsers.h"            /* get_*_from_address() */

/*
 * Global variables and constants
 */

static const uint32_t VERBOSITY = 4;

/* 
 * Declarations of static functions
 */

static ssh_session lssh_connect_to_host (const char *user,
                                         const char *host, 
                                         uint16_t port,
                                         int verbosity);

static int32_t lssh_auth (ssh_session session);

/*
 * Public functions
 */

/* Create a new SSH session */
ssh_session
lazy_ssh_login (const char *address)
{
  char        *user;
  char        *host;
  uint16_t    port;
  ssh_session new_session;

  /* Parse the host address */
  SYSLOG_DEBUG ("Parse the host address");
  if (get_username_from_address (address, &user) <= 0)
    {
      SYSLOG_WARNING ("Wrong user name: %s", address);
      return NULL;
    }

  if (get_hostname_from_address (address, &host) <= 0)
    {
      SYSLOG_WARNING ("Wrong host name: %s", address);
      return NULL;
    }

  if (get_port_from_address (address, &port) <= 0)
    {
      SYSLOG_WARNING ("Wrong port: %s", address);
      return NULL;
    }

  /* Create a new SSH session */
  SYSLOG_DEBUG ("Create a new SSH session");
  new_session = lssh_connect_to_host (user, host, port, VERBOSITY);
  if (new_session == NULL)
    return NULL;

  if (lssh_auth (new_session) != SSH_AUTH_SUCCESS)
    return NULL;

  return new_session;
}

/* Close the SSH session */
void
lazy_ssh_logout (ssh_session session)
{
  ssh_disconnect (session);
  ssh_free (session);
}

/* Send the command cmd to a host and get output from it. */
int32_t
lazy_ssh_exec (ssh_session session, const char *cmd, char **output)
{
  char *buf;
  int32_t result;
  ssh_channel channel;

  channel = ssh_channel_new (session);
  if (channel == NULL)
    {
      SYSLOG_WARNING ("Unable to create a new SSH channel.");
      return SSH_ERROR;
    }
  
  result = ssh_channel_open_session(channel);
  if (result != SSH_OK)
    {
      SYSLOG_WARNING ("Unable to open a new SSH session.");
      goto out;
    }

  result = ssh_channel_request_exec (channel, cmd);
  if (result < 0)
    {
      SYSLOG_WARNING ("ssh_channel_request_exec() failed.");
      goto out;
    }

  SYSLOG_DEBUG ("Polling the SSH channel for data...");
  while (TRUE)
    {
      result = ssh_channel_poll (channel, 0);
      if (result > 0)
	{
	  /* Read data from the SSH channel */
          SYSLOG_DEBUG ("Data to read: %d byte(s)", result);
	  *output = (char*) calloc (result, sizeof (char));
	  result = ssh_channel_read (channel, *output, result, 0);
	  goto out;
	}
      else if (result == SSH_ERROR)
	{
	  SYSLOG_WARNING ("ssh_channel_poll() failed.");
	  goto out;
	}
    }

 out:
  ssh_channel_free (channel);

  return result;
}

/*
 * Static functions
 */

/* Do authentification for the session */
static int32_t
lssh_auth (ssh_session session)
{
  static const char PRIVATE_KEY_FILE[] = "/.ssh/lazycat";
  static const char PUBLIC_KEY_FILE[]  = "/.ssh/lazycat.pub";

  char *private_key_file;
  char *public_key_file;
  char *user_home_dir;

  ssh_string public_key;
  ssh_private_key private_key;
  int32_t result;

  /* Get pathes */

  user_home_dir = getenv ("HOME");
  SYSLOG_DEBUG ("Home: %s", user_home_dir);

  private_key_file = (char*) calloc (strlen (user_home_dir)
                                     + strlen (PRIVATE_KEY_FILE) + 1,
                                     sizeof (char));

  strcpy (private_key_file, user_home_dir);
  strcat (private_key_file, PRIVATE_KEY_FILE);

  /* Get keys */

  private_key = privatekey_from_file (session,
                                      private_key_file,
        			      0,     /* Type */
        			      NULL); /* Passphrase */

  free (private_key_file);

  if (private_key == NULL)
    {
      SYSLOG_WARNING ("Unable to get the private key.");
      return -1;
    }

  public_key_file = (char*) calloc (strlen (user_home_dir)
				    + strlen (PUBLIC_KEY_FILE) + 1,
				    sizeof (char));

  strcpy (public_key_file, user_home_dir);
  strcat (public_key_file, PUBLIC_KEY_FILE);

  public_key = publickey_from_file (session, public_key_file, NULL);

  free (public_key_file);

  if (public_key == NULL)
    {
      SYSLOG_WARNING ("Unable to get the public key.");
      return -1;
    }

  /* Auth */

  result = ssh_userauth_pubkey (session, NULL, public_key, private_key);
  switch (result)
    {
    case SSH_AUTH_SUCCESS:
      return result;

    case SSH_AUTH_DENIED:
    case SSH_AUTH_PARTIAL:
      SYSLOG_ERROR ("Authentification failed: %s", ssh_get_error (session));
      break;

    default:
      SYSLOG_ERROR ("Error occured during authentificating: %s", 
                    ssh_get_error (session));
    }

  return result;
}

/* Connect to a SSH server */
static ssh_session
lssh_connect_to_host (const char *user,
                      const char *host,
                      uint16_t   port,
                      int        verbosity)
{
  ssh_session session = NULL;

  SYSLOG_DEBUG ("User: %s", user);
  SYSLOG_DEBUG ("Host: %s", host);

  if ((host == NULL) || (user == NULL))
    {
      SYSLOG_WARNING ("%s: Wrong parameters have been passed to function.",
                      __func__);
      goto out;
    }

  session = ssh_new ();
  if (session == NULL)
    {
      SYSLOG_WARNING ("%s: Unable to create a new SSH session.",
                      __func__);
      goto out;
    }

  if (ssh_options_set (session, SSH_OPTIONS_USER, user) < 0)
    goto option_set_error;

  if (ssh_options_set (session, SSH_OPTIONS_HOST, host) < 0)
    goto option_set_error;

  if (ssh_options_set (session, SSH_OPTIONS_PORT, &port) < 0)
    goto option_set_error;

  if (ssh_options_set (session, SSH_OPTIONS_LOG_VERBOSITY, &verbosity) < 0)
    goto option_set_error;

  if (ssh_connect (session) == 0)
    {
      /* Connected successfully. Return the session. */
      goto out;
    }
  else
    {
      SYSLOG_ERROR ("%s: Connection failed: %s", 
                    __func__, ssh_get_error (session));
      ssh_disconnect (session);
    }

 option_set_error:
  ssh_free (session);
  session = NULL;

 out:
  return session;
}
